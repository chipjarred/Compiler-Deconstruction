// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// ---------------------------------------------------
public struct RISCCodeGenerator
{	
	private typealias OpCode = RISCOpCode
	public static let wordSize = MemoryLayout<UInt32>.stride

	internal static let maxCode = 1000
	internal static let maxRel = 200
	internal static let NofCom = 16

	/*reserved registers*/
	internal let FP: Int = 12
	internal let SP: Int = 13
	internal let LNK: Int = 14
	internal let PC: Int = 15

	public static let Head = 0
		
	public static let boolType = TypeInfo.boolean
	public static let intType = TypeInfo.integer
	public var curlev: Int = 0
	public var pc: Int = 0
	internal var cno: Int { return comname.count }
	public internal(set) var entry: Int = 0
	internal var fixlist: Int = 0

	/* used registers */
	internal var regs = Set<Int>()

	internal var code = [UInt32](repeating: 0, count: maxCode)

	// ---------------------------------------------------
	// Function to get object code so it can be saved by driver program
	public func getObjectCode() -> [UInt32]
	{
		var objectCode = [UInt32](capacity: pc)
		for i in 0..<pc {
			objectCode.append(code[i])
		}
		return objectCode
	}

	// commands
	internal var comname = [String](capacity: NofCom)
	internal var comadr = [Int](capacity: NofCom)

	// ---------------------------------------------------
	private mutating func getRegister() -> Int
	{
		var i: Int
		
		i = 0
		while (i < FP) && (regs.contains(i)) {
			i += 1
		}
		regs.insert(i)
		return i
	}

	// ---------------------------------------------------
	/**
	Determine the instruction format for`RISCOpCode`.
	
	- Parameter opCode: The RISC opcode for which the instruction format is to be determined
	
	- Returns: The instruction format number, 0-3, for a given opCode.
	*/
	private func instructionFormat(for opCode: RISCOpCode) -> UInt32
	{
		if opCode < .MOVI { return 0 }
		if opCode < .LDW { return 1 }
		if opCode < .BEQ { return 2 }
		return 3
	}
	
	// ---------------------------------------------------
	/**
	Encode an instruction from its opcode and operands.
	
	- Parameters:
		- opCode: the opcode for the instruction
		- a: The register number for the `a` operand for the instruction, ignored for format 3 instructions
		- b: The register number for the `b` operand for the instruction, ignored for format 3 instructions
		- c: Operand `c` for the instruction, interpreted differently for each instruction format:
			- format 0: `c` contains the register number for the `c` operand
			- format 1: `c` contains the immediate value for the instruction
			- format 2: `c` contains the address for register-indirect addressing
			- format 3: `c` contains the PC-relative address for the destination of a branch instruction
	
	- Returns: a `UInt32` containing the encoded instruction
	*/
	internal func encodeInstruction(
		_ opCode: RISCOpCode,
		a: Int,
		b: Int,
		c: Int) -> UInt32
	{
		let format = instructionFormat(for: opCode)
		var instruction: UInt32 = format << 4
		
		instruction |= opCode.code & 0xf
		
		if format < 3
		{	// formats 0, 1, and 2
			instruction <<= 4 // make room for a
			instruction |= UInt32(a) & 0xf
			instruction <<= 4 // make room for b
			instruction |= UInt32(b) & 0xf
			instruction <<= 18 // make room for c
			let mask: UInt32 = format == 0 ? 0xf : 0x3ffff
			instruction |= UInt32(bitPattern: Int32(c)) & mask
		}
		else {
			// format 3
			instruction <<= 26 // make room for c (displacement)
			instruction |= UInt32(bitPattern: Int32(c)) & 0x03ffffff
		}
		
		return instruction
	}

	// ---------------------------------------------------
	/**
	Encode and store in the code the instruction specified by the opcode, `op`, and operands, `a`, `b`
	and `c`.
	
	- Parameters:
		- opCode: the opcode for the instruction
		- a: The register number for the `a` operand for the instruction, ignored for format 3 instructions
		- b: The register number for the `b` operand for the instruction, ignored for format 3 instructions
		- c: Operand `c` for the instruction, interpreted differently for each instruction format:
			- format 0: `c` contains the register number for the `c` operand
			- format 1: `c` contains the immediate value for the instruction
			- format 2: `c` contains the address for register-indirect addressing
			- format 3: `c` contains the PC-relative address for the destination of a branch instruction
	*/
	internal mutating func put(_ op: RISCOpCode, _ a: Int, _ b: Int, _ c: Int)
	{
		code[pc] = encodeInstruction(op, a: a, b: b, c: c)
		pc += 1
	}

	// ---------------------------------------------------
	/**
	Encode and store in the code a branch instruction from the opcode, `op`, and PC-relative address,
	`disp`
	
	- Parameters:
		- opCode: the opcode for the instruction
		- disp: PC-relative address of the destination for the branch
	*/
	private mutating func putBR(_ op: RISCOpCode, _ disp: Int)
	{
		assert((RISCOpCode.BEQ...RISCOpCode.RET).contains(op))
		put(op, 0, 0, disp) // emit branch instruction
	}

	// ---------------------------------------------------
	private func testRange(_ x: Int) throws
	{
		// 18-bit entity
		if (x >= 0x20000) || (x < -0x20000) {
			throw CodeGenError.valueTooLarge(value: x, range: 0x1ffff...0x20000)
		}
	}

	// ---------------------------------------------------
	/**
	Associate a register with an `RISCOperand`, and emit instructions to load its integer value into that
	register.
	
	- Parameter x: The `RISCOperand` representing an source language  r-value to be loaded.
	
	- Returns: a `RISCOperand` representing the loaded r-value.
	*/
	internal mutating func load(_ x: RISCOperand) throws -> RISCOperand
	{
		var r: Int = 0
		
		var result = x
		// x.mode # Reg
		if x.mode == .variable
		{
			if x.lev == 0 {
				result.a = result.a - pc * 4
			}
			r = getRegister()
			put(.LDW, r, result.r, result.a)
			regs.remove(result.r)
			result.r = r
		}
		else if x.mode == .constant
		{
			try testRange(result.a)
			result.r = getRegister()
			put(.MOVI, result.r, 0, result.a)
		}
		result.mode = .register
		
		return result
	}

	// ---------------------------------------------------
	/**
	Associate a register with an `RISCOperand` and load a boolean value into it.
	
	- Parameter x: The `RISCOperand` representing a source language r-value to be loaded
	
	- Returns: a `RISCOperand` representing the loaded r-value.
	*/
	private mutating func loadBool(_ x: RISCOperand) throws -> RISCOperand
	{
		if x.type?.form != .boolean {
			throw CodeGenError.wrongForm(expected: .boolean, got: x.type?.form)
		}
		var result = try load(x)
		result.mode = .condition
		result.a = 0
		result.b = 0
		result.c = 1
		
		return result
	}

	// ---------------------------------------------------
	/**
	Emit instructions to perform the operation: `x = opCode(x, y)`.
	
	- Parameters:
		- opCode: The opcode representing the operation to be performed.
		- x: The `RISCOperand` to serve as both the l-value for the assignment, and the first r-value
			operand.
		- y: The `RISCOperand` to serve as second r-value operand.
	*/
	private mutating func putOp(
		_ opCode: RISCOpCode,
		_ x: inout RISCOperand,
		_ y: inout RISCOperand) throws
	{
		if x.mode != .register {
			x = try load(x)
		}
		
		if y.mode == .constant {
			try testRange(y.a)
			put(opCode + 16, x.r, x.r, y.a)
		}
		else
		{
			if y.mode != .register {
				y = try load(y)
			}
			put(opCode, x.r, x.r, y.r)
			regs.remove(y.r)
		}
	}

	// ---------------------------------------------------
	/**
	Perform a boolean negation.
	
	- Parameter cond: The value to be negated.
	
	- Note: This implementation just toggles the least significant bit, so to be a true `NOT` operation, it
	relies on the other bits being 0, or at least all uses of the value masking them out.  This is different than
	some languages where `0` evalutes to `false`, and non-zero evaluates to `true`.
	*/
	private func negated(_ cond: Int) -> Int {
		return (cond % 2 == 1) ? cond - 1 : cond + 1
	}

	// ---------------------------------------------------
	private mutating func merged(_ L0: Int, _ L1: Int) -> Int
	{
		if L0 != 0
		{
			var L2 = L0
			var L3: Int = 0
			while true
			{
				L3 = Int(code[L2] % 0x40000)
				if L3 == 0 { break }
				L2 = L3
			}
			code[L2] = UInt32(Int(code[L2]) &- L3 &+ L1)
			return L0
		}
		else {
			return L1
		}
	}

	// ---------------------------------------------------
	private mutating func fix(_ at: Int, _ with: Int)
	{
		code[at] =
			(code[at] / 0x400000) &* 0x400000 &+ UInt32((with % 0x400000))
	}

	// ---------------------------------------------------
	private mutating func fixWith(_ L0: Int, _ L1: Int)
	{
		var L2: Int
		var L0 = L0
		
		while L0 != 0
		{
			L2 = Int(code[L0] % 0x40000)
			fix(L0, L1 - L0)
			L0 = L2
		}
	}

	/*-----------------------------------------------*/
	public mutating func fixLink(_ L: Int)
	{
		var L1: Int
		var L = L
		
		while L != 0
		{
			L1 = Int(code[L] % 0x40000)
			fix(L, pc - L)
			L = L1
		}
	}

	// ---------------------------------------------------
	public mutating func IncLevel(_ n: Int) {
		curlev += n
	}

	// ---------------------------------------------------
	/**
	Make a `RISCOperand` that represents a constant value.
	
	- Parameters:
		- type: The `TypeInfo` of the constant to be constructed
		- value: The integer value associated with the constant (for integer types, it's actual value).
	
	- Returns: the `RISCOperand` represning the constant value
	*/
	public func makeConstItem(_ type: TypeInfo?, _ value: Int) -> RISCOperand
	{
		var item = RISCOperand()
		item.mode = .constant
		item.type = type
		item.a = value
		
		return item
	}
	
	// ---------------------------------------------------
	/**
	A default `SymbolInfo` to use to create a default `RISCCodeGenerator.RISCOperand` to use when
	creating the proper type throws an error, so that the parser can continue, hopefully producing additional
	useful errors rather than simply abortong on the first one.
	*/
	fileprivate static var defaultSymbol = SymbolInfo(
		kind: .variable,
		type: RISCCodeGenerator.intType,
		value: 0
	)
	
	// ---------------------------------------------------
	/**
	Make a default `RISCOperand` to use as a placeholder so the compiler can continue
	processing after code generation has produced an error.  Creating default `RISCOperand` should never
	throw an error, and if it does, we simply abort with a message, because at that point we cannot recover
	and do anything useful.
	*/
	public mutating func makeDefaultOperand() -> RISCOperand
	{
		do { return try makeOperand(RISCCodeGenerator.defaultSymbol) }
		catch
		{
			fatalError(
				"Unexpected error, \(error.localizedDescription), while trying"
				+ " to make default RISCCodeGenerator item."
			)
		}
	}

	// ---------------------------------------------------
	/**
	Construct a `RISCOperand` from a `SymbolInfo`
	
	- Parameter y: `SymbolInfo` for the `RISCOperand` to be constructed
	
	- Returns: A `RISCOperand` representing the source level symbol described in `SymbolInfo`
	*/
	public mutating func makeOperand(
		_ symbolInfo: SymbolInfo) throws -> RISCOperand
	{
		var r: Int = 0
		
		var item = RISCOperand()
		item.mode = symbolInfo.kind
		item.type = symbolInfo.type
		item.lev = symbolInfo.level
		item.a = symbolInfo.value
		item.b = 0
		if symbolInfo.level == 0 {
			item.r = PC
		}
		else if symbolInfo.level == curlev {
			item.r = FP
		}
		else
		{
			item.r = 0
			throw CodeGenError.localOrGlobalOnly
		}
		if symbolInfo.kind == .parameter
		{
			r = getRegister()
			put(.LDW, r, item.r, item.a)
			item.mode = .variable
			item.r = r
			item.a = 0
		}
		
		return item
	}

	// ---------------------------------------------------
	/**
	Emits instructions for a unary operation and assignment: `x = op(x)`
	
	- Parameters:
		- op: `TokenType` describing the unary operation to be performed.
		- x: a `RISCOperand` to serve as both the r-value operand for `op` and also the l-value for
			the assignment.
	*/
	public mutating func emitUnaryExpression(
		_ op: TokenType,
		_ x: inout RISCOperand) throws
	{
		var t: Int

		if op == .plus || op == .unaryPlus {
			return
		}
		else if op == .minus || op == .unaryMinus
		{
			if x.type!.form != .integer
			{
				throw CodeGenError.wrongForm(
					expected: .integer,
					got: x.type!.form
				)
			}
			else if x.mode == .constant {
				x.a = -x.a
			}
			else
			{
				if x.mode == .variable {
					x = try load(x)
				}
				put(.MVN, x.r, 0, x.r)
			}
		}
		else if op == .not
		{
			if x.mode != .condition {
				x = try loadBool(x)
			}
			x.c = negated(x.c)
			t = x.a
			x.a = x.b
			x.b = t
		}
	}
	
	// ---------------------------------------------------
	/**
	Emits instructions for short circuiting of logical AND and OR expressions based on the first operand.
	
	Short-cicuiting just means that if the left operand determines the value of the operation by itself,
	evaluation of the right operand is skipped.
	
	For `x & y`, if `x` is `false` there then the whole expression is `false`, regardless of the value
	of `y`, so `y` is not evaluated.
	
	For `x OR y`, if `x` is `true` then the whole expression is `true`, regardless of the value of `y`,
	so `y` is not evaluated.
	
	This has implications in the case where `y` may have side-effects, such as if it were call to a function
	procedure (which Oberon-0 doesn't currently support).
	
	- Parameters:
		- op: `TokenType` describing the logical operation to be performed (must be `.and` or
			`.or`)
		- x: a `RISCOperand` to serve as the left operand for the AND or OR.
	*/
	public mutating func emitLogicShortCircuit(
		for op: TokenType,
		operand x: inout RISCOperand) throws
	{
		assert(op == .and || op == .or)
		
		if op == .and
		{
			if x.mode != .condition {
				x = try loadBool(x)
			}
			putBR(.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			x.a = pc - 1
			fixLink(x.b)
			x.b = 0
		}
		else if op == .or
		{
			if x.mode != .condition {
				x = try loadBool(x)
			}
			putBR(.BEQ + x.c, x.b)
			regs.remove(x.r)
			x.b = pc - 1
			fixLink(x.a);
			x.a = 0
		}
	}

	// ---------------------------------------------------
	/**
	Emits instructions for a binary operation and assignment: `x = op(x, y)`
	
	- Parameters:
		- op: `TokenType` describing the binary operation to be performed.
		- x: a `RISCOperand` to serve as both the first r-value operand for `op` and also the l-value
			for the assignment.
		- y: a `RISCOperand` to serve as the second r-value operand for `op`
	*/
	public mutating func emitBinaryExpression(
		_ op: TokenType,
		_ x: inout RISCOperand,
		_ y: inout RISCOperand) throws
	{
		if (x.type!.form == .integer) && (y.type!.form == .integer)
		{
			if (x.mode == .constant) && (y.mode == .constant)
			{
				// overflow checks missing
				switch op
				{
					case .plus: x.a += y.a
					case .minus: x.a -= y.a
					case .times: x.a *= y.a
					case .div: x.a /= y.a
					case .mod: x.a %= y.a
					default:
						throw CodeGenError.expectedArithmeticBinaryOperator(
							got: op
						)
				}
			}
			else
			{
				switch op
				{
					case .plus: try putOp(.ADD, &x, &y)
					case .minus: try putOp(.SUB, &x, &y)
					case .times: try putOp(.MUL, &x, &y)
					case .div: try putOp(.Div, &x, &y)
					case .mod: try putOp(.Mod, &x, &y)
					default:
						throw CodeGenError.expectedArithmeticBinaryOperator(
							got: op
						)
				}
			}
		}
		else if (x.type!.form == .boolean) && (y.type!.form == .boolean)
		{
			if y.mode != .condition {
				y = try loadBool(y)
			}
			
			if op == .or
			{
				x.a = y.a
				x.b = merged(y.b, x.b)
				x.c = y.c
			}
			else if op == .and
			{
				x.a = merged(y.a, x.a)
				x.b = y.b
				x.c = y.c
			}
			else
			{
				throw CodeGenError.expectedLogicalBinaryOperator(got: op)
			}
		}
		else {
			throw CodeGenError.incompatibleTypes(x.type!.form, y.type!.form)
		}
	}

	// ---------------------------------------------------
	/**
	Emits instructions for a comparison operation and assignment: `x = op(x, y)`
	
	- Parameters:
		- op: `TokenType` describing the comparison operation to be performed.
		- x: a `RISCOperand` to serve as both the first r-value operand for `op` and also the l-value
			for the assignment.
		- y: a `RISCOperand` to serve as the second r-value operand for `op`
	*/
	public mutating func emitComparison(
		_ op: TokenType,
		_ x: inout RISCOperand,
		_ y: inout RISCOperand) throws
	{
		if (x.type!.form != .integer) || (y.type!.form != .integer)
		{
			throw CodeGenError.wrongForm(
				expected: .integer,
				got: x.type!.form == .integer ? y.type!.form : x.type!.form
			)
		}
		else
		{
			try putOp(.CMP, &x, &y)
			x.c = Int(op.rawValue - TokenType.isEqualTo.rawValue)
			regs.remove(y.r)
		}
		x.mode = .condition
		x.type = RISCCodeGenerator.boolType
		x.a = 0
		x.b = 0
	}

	// ---------------------------------------------------
	/**
	Emits instructions for an  assignment: `x = y`
	
	- Parameters:
		- x: a `RISCOperand` to serve as the l-value for the assignment.
		- y: a `RISCOperand` to serve as the r-value for the assignment
	*/
	public mutating func emitAssignment(
		into x: inout RISCOperand,
		from y: inout RISCOperand) throws
	{
		if x.type != nil, y.type != nil,
			[.boolean, .integer].contains(x.type!.form)
			&& (x.type!.form == y.type!.form)
		{
			if y.mode == .condition
			{
				put(.BEQ + negated(y.c), y.r, 0, y.a)
				regs.remove(y.r)
				y.a = pc - 1
				fixLink(y.b)
				y.r = getRegister()
				put(.MOVI, y.r, 0, 1)
				putBR(.BR, 2)
				fixLink(y.a)
				put(.MOVI, y.r, 0, 0)
			}
			else if y.mode != .register {
				y = try load(y)
			}
			
			if x.mode == .variable
			{
				if x.lev == 0 {
					x.a = x.a - pc * 4
				}
				put(.STW, y.r, x.r, x.a)
			}
			else { throw CodeGenError.illegalAssignment }
			regs.remove(x.r)
			regs.remove(y.r)
		}
		else
		{
			throw CodeGenError.incompatibleAssignment(
				src: y.type!.form,
				dst: x.type!.form
			)
		}
	}

	// ---------------------------------------------------
	public mutating func parameter(
		_ x: inout RISCOperand,
		_ symbolInfo: SymbolInfo) throws
	{
		var r: Int = 0
		
		if x.type == symbolInfo.type
		{
			if symbolInfo.kind == .parameter
			{
				// Var param
				if x.mode == .variable
				{
					if x.a != 0
					{
						r = getRegister()
						put(.ADDI, r, x.r, x.a)
					}
					else {
						r = x.r
					}
				}
				else { throw CodeGenError.illegalParameterMode(x.mode) }
				put(.PSH, r, SP, 4)
				regs.remove(r)
			}
			else
			{
				// value param
				if x.mode != .register {
					x = try load(x)
				}
				put(.PSH, x.r, SP, 4)
				regs.remove(x.r)
			}
		}
		else { throw CodeGenError.incompatbleActualParameter }
	}

	// ---------------------------------------------------
	public mutating func conditionalJump(_ x: inout RISCOperand) throws
	{
		if x.type!.form == .boolean
		{
			if x.mode != .condition {
				x = try loadBool(x)
			}
			putBR(.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			fixLink(x.b)
			x.a = pc - 1
		}
		else
		{
			x.a = pc
			throw CodeGenError.wrongForm(expected: .boolean, got: x.type!.form)
		}
	}

	// ---------------------------------------------------
	public mutating func jumpBack(_ L: Int) {
		putBR(.BR, L - pc)
	}

	// ---------------------------------------------------
	public mutating func jumpForward(_ L: inout Int)
	{
		putBR(.BR, L)
		L = pc - 1
	}

	// ---------------------------------------------------
	public mutating func call(procedure: inout RISCOperand) {
		putBR(.BSR, procedure.a - pc)
	}

	// ---------------------------------------------------
	public mutating func call(
		standardProcedure procedure: inout RISCOperand,
		with parameter: inout RISCOperand) throws
	{
		if procedure.a < 4
		{
			if parameter.type!.form != .integer
			{
				throw CodeGenError.wrongForm(
					expected: .integer,
					got: parameter.type!.form
				)
			}
		}
		
		if procedure.a == 1
		{
			var z = RISCOperand()
			
			z.r = getRegister()
			z.mode = .register
			z.type = RISCCodeGenerator.intType
			put(.RD, z.r, 0, 0)
			try emitAssignment(into: &parameter, from: &z)
		}
		else if procedure.a == 2
		{
			parameter = try load(parameter)
			put(.WRD, 0, 0, parameter.r)
			regs.remove(parameter.r)
		}
		else if procedure.a == 3
		{
			parameter = try load(parameter)
			put(.WRH, 0, 0, parameter.r)
			regs.remove(parameter.r)
		}
		else {
			put(.WRL, 0, 0, 0)
		}
	}

	// ---------------------------------------------------
	public mutating func header(_ size: Int)
	{
		entry = pc
		put(.MOVI, SP, 0, RISCEmulator.MemSize - size)
		put(.PSH, LNK, SP, 4) // init SP
	}

	// ---------------------------------------------------
	public mutating func enter(_ size: Int)
	{
		put(.PSH, LNK, SP, 4)
		put(.PSH, FP, SP, 4)
		put(.MOV, FP, 0, SP)
		put(.SUBI, SP, SP, size)
	}

	// ---------------------------------------------------
	public mutating func procedureReturn(_ size: Int)
	{
		put(.MOV, SP, 0, FP)
		put(.POP, FP, SP, 4)
		put(.POP, LNK, SP, size + 4)
		putBR(.RET, LNK)
	}

	// ---------------------------------------------------
	public mutating func open()
	{
		curlev = 0
		pc = 0
		regs = []
	}

	// ---------------------------------------------------
	public mutating func close()
	{
		put(.POP, LNK, SP, 4)
		putBR(.RET, LNK)
	}

	// ---------------------------------------------------
	public mutating func enterCmd(_ name: String)
	{
		comname.append(name)
		comadr.append(pc * 4)
	}

	// ---------------------------------------------------
	public mutating func decode<OutStream: TextOutputStream>(
		to outStream: inout OutStream)
	{
		var outStr = "entry\(entry * 4, pad: 6)\n"
		
		for i in 0..<pc
		{
			let w = code[i]
			let op = RISCOpCode(w / 0x4000000 % 0x40)
			outStr += "\(4 * i, pad: 4)\t\(op)"
			
			var a: Int
			if op < .BEQ
			{
				a = Int(w % 0x40000)
				if a >= 0x20000 {
					a -= 0x40000 /*sign extension*/
				}
				outStr += "\t\(Int(w / 0x400000 % 0x10), pad: 4),"
				outStr += "\(Int(w / 0x40000 % 0x10), pad: 4),"
			}
			else
			{
				a = Int(w % 0x4000000)
				if a >= 0x2000000 {
					a -= 0x4000000 /*sign extension*/
				}
			}
			outStr += "\(a, pad: 6)\n"
		}
		print(outStr, to: &outStream)
	}
}

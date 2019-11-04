// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
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

	internal static let maxCode = 1000
	internal static let maxRel = 200
	internal static let NofCom = 16

	/*reserved registers*/
	internal let FP: Int = 12
	internal let SP: Int = 13
	internal let LNK: Int = 14
	internal let PC: Int = 15

	public static let Head = 0
		
	public static var boolType = TypeInfo(form: .boolean, size: 4)
	public static var intType = TypeInfo(form: .integer, size: 4)
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
	private mutating func getReg() -> Int
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
	return the format number, 0-3, for a given opCode.
	- Note: This function is NOT part of the original code
	*/
	private func instructionFormat(for opCode: RISCOpCode) -> UInt32
	{
		if opCode < .MOVI { return 0 }
		if opCode < .LDW { return 1 }
		if opCode < .BEQ { return 2 }
		return 3
	}

	// ---------------------------------------------------
	internal mutating func put(_ op: RISCOpCode, _ a: Int, _ b: Int, _ c: Int)
	{
		// format 2 instruction
		// first 2 bits are the format specifier = 0b10
		let format = instructionFormat(for: op)
		var instruction: UInt32 = format << 4
		
		instruction |= op.code & 0xf
		
		if format < 3
		{
			// formats 0, 1, and 2
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
		code[pc] = instruction
		pc += 1
	}

	// ---------------------------------------------------
	private mutating func putBR(_ op: RISCOpCode, _ disp: Int) {
		put(op, 0, 0, disp) /* emit branch instruction */
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
			r = getReg()
			put(.LDW, r, result.r, result.a)
			regs.remove(result.r)
			result.r = r
		}
		else if x.mode == .constant
		{
			try testRange(result.a)
			result.r = getReg()
			put(.MOVI, result.r, 0, result.a)
		}
		result.mode = .register
		
		return result
	}

	// ---------------------------------------------------
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
	private mutating func putOp(
		_ cd: RISCOpCode,
		_ x: inout RISCOperand,
		_ y: inout RISCOperand) throws
	{
		if x.mode != .register {
			x = try load(x)
		}
		if y.mode == .constant {
			try testRange(y.a)
			put(cd + 16, x.r, x.r, y.a)
		}
		else
		{
			if y.mode != .register {
				y = try load(y)
			}
			put(cd, x.r, x.r, y.r)
			regs.remove(y.r)
		}
	}

	// ---------------------------------------------------
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

	/*-----------------------------------------------*/

	public mutating func IncLevel(_ n: Int) {
		curlev += n
	}

	// ---------------------------------------------------
	public func makeConstItem(_ typ: TypeInfo?, _ val: Int) -> RISCOperand
	{
		var item = RISCOperand()
		item.mode = .constant
		item.type = typ
		item.a = val
		
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
	public mutating func makeOperand(_ y: SymbolInfo) throws -> RISCOperand
	{
		var r: Int = 0
		
		var item = RISCOperand()
		item.mode = y.kind
		item.type = y.type
		item.lev = y.level
		item.a = y.value
		item.b = 0
		if y.level == 0 {
			item.r = PC
		}
		else if y.level == curlev {
			item.r = FP
		}
		else
		{
			item.r = 0
			throw CodeGenError.localOrGlobalOnly
		}
		if y.kind == .parameter
		{
			r = getReg()
			put(.LDW, r, item.r, item.a)
			item.mode = .variable
			item.r = r
			item.a = 0
		}
		
		return item
	}

	// ---------------------------------------------------
	// x := op x
	public mutating func Op1(_ op: TokenType, _ x: inout RISCOperand) throws
	{
		var t: Int

		if op == .minus
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
		else if op == .and
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
	// x := x op y
	public mutating func Op2(
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
	// x := x ? y
	public mutating func relation(
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
	// x := y
	public mutating func store(
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
				y.r = getReg()
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
						r = getReg()
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
	public mutating func call(_ x: inout RISCOperand) {
		putBR(.BSR, x.a - pc)
	}

	// ---------------------------------------------------
	public mutating func ioCall(
		_ x: inout RISCOperand,
		_ y: inout RISCOperand) throws
	{
		var z = RISCOperand()
		
		if x.a < 4
		{
			if y.type!.form != .integer
			{
				throw CodeGenError.wrongForm(
					expected: .integer,
					got: y.type!.form
				)
			}
		}
		
		if x.a == 1
		{
			z.r = getReg()
			z.mode = .register
			z.type = RISCCodeGenerator.intType
			put(.RD, z.r, 0, 0)
			try store(into: &y, from: &z)
		}
		else if x.a == 2
		{
			y = try load(y)
			put(.WRD, 0, 0, y.r)
			regs.remove(y.r)
		}
		else if x.a == 3
		{
			y = try load(y)
			put(.WRH, 0, 0, y.r)
			regs.remove(y.r)
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

//
//  RISCCodeGenerator.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

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

	// ---------------------------------------------------
	public struct Item
	{
		public typealias Mode = SymbolInfo.Kind
		
		public var mode: Mode = .head
		public var lev: Int = 0
		public var type: TypeInfo? = nil
		public var a: Int = 0
		internal var b: Int = 0
		internal var c: Int = 0
		internal var r: Int = 0
		
		// ---------------------------------------------------
		public init() { }
		
		// ---------------------------------------------------
		// x.a
		public mutating func setFieldInfo(from symbolInfo: SymbolInfo)
		{
			a += symbolInfo.value
			type = symbolInfo.type
		}
		
		// ---------------------------------------------------
		// x := x[index]
		public mutating func index(
			at index: Item,
			for codegenerator: inout RISCCodeGenerator)
		{
			if index.type != intType {
				Lexer.mark("index not integer")
			}
			if index.mode == .constant
			{
				if (index.a < 0) || (index.a >= type!.len) {
					Lexer.mark("bad index")
				}
				self.a += index.a * Int(type!.base!.size)
			}
			else
			{
				var y = index
				if y.mode != .register {
					y = codegenerator.load(y)
				}
				codegenerator.put(.CHKI, y.r, 0, type!.len)
				codegenerator.put(.MULI, y.r, y.r, type!.base!.size)
				codegenerator.put(.ADD, y.r, r, y.r)
				codegenerator.regs.remove(r)
				r = y.r
			}
			type = type!.base
		}
	}
		
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
	private mutating func put(_ op: RISCOpCode, _ a: Int, _ b: Int, _ c: Int)
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
			instruction |= UInt32(bitPattern: Int32(c)) & (format == 0 ? 0xf : 0x3ffff)
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
	private func testRange(_ x: Int)
	{
		// 18-bit entity
		if (x >= 0x20000) || (x < -0x20000) {
			Lexer.mark("value too large")
		}
	}

	// ---------------------------------------------------
	private mutating func load(_ x: Item) -> Item
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
			testRange(result.a)
			result.r = getReg()
			put(.MOVI, result.r, 0, result.a)
		}
		result.mode = .register
		
		return result
	}

	// ---------------------------------------------------
	private mutating func loadBool(_ x: Item) -> Item
	{
		if x.type?.form != .boolean {
			Lexer.mark("Boolean?")
		}
		var result = load(x)
		result.mode = .condition
		result.a = 0
		result.b = 0
		result.c = 1
		
		return result
	}

	// ---------------------------------------------------
	private mutating func putOp(
		_ cd: RISCOpCode,
		_ x: inout Item,
		_ y: inout Item)
	{
		if x.mode != .register {
			x = load(x)
		}
		if y.mode == .constant {
			testRange(y.a)
			put(cd + 16, x.r, x.r, y.a)
		}
		else
		{
			if y.mode != .register {
				y = load(y)
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
	public func makeConstItem(_ typ: TypeInfo?, _ val: Int) -> Item
	{
		var item = Item()
		item.mode = .constant
		item.type = typ
		item.a = val
		
		return item
	}
	
	fileprivate static var defaultSymbol = SymbolInfo(
		kind: .variable,
		type: RISCCodeGenerator.intType,
		value: 0
	)
	// ---------------------------------------------------
	public mutating func makeDefaultItem() -> Item {
		return makeItem(RISCCodeGenerator.defaultSymbol)
	}

	// ---------------------------------------------------
	public mutating func makeItem(_ y: SymbolInfo) -> Item
	{
		var r: Int = 0
		
		var item = Item()
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
			Lexer.mark("level!")
			item.r = 0
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
	public mutating func Op1(_ op: Symbol, _ x: inout Item)
	{
		var t: Int

		if op == .minus
		{
			if x.type!.form != .integer {
				Lexer.mark("bad type")
			}
			else if x.mode == .constant {
				x.a = -x.a
			}
			else
			{
				if x.mode == .variable {
					x = load(x)
				}
				put(.MVN, x.r, 0, x.r)
			}
		}
		else if op == .not
		{
			if x.mode != .condition {
				x = loadBool(x)
			}
			x.c = negated(x.c)
			t = x.a
			x.a = x.b
			x.b = t
		}
		else if op == .and
		{
			if x.mode != .condition {
				x = loadBool(x)
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
				x = loadBool(x)
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
	public mutating func Op2(_ op: Symbol, _ x: inout Item, _ y: inout Item)
	{
		if (x.type!.form == .integer) && (y.type!.form == .integer)
		{
			if (x.mode == .constant) && (y.mode == .constant)
			{
				/*overflow checks missing*/
				if op == .plus {
					x.a += y.a
				}
				else if op == .minus {
					x.a -= y.a
				}
				else if op == .times {
					x.a = x.a * y.a
				}
				else if op == .div {
					x.a = x.a / y.a
				}
				else if op == .mod {
					x.a = x.a % y.a
				}
				else { Lexer.mark("bad type") }
			}
			else
			{
				if op == .plus {
					putOp(.ADD, &x, &y)
				}
				else if op == .minus {
					putOp(.SUB, &x, &y)
				}
				else if op == .times {
					putOp(.MUL, &x, &y)
				}
				else if op == .div {
					putOp(.Div, &x, &y)
				}
				else if op == .mod {
					putOp(.Mod, &x, &y)
				}
				else { Lexer.mark("bad type") }
			}
		}
		else if (x.type!.form == .boolean) && (y.type!.form == .boolean)
		{
			if y.mode != .condition {
				y = loadBool(y)
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
		}
		else { Lexer.mark("bad type") }
	}

	// ---------------------------------------------------
	// x := x ? y
	public mutating func relation(
		_ op: Symbol,
		_ x: inout Item,
		_ y: inout Item)
	{
		if (x.type!.form != .integer) || (y.type!.form != .integer) {
			Lexer.mark("bad type")
		}
		else
		{
			putOp(.CMP, &x, &y)
			x.c = Int(op.rawValue - Symbol.eql.rawValue)
			regs.remove(y.r)
		}
		x.mode = .condition
		x.type = RISCCodeGenerator.boolType
		x.a = 0
		x.b = 0
	}

	// ---------------------------------------------------
	// x := y
	public mutating func store(_ x: inout Item, _ y: inout Item)
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
				y = load(y)
			}
			if x.mode == .variable
			{
				if x.lev == 0 {
					x.a = x.a - pc * 4
				}
				put(.STW, y.r, x.r, x.a)
			}
			else { Lexer.mark("illegal assignment") }
			regs.remove(x.r)
			regs.remove(y.r)
		}
		else { Lexer.mark("incompatible assignment") }
	}

	// ---------------------------------------------------
	public mutating func parameter(_ x: inout Item, _ symbolInfo: SymbolInfo)
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
				else { Lexer.mark("illegal parameter mode") }
				put(.PSH, r, SP, 4)
				regs.remove(r)
			}
			else
			{
				// value param
				if x.mode != .register {
					x = load(x)
				}
				put(.PSH, x.r, SP, 4)
				regs.remove(x.r)
			}
		}
		else { Lexer.mark("bad parameter type") }
	}

	// ---------------------------------------------------
	public mutating func conditionalJump(_ x: inout Item)
	{
		if x.type!.form == .boolean
		{
			if x.mode != .condition {
				x = loadBool(x)
			}
			putBR(.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			fixLink(x.b)
			x.a = pc - 1
		}
		else
		{
			Lexer.mark("Boolean?")
			x.a = pc
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
	public mutating func call(_ x: inout Item) {
		putBR(.BSR, x.a - pc)
	}

	// ---------------------------------------------------
	public mutating func ioCall(_ x: inout Item, _ y: inout Item)
	{
		var z = Item()
		
		if x.a < 4
		{
			if y.type!.form != .integer {
				Lexer.mark("Integer?")
			}
		}
		if x.a == 1
		{
			z.r = getReg()
			z.mode = .register
			z.type = RISCCodeGenerator.intType
			put(.RD, z.r, 0, 0)
			store(&y, &z)
		}
		else if x.a == 2
		{
			y = load(y)
			put(.WRD, 0, 0, y.r)
			regs.remove(y.r)
		}
		else if x.a == 3
		{
			y = load(y)
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


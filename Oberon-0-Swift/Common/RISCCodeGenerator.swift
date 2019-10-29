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
	internal static let maxCode = 1000
	internal static let maxRel = 200
	internal static let NofCom = 16

	/* class / mode */
	public static let Var = 1
	public static let Par = 2
	public static let Const = 3
	public static let Fld = 4
	public static let Typ = 5
	public static let Proc = 6
	public static let SProc = 7
	internal static let Reg = 10
	internal static let Cond = 11

	/* form */
	public static let Boolean = 0
	public static let Integer = 1
	public static let Array = 2
	public static let Record = 3

	/*reserved registers*/
	internal static let FP: Int = 12
	internal static let SP: Int = 13
	internal static let LNK: Int = 14
	internal static let PC: Int = 15

	public static let Head = 0

	public typealias `Type` = TypeDesc?
	public typealias Object = ObjDesc?

	public struct Item
	{
		public var mode: Int = 0
		public var lev: Int = 0
		public var type: Type = nil
		public var a: Int = 0
		internal var b: Int = 0
		internal var c: Int = 0
		internal var r: Int = 0
		
		public init() { }
	}
	
	// ---------------------------------------------------
	public class SymbolInfo: Equatable
	{
		public var kind: Int = 0
		public var level: Int = 0
		public var type: Type = nil
		public var name = ""
		public var val: Int = 0
		
		// ---------------------------------------------------
		init(
			name: String = "",
			kind: Int = 0,
			level: Int = 0,
			type: Type = nil,
			value: Int = 0)
		{
			self.name = name
			self.kind = kind
			self.level = level
			self.type = type
			self.val = value
		}
		
		// ---------------------------------------------------
		public static func == (left: SymbolInfo, right: SymbolInfo) -> Bool
		{
			return left.kind == right.kind
				&& left.level == right.level
				&& left.name == right.name
				&& left.val == right.val
				&& left.type == right.type
		}
	}

	// ---------------------------------------------------
	public class ObjDesc: Equatable
	{
		public var symbolInfo = SymbolInfo()
		
		public var kind: Int
		{
			get { return symbolInfo.kind }
			set { symbolInfo.kind = newValue }
		}
		
		public var level: Int
		{
			get { return symbolInfo.level }
			set { symbolInfo.level = newValue }
		}
		
		public var type: Type
		{
			get { return symbolInfo.type }
			set { symbolInfo.type = newValue }
		}
			
		public var name: String
		{
			get { return symbolInfo.name }
			set { symbolInfo.name = newValue }
		}
		
		public var val: Int
		{
			get { return symbolInfo.val }
			set { symbolInfo.val = newValue }
		}
		
		public var next: Object = nil
		public var dsc: Object = nil
		
		public required init() { }
		
		public static func == (left: ObjDesc, right: ObjDesc) -> Bool {
			return left.symbolInfo == right.symbolInfo
		}
	}

	public class TypeDesc: Equatable
	{
		public var form: Int = 0
		public var fields: ObjDesc? = nil
		public var base: Type = nil
		public var size: Int = 0
		public var len: Int = 0
		
		public required init() { }
		public init(form: Int, size: Int)
		{
			self.form = form
			self.size = size
		}
		
		public static func == (left: TypeDesc, right: TypeDesc) -> Bool
		{
			return left.form == right.form
				&& left.base == right.base
				&& left.size == right.size
				&& left.len == right.len
				&& left.fields == right.fields
		}
	}

	public static var boolType: Type = TypeDesc(form: Boolean, size: 4)
	public static var intType: Type = TypeDesc(form: Integer, size: 4)
	public static var curlev: Int = 0
	public static var pc: Int = 0
	internal static var cno: Int = 0
	public internal(set) static var entry: Int = 0
	internal static var fixlist: Int = 0

	/* used registers */
	internal static var regs = Set<Int>()

	internal static var code = [UInt32](repeating: 0, count: maxCode)

	// ---------------------------------------------------
	// Function to get object code so it can be saved by driver program
	public static func getObjectCode() -> [UInt32]
	{
		var objectCode = [UInt32]()
		objectCode.reserveCapacity(pc)
		for i in 0..<pc {
			objectCode.append(code[i])
		}
		return objectCode
	}

	/* commands */
	internal static var comname = makeComname()
	
	// ---------------------------------------------------
	fileprivate static func makeComname() -> [String]
	{
		var a = [String]()
		a.reserveCapacity(NofCom)
		return a
	}
	internal static var comadr = [Int](repeating: 0, count: NofCom)

	// for decoder
	internal static var mnemo = makeMneumonics()

	// ---------------------------------------------------
	internal static func getReg(_ r: inout Int)
	{
		var i: Int
		
		i = 0
		while (i < FP) && (regs.contains(i)) {
			i += 1
		}
		regs.insert(i)
		r = i
	}


	// ---------------------------------------------------
	/**
	return the format number, 0-3, for a given opCode.
	- Note: This function is NOT part of the original code
	*/
	internal static func instructionFormat(for opCode: RISCEmulator.OpCode) -> UInt32
	{
		if opCode < RISCEmulator.MOVI { return 0 }
		if opCode < RISCEmulator.LDW { return 1 }
		if opCode < RISCEmulator.BEQ { return 2 }
		return 3
	}

	// ---------------------------------------------------
	internal static func put(_ op: RISCEmulator.OpCode, _ a: Int, _ b: Int, _ c: Int)
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
	internal static func putBR(_ op: RISCEmulator.OpCode, _ disp: Int) {
		put(op, 0, 0, disp) /* emit branch instruction */
	}

	// ---------------------------------------------------
	internal static func testRange(_ x: Int)
	{
		// 18-bit entity
		if (x >= 0x20000) || (x < -0x20000) {
			Oberon0Lexer.mark("value too large")
		}
	}

	// ---------------------------------------------------
	internal static func load(_ x: inout Item)
	{
		var r: Int = 0
		
		// x.mode # Reg
		if x.mode == Var
		{
			if x.lev == 0 {
				x.a = x.a - pc * 4
			}
			getReg(&r)
			put(RISCEmulator.LDW, r, x.r, x.a)
			regs.remove(x.r)
			x.r = r
		}
		else if x.mode == Const
		{
			testRange(x.a)
			getReg(&x.r)
			put(RISCEmulator.MOVI, x.r, 0, x.a)
		}
		x.mode = Reg
	}

	// ---------------------------------------------------
	internal static func loadBool(_ x: inout Item)
	{
		if x.type?.form != Boolean {
			Oberon0Lexer.mark("Boolean?")
		}
		load(&x)
		x.mode = Cond
		x.a = 0
		x.b = 0
		x.c = 1
	}

	// ---------------------------------------------------
	static func putOp(
		_ cd: RISCEmulator.OpCode,
		_ x: inout Item,
		_ y: inout Item)
	{
		if x.mode != Reg {
			load(&x)
		}
		if y.mode == Const {
			testRange(y.a)
			put(cd + 16, x.r, x.r, y.a)
		}
		else
		{
			if y.mode != Reg {
				load(&y)
			}
			put(cd, x.r, x.r, y.r)
			regs.remove(y.r)
		}
	}

	// ---------------------------------------------------
	internal static func negated(_ cond: Int) -> Int {
		return (cond % 2 == 1) ? cond - 1 : cond + 1
	}

	// ---------------------------------------------------
	internal static func merged(_ L0: Int, _ L1: Int) -> Int
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
	internal static func fix(_ at: Int, _ with: Int)
	{
		code[at] =
			(code[at] / 0x400000) &* 0x400000 &+ UInt32((with % 0x400000))
	}

	// ---------------------------------------------------
	internal static func fixWith(_ L0: Int, _ L1: Int)
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
	public static func fixLink(_ L: Int)
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

	public static func IncLevel(_ n: Int) {
		curlev += n
	}

	// ---------------------------------------------------
	public static func MakeConstItem(_ x: inout Item, _ typ: Type, _ val: Int)
	{
		x.mode = Const
		x.type = typ
		x.a = val
	}

	// ---------------------------------------------------
	public static func MakeItem(_ x: inout Item, _ y: SymbolInfo)
	{
		var r: Int = 0
		
		x.mode = y.kind
		x.type = y.type
		x.lev = y.level
		x.a = y.val
		x.b = 0
		if y.level == 0 {
			x.r = PC
		}
		else if y.level == curlev {
			x.r = FP
		}
		else
		{
			Oberon0Lexer.mark("level!")
			x.r = 0
		}
		if y.kind == Par
		{
			getReg(&r)
			put(RISCEmulator.LDW, r, x.r, x.a)
			x.mode = Var
			x.r = r
			x.a = 0
		}
	}

	/* x := x.y */
	public static func Field(_ x: inout Item, _ y: Object)
	{
		x.a += y!.val
		x.type = y!.type
	}

	/* x := x[y] */
	public static func Index(_ x: inout Item, _ y: inout Item)
	{
		if y.type != intType {
			Oberon0Lexer.mark("index not integer")
		}
		if y.mode == Const
		{
			if (y.a < 0) || (y.a >= x.type!.len) {
				Oberon0Lexer.mark("bad index")
			}
			x.a += y.a * Int(x.type!.base!.size)
		}
		else
		{
			if y.mode != Reg {
				load(&y)
			}
			put(RISCEmulator.CHKI, y.r, 0, x.type!.len)
			put(RISCEmulator.MULI, y.r, y.r, x.type!.base!.size)
			put(RISCEmulator.ADD, y.r, x.r, y.r)
			regs.remove(x.r)
			x.r = y.r
		}
		x.type = x.type!.base
	}

	public static func Op1(_ op: OberonSymbol, _ x: inout Item) /* x := op x */
	{
		var t: Int

		if op == .minus
		{
			if x.type!.form != Integer {
				Oberon0Lexer.mark("bad type")
			}
			else if x.mode == Const {
				x.a = -x.a
			}
			else
			{
				if x.mode == Var {
					load(&x)
				}
				put(RISCEmulator.MVN, x.r, 0, x.r)
			}
		}
		else if op == .not
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			x.c = negated(x.c)
			t = x.a
			x.a = x.b
			x.b = t
		}
		else if op == .and
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			putBR(RISCEmulator.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			x.a = pc - 1
			fixLink(x.b)
			x.b = 0
		}
		else if op == .or
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			putBR(RISCEmulator.BEQ + x.c, x.b)
			regs.remove(x.r)
			x.b = pc - 1
			fixLink(x.a);
			x.a = 0
		}
	}

	/* x := x op y */
	public static func Op2(_ op: OberonSymbol, _ x: inout Item, _ y: inout Item)
	{
		if (x.type!.form == Integer) && (y.type!.form == Integer)
		{
			if (x.mode == Const) && (y.mode == Const)
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
				else { Oberon0Lexer.mark("bad type") }
			}
			else
			{
				if op == .plus {
					putOp(RISCEmulator.ADD, &x, &y)
				}
				else if op == .minus {
					putOp(RISCEmulator.SUB, &x, &y)
				}
				else if op == .times {
					putOp(RISCEmulator.MUL, &x, &y)
				}
				else if op == .div {
					putOp(RISCEmulator.Div, &x, &y)
				}
				else if op == .mod {
					putOp(RISCEmulator.Mod, &x, &y)
				}
				else { Oberon0Lexer.mark("bad type") }
			}
		}
		else if (x.type!.form == Boolean) && (y.type!.form == Boolean)
		{
			if y.mode != Cond {
				loadBool(&y)
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
		else { Oberon0Lexer.mark("bad type") }
	}

	/* x := x ? y */
	public static func Relation(_ op: OberonSymbol, _ x: inout Item, _ y: inout Item)
	{
		if (x.type!.form != Integer) || (y.type!.form != Integer) {
			Oberon0Lexer.mark("bad type")
		}
		else
		{
			putOp(RISCEmulator.CMP, &x, &y)
			x.c = Int(op.rawValue - OberonSymbol.eql.rawValue)
			regs.remove(y.r)
		}
		x.mode = Cond
		x.type = boolType
		x.a = 0
		x.b = 0
	}

	/* x := y */
	public static func Store(_ x: inout Item, _ y: inout Item)
	{
		// this variable is declared in the original Oberon code, but never used
		// commented it out to silence Swift warning about unused variable
		// var r: Int

		if x.type != nil, y.type != nil,
			[Boolean, Integer].contains(x.type!.form)
			&& (x.type!.form == y.type!.form)
		{
			if y.mode == Cond
			{
				put(RISCEmulator.BEQ + negated(y.c), y.r, 0, y.a)
				regs.remove(y.r)
				y.a = pc - 1
				fixLink(y.b)
				getReg(&y.r)
				put(RISCEmulator.MOVI, y.r, 0, 1)
				putBR(RISCEmulator.BR, 2)
				fixLink(y.a)
				put(RISCEmulator.MOVI, y.r, 0, 0)
			}
			else if y.mode != Reg {
				load(&y)
			}
			if x.mode == Var
			{
				if x.lev == 0 {
					x.a = x.a - pc * 4
				}
				put(RISCEmulator.STW, y.r, x.r, x.a)
			}
			else { Oberon0Lexer.mark("illegal assignment") }
			regs.remove(x.r)
			regs.remove(y.r)
		}
		else { Oberon0Lexer.mark("incompatible assignment") }
	}

	public static func Parameter(_ x: inout Item, _ ftyp: Type, _ `class`: Int)
	{
		var r: Int = 0
		
		if x.type == ftyp
		{
			if `class` == Par
			{
				/*Var param*/
				if x.mode == Var
				{
					if x.a != 0
					{
						getReg(&r)
						put(RISCEmulator.ADDI, r, x.r, x.a)
					}
					else {
						r = x.r
					}
				}
				else { Oberon0Lexer.mark("illegal parameter mode") }
				put(RISCEmulator.PSH, r, SP, 4)
				regs.remove(r)
			}
			else
			{
				/*value param*/
				if x.mode != Reg {
					load(&x)
				}
				put(RISCEmulator.PSH, x.r, SP, 4)
				regs.remove(x.r)
			}
		}
		else { Oberon0Lexer.mark("bad parameter type") }
	}

	// ---------------------------------------------------
	public static func conditionalJump(_ x: inout Item)
	{
		if x.type!.form == Boolean
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			putBR(RISCEmulator.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			fixLink(x.b)
			x.a = pc - 1
		}
		else
		{
			Oberon0Lexer.mark("Boolean?")
			x.a = pc
		}
	}

	// ---------------------------------------------------
	public static func jumpBack(_ L: Int) {
		putBR(RISCEmulator.BR, L - pc)
	}

	// ---------------------------------------------------
	public static func jumpForward(_ L: inout Int)
	{
		putBR(RISCEmulator.BR, L)
		L = pc - 1
	}

	// ---------------------------------------------------
	public static func call(_ x: inout Item) {
		putBR(RISCEmulator.BSR, x.a - pc)
	}

	// ---------------------------------------------------
	public static func ioCall(_ x: inout Item, _ y: inout Item)
	{
		var z = Item()
		
		if x.a < 4
		{
			if y.type!.form != Integer {
				Oberon0Lexer.mark("Integer?")
			}
		}
		if x.a == 1
		{
			getReg(&z.r)
			z.mode = Reg
			z.type = intType
			put(RISCEmulator.RD, z.r, 0, 0)
			Store(&y, &z)
		}
		else if x.a == 2
		{
			load(&y)
			put(RISCEmulator.WRD, 0, 0, y.r)
			regs.remove(y.r)
		}
		else if x.a == 3
		{
			load(&y)
			put(RISCEmulator.WRH, 0, 0, y.r)
			regs.remove(y.r)
		}
		else {
			put(RISCEmulator.WRL, 0, 0, 0)
		}
	}

	// ---------------------------------------------------
	public static func header(_ size: Int)
	{
		entry = pc
		put(RISCEmulator.MOVI, SP, 0, RISCEmulator.MemSize - size)
		put(RISCEmulator.PSH, LNK, SP, 4) // init SP
	}

	// ---------------------------------------------------
	public static func enter(_ size: Int)
	{
		put(RISCEmulator.PSH, LNK, SP, 4)
		put(RISCEmulator.PSH, FP, SP, 4)
		put(RISCEmulator.MOV, FP, 0, SP)
		put(RISCEmulator.SUBI, SP, SP, size)
	}

	// ---------------------------------------------------
	public static func procedureReturn(_ size: Int)
	{
		put(RISCEmulator.MOV, SP, 0, FP)
		put(RISCEmulator.POP, FP, SP, 4)
		put(RISCEmulator.POP, LNK, SP, size + 4)
		putBR(RISCEmulator.RET, LNK)
	}

	// ---------------------------------------------------
	public static func open()
	{
		curlev = 0
		pc = 0
		cno = 0
		regs = []
	}

	// ---------------------------------------------------
	public static func close()
	{
		put(RISCEmulator.POP, LNK, SP, 4)
		putBR(RISCEmulator.RET, LNK)
	}

	// ---------------------------------------------------
	public static func enterCmd(_ name: inout String)
	{
		comname.append(name)
		comadr[cno] = pc * 4
		cno += 1
	}

	// ---------------------------------------------------
	public static func decode<OutStream: TextOutputStream>(
		to outStream: inout OutStream)
	{
		var outStr = "entry\(entry * 4, pad: 6)\n"
		
		for i in 0..<pc
		{
			let w = code[i]
			let op = RISCEmulator.OpCode(w / 0x4000000 % 0x40)
			outStr += "\(4 * i, pad: 4)\t\(mnemo[op.code] ?? "Unknown")"
			
			var a: Int
			if op < RISCEmulator.BEQ
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

	// ---------------------------------------------------
	fileprivate static func makeMneumonics() -> [UInt32:String]
	{
		var mnemo = [UInt32:String]()

		mnemo[RISCEmulator.MOV.code] = "MOV "
		mnemo[RISCEmulator.MVN.code] = "MVN "
		mnemo[RISCEmulator.ADD.code] = "ADD "
		mnemo[RISCEmulator.SUB.code] = "SUB "
		mnemo[RISCEmulator.MUL.code] = "MUL "
		mnemo[RISCEmulator.Div.code] = "DIV "
		mnemo[RISCEmulator.Mod.code] = "MOD "
		mnemo[RISCEmulator.CMP.code] = "CMP "
		mnemo[RISCEmulator.MOVI.code] = "MOVI"
		mnemo[RISCEmulator.MVNI.code] = "MVNI"
		mnemo[RISCEmulator.ADDI.code] = "ADDI"
		mnemo[RISCEmulator.SUBI.code] = "SUBI"
		mnemo[RISCEmulator.MULI.code] = "MULI"
		mnemo[RISCEmulator.DIVI.code] = "DIVI"
		mnemo[RISCEmulator.MODI.code] = "MODI"
		mnemo[RISCEmulator.CMPI.code] = "CMPI"
		mnemo[RISCEmulator.CHKI.code] = "CHKI"
		mnemo[RISCEmulator.LDW.code] = "LDW "
		mnemo[RISCEmulator.LDB.code] = "LDB "
		mnemo[RISCEmulator.POP.code] = "POP "
		mnemo[RISCEmulator.STW.code] = "STW "
		mnemo[RISCEmulator.STB.code] = "STB "
		mnemo[RISCEmulator.PSH.code] = "PSH "
		mnemo[RISCEmulator.BEQ.code] = "BEQ "
		mnemo[RISCEmulator.BNE.code] = "BNE "
		mnemo[RISCEmulator.BLT.code] = "BLT "
		mnemo[RISCEmulator.BGE.code] = "BGE "
		mnemo[RISCEmulator.BLE.code] = "BLE "
		mnemo[RISCEmulator.BGT.code] = "BGT "
		mnemo[RISCEmulator.BR.code] = "BR "
		mnemo[RISCEmulator.BSR.code] = "BSR "
		mnemo[RISCEmulator.RET.code] = "RET "
		mnemo[RISCEmulator.RD.code] = "READ"
		mnemo[RISCEmulator.WRD.code] = "WRD "
		mnemo[RISCEmulator.WRH.code] = "WRH "
		mnemo[RISCEmulator.WRL.code] = "WRL "
		
		return mnemo
	}
}


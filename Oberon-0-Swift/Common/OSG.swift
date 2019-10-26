//
//  OSG.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

public struct OSG
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

	public struct Item: DefaultInitializable
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

	public class ObjDesc: Equatable, DefaultInitializable
	{
		public var `class`: Int = 0
		public var lev: Int = 0
		public var next: Object = nil
		public var dsc: Object = nil
		public var type: Type = nil
		public var name = ""
		public var val: Int = 0
		
		public required init() { }
		
		public static func == (left: ObjDesc, right: ObjDesc) -> Bool
		{
			return left.class == right.class
				&& left.lev == right.lev
				&& left.name == right.name
				&& left.val == right.val
				&& left.type == right.type
				&& left.dsc == right.dsc
				&& left.next == right.next
		}
	}

	public class TypeDesc: Equatable, DefaultInitializable
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
	
	fileprivate static func makeComname() -> [String]
	{
		var a = [String]()
		a.reserveCapacity(NofCom)
		return a
	}
	internal static var comadr = [Int](repeating: 0, count: NofCom)

	/*for decoder*/
	internal static var mnemo = makeMneumonics()

	internal static func GetReg(_ r: inout Int)
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
	Return the format number, 0-3, for a given opCode.
	- Note: This function is NOT part of the original code
	*/
	internal static func instructionFormat(for opCode: RISC.OpCode) -> UInt32
	{
		if opCode < RISC.MOVI { return 0 }
		if opCode < RISC.LDW { return 1 }
		if opCode < RISC.BEQ { return 2 }
		return 3
	}

	internal static func Put(_ op: RISC.OpCode, _ a: Int, _ b: Int, _ c: Int)
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

	internal static func PutBR(_ op: RISC.OpCode, _ disp: Int) {
		Put(op, 0, 0, disp) /* emit branch instruction */
	}

	internal static func TestRange(_ x: Int)
	{
		/* 18-bit entity */
		if (x >= 0x20000) || (x < -0x20000) {
			OSS.Mark("value too large")
		}
	}

	internal static func load(_ x: inout Item)
	{
		var r: Int = 0
		
		/* x.mode # Reg */
		if x.mode == Var
		{
			if x.lev == 0 {
				x.a = x.a - pc * 4
			}
			GetReg(&r)
			Put(RISC.LDW, r, x.r, x.a)
			regs.remove(x.r)
			x.r = r
		}
		else if x.mode == Const
		{
			TestRange(x.a)
			GetReg(&x.r)
			Put(RISC.MOVI, x.r, 0, x.a)
		}
		x.mode = Reg
	}

	internal static func loadBool(_ x: inout Item)
	{
		if x.type?.form != Boolean {
			OSS.Mark("Boolean?")
		}
		load(&x)
		x.mode = Cond
		x.a = 0
		x.b = 0
		x.c = 1
	}

	// ---------------------------------------------------
	static func PutOp(
		_ cd: RISC.OpCode,
		_ x: inout Item,
		_ y: inout Item)
	{
		if x.mode != Reg {
			load(&x)
		}
		if y.mode == Const {
			TestRange(y.a)
			Put(cd + 16, x.r, x.r, y.a)
		}
		else
		{
			if y.mode != Reg {
				load(&y)
			}
			Put(cd, x.r, x.r, y.r)
			regs.remove(y.r)
		}
	}

	internal static func negated(_ cond: Int) -> Int {
		return (cond % 2 == 1) ? cond - 1 : cond + 1
	}

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

	internal static func fix(_ at: Int, _ with: Int)
	{
		code[at] =
			(code[at] / 0x400000) &* 0x400000 &+ UInt32((with % 0x400000))
	}

	internal static func FixWith(_ L0: Int, _ L1: Int)
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
	public static func FixLink(_ L: Int)
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

	public static func MakeConstItem(_ x: inout Item, _ typ: Type, _ val: Int)
	{
		x.mode = Const
		x.type = typ
		x.a = val
	}

	public static func MakeItem(_ x: inout Item, _ y: Object)
	{
		var r: Int = 0
		
		x.mode = y!.class
		x.type = y!.type
		x.lev = y!.lev
		x.a = y!.val
		x.b = 0
		if y!.lev == 0 {
			x.r = PC
		}
		else if y!.lev == curlev {
			x.r = FP
		}
		else
		{
			OSS.Mark("level!")
			x.r = 0
		}
		if y!.class == Par
		{
			GetReg(&r)
			Put(RISC.LDW, r, x.r, x.a)
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
			OSS.Mark("index not integer")
		}
		if y.mode == Const
		{
			if (y.a < 0) || (y.a >= x.type!.len) {
				OSS.Mark("bad index")
			}
			x.a += y.a * Int(x.type!.base!.size)
		}
		else
		{
			if y.mode != Reg {
				load(&y)
			}
			Put(RISC.CHKI, y.r, 0, x.type!.len)
			Put(RISC.MULI, y.r, y.r, x.type!.base!.size)
			Put(RISC.ADD, y.r, x.r, y.r)
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
				OSS.Mark("bad type")
			}
			else if x.mode == Const {
				x.a = -x.a
			}
			else
			{
				if x.mode == Var {
					load(&x)
				}
				Put(RISC.MVN, x.r, 0, x.r)
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
			PutBR(RISC.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			x.a = pc - 1
			FixLink(x.b)
			x.b = 0
		}
		else if op == .or
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			PutBR(RISC.BEQ + x.c, x.b)
			regs.remove(x.r)
			x.b = pc - 1
			FixLink(x.a);
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
				else { OSS.Mark("bad type") }
			}
			else
			{
				if op == .plus {
					PutOp(RISC.ADD, &x, &y)
				}
				else if op == .minus {
					PutOp(RISC.SUB, &x, &y)
				}
				else if op == .times {
					PutOp(RISC.MUL, &x, &y)
				}
				else if op == .div {
					PutOp(RISC.Div, &x, &y)
				}
				else if op == .mod {
					PutOp(RISC.Mod, &x, &y)
				}
				else { OSS.Mark("bad type") }
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
		else { OSS.Mark("bad type") }
	}

	/* x := x ? y */
	public static func Relation(_ op: OberonSymbol, _ x: inout Item, _ y: inout Item)
	{
		if (x.type!.form != Integer) || (y.type!.form != Integer) {
			OSS.Mark("bad type")
		}
		else
		{
			PutOp(RISC.CMP, &x, &y)
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
				Put(RISC.BEQ + negated(y.c), y.r, 0, y.a)
				regs.remove(y.r)
				y.a = pc - 1
				FixLink(y.b)
				GetReg(&y.r)
				Put(RISC.MOVI, y.r, 0, 1)
				PutBR(RISC.BR, 2)
				FixLink(y.a)
				Put(RISC.MOVI, y.r, 0, 0)
			}
			else if y.mode != Reg {
				load(&y)
			}
			if x.mode == Var
			{
				if x.lev == 0 {
					x.a = x.a - pc * 4
				}
				Put(RISC.STW, y.r, x.r, x.a)
			}
			else { OSS.Mark("illegal assignment") }
			regs.remove(x.r)
			regs.remove(y.r)
		}
		else { OSS.Mark("incompatible assignment") }
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
						GetReg(&r)
						Put(RISC.ADDI, r, x.r, x.a)
					}
					else {
						r = x.r
					}
				}
				else { OSS.Mark("illegal parameter mode") }
				Put(RISC.PSH, r, SP, 4)
				regs.remove(r)
			}
			else
			{
				/*value param*/
				if x.mode != Reg {
					load(&x)
				}
				Put(RISC.PSH, x.r, SP, 4)
				regs.remove(x.r)
			}
		}
		else { OSS.Mark("bad parameter type") }
	}

	/*---------------------------------*/

	public static func CJump(_ x: inout Item)
	{
		if x.type!.form == Boolean
		{
			if x.mode != Cond {
				loadBool(&x)
			}
			PutBR(RISC.BEQ + negated(x.c), x.a)
			regs.remove(x.r)
			FixLink(x.b)
			x.a = pc - 1
		}
		else
		{
			OSS.Mark("Boolean?")
			x.a = pc
		}
	}

	public static func BJump(_ L: Int) {
		PutBR(RISC.BR, L - pc)
	}

	public static func FJump(_ L: inout Int)
	{
		PutBR(RISC.BR, L)
		L = pc - 1
	}

	public static func Call(_ x: inout Item) {
		PutBR(RISC.BSR, x.a - pc)
	}

	public static func IOCall(_ x: inout Item, _ y: inout Item)
	{
		var z = Item()
		
		if x.a < 4
		{
			if y.type!.form != Integer {
				OSS.Mark("Integer?")
			}
		}
		if x.a == 1
		{
			GetReg(&z.r)
			z.mode = Reg
			z.type = intType
			Put(RISC.RD, z.r, 0, 0)
			Store(&y, &z)
		}
		else if x.a == 2
		{
			load(&y)
			Put(RISC.WRD, 0, 0, y.r)
			regs.remove(y.r)
		}
		else if x.a == 3
		{
			load(&y)
			Put(RISC.WRH, 0, 0, y.r)
			regs.remove(y.r)
		}
		else {
			Put(RISC.WRL, 0, 0, 0)
		}
	}

	public static func Header(_ size: Int)
	{
		entry = pc
		Put(RISC.MOVI, SP, 0, RISC.MemSize - size)
		/*init SP*/
		Put(RISC.PSH, LNK, SP, 4)
	}

	public static func Enter(_ size: Int)
	{
		Put(RISC.PSH, LNK, SP, 4)
		Put(RISC.PSH, FP, SP, 4)
		Put(RISC.MOV, FP, 0, SP)
		Put(RISC.SUBI, SP, SP, size)
	}

	public static func Return(_ size: Int)
	{
		Put(RISC.MOV, SP, 0, FP)
		Put(RISC.POP, FP, SP, 4)
		Put(RISC.POP, LNK, SP, size + 4)
		PutBR(RISC.RET, LNK)
	}

	public static func Open()
	{
		curlev = 0
		pc = 0
		cno = 0
		regs = []
	}

	public static func Close()
	{
		Put(RISC.POP, LNK, SP, 4)
		PutBR(RISC.RET, LNK)
	}

	public static func EnterCmd(_ name: inout String)
	{
		comname.append(name)
		comadr[cno] = pc * 4
		cno += 1
	}

	// ---------------------------------------------------
	public static func Decode(_ T: inout Texts.Text)
	{
		var outStr = "entry\(entry * 4, pad: 6)\n"
		
		for i in 0..<pc
		{
			let w = code[i]
			let op = RISC.OpCode(w / 0x4000000 % 0x40)
			outStr += "\(4 * i, pad: 4)\t\(mnemo[op.code] ?? "Unknown")"
			
			var a: Int
			if op < RISC.BEQ
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
		outStr += "\n"
		Texts.Append(T, outStr)
	}

	// ---------------------------------------------------
	fileprivate static func makeMneumonics() -> [UInt32:String]
	{
		var mnemo = [UInt32:String]()

		mnemo[RISC.MOV.code] = "MOV "
		mnemo[RISC.MVN.code] = "MVN "
		mnemo[RISC.ADD.code] = "ADD "
		mnemo[RISC.SUB.code] = "SUB "
		mnemo[RISC.MUL.code] = "MUL "
		mnemo[RISC.Div.code] = "DIV "
		mnemo[RISC.Mod.code] = "MOD "
		mnemo[RISC.CMP.code] = "CMP "
		mnemo[RISC.MOVI.code] = "MOVI"
		mnemo[RISC.MVNI.code] = "MVNI"
		mnemo[RISC.ADDI.code] = "ADDI"
		mnemo[RISC.SUBI.code] = "SUBI"
		mnemo[RISC.MULI.code] = "MULI"
		mnemo[RISC.DIVI.code] = "DIVI"
		mnemo[RISC.MODI.code] = "MODI"
		mnemo[RISC.CMPI.code] = "CMPI"
		mnemo[RISC.CHKI.code] = "CHKI"
		mnemo[RISC.LDW.code] = "LDW "
		mnemo[RISC.LDB.code] = "LDB "
		mnemo[RISC.POP.code] = "POP "
		mnemo[RISC.STW.code] = "STW "
		mnemo[RISC.STB.code] = "STB "
		mnemo[RISC.PSH.code] = "PSH "
		mnemo[RISC.BEQ.code] = "BEQ "
		mnemo[RISC.BNE.code] = "BNE "
		mnemo[RISC.BLT.code] = "BLT "
		mnemo[RISC.BGE.code] = "BGE "
		mnemo[RISC.BLE.code] = "BLE "
		mnemo[RISC.BGT.code] = "BGT "
		mnemo[RISC.BR.code] = "BR "
		mnemo[RISC.BSR.code] = "BSR "
		mnemo[RISC.RET.code] = "RET "
		mnemo[RISC.RD.code] = "READ"
		mnemo[RISC.WRD.code] = "WRD "
		mnemo[RISC.WRH.code] = "WRH "
		mnemo[RISC.WRL.code] = "WRL "
		
		return mnemo
	}
}


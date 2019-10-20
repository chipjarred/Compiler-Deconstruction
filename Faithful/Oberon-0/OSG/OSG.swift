//
//  OSG.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import OSS
import RISC
import Oberon
import Texts

internal let maxCode = 1000
internal let maxRel = 200
internal let NofCom = 16

/* class / mode */
public let Var = 1
public let Par = 2
public let Const = 3
public let Fld = 4
public let Typ = 5
public let Proc = 6
public let SProc = 7
internal let Reg = 10
internal let Cond = 11

/* form */
public let Boolean = 0
public let Integer = 1
public let Array = 2
public let Record = 3
internal let MOV: LONGINT = 0
internal let MVN: LONGINT = 1
internal let ADD: LONGINT = 2
internal let SUB: LONGINT = 3
internal let MUL: LONGINT = 4
internal let Div: LONGINT = 5
internal let Mod: LONGINT = 6
internal let CMP: LONGINT = 7
internal let MOVI: LONGINT = 16
internal let MVNI: LONGINT = 17
internal let ADDI: LONGINT = 18
internal let SUBI: LONGINT = 19
internal let MULI: LONGINT = 20
internal let DIVI: LONGINT = 21
internal let MODI: LONGINT = 22
internal let CMPI: LONGINT = 23
internal let CHKI: LONGINT = 24
internal let LDW: LONGINT = 32
internal let LDB: LONGINT = 33
internal let POP: LONGINT = 34
internal let STW: LONGINT = 36
internal let STB: LONGINT = 37
internal let PSH: LONGINT = 38
internal let RD: LONGINT = 40
internal let WRD: LONGINT = 41
internal let WRH: LONGINT = 42
internal let WRL: LONGINT = 43
internal let BEQ: LONGINT = 48
internal let BNE: LONGINT = 49
internal let BLT: LONGINT = 50
internal let BGE: LONGINT = 51
internal let BLE: LONGINT = 52
internal let BGT: LONGINT = 53
internal let BR: LONGINT = 56
internal let BSR: LONGINT = 57

/*reserved registers*/
internal let RET: LONGINT = 58
internal let FP: LONGINT = 12
internal let SP = 13
internal let LNK: LONGINT = 14
internal let PC: LONGINT = 15

public let Head = 0

public typealias Type = TypeDesc?
public typealias Object = ObjDesc?

public struct Item: DefaultInitializable
{
	public var mode: INTEGER = 0
	public var lev: INTEGER = 0
	public var type: Type = nil
	public var a: LONGINT = 0
	internal var b: LONGINT = 0
	internal var c: LONGINT = 0
	internal var r: LONGINT = 0
	
	public init() { }
}

public class ObjDesc: Equatable, DefaultInitializable
{
	public var `class`: INTEGER = 0
	public var lev: INTEGER = 0
	public var next: Object = nil
	public var dsc: Object = nil
	public var type: Type = nil
	public var name = OSS.Ident(count: OSS.id.count)
	public var val: LONGINT = 0
	
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
	public var form: INTEGER = 0
	public var fields: ObjDesc? = nil
	public var base: Type = nil
	public var size: INTEGER = 0
	public var len: INTEGER = 0
	
	public required init() { }
	public init(form: INTEGER, size: INTEGER)
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

public var boolType: Type = TypeDesc(form: Boolean, size: 4)
public var intType: Type = TypeDesc(form: Integer, size: 4)
public var curlev: INTEGER = 0
public var pc: INTEGER = 0
internal var cno: INTEGER = 0
internal var entry: LONGINT = 0
internal var fixlist: LONGINT = 0

/* used registers */
internal var regs = Set<INTEGER>()
internal var W = Texts.Writer()
internal var code = ARRAY<LONGINT>(count: maxCode)

/* commands */
internal var comname = [OSS.Ident](
	repeating: OSS.Ident(count: OSS.id.count),
	count: NofCom
)
internal var comadr = ARRAY<LONGINT>(count: NofCom)

/*for decoder*/
internal var mnemo = [ARRAY<CHAR>](
	repeating: ARRAY<CHAR>(count: 5),
	count: 64
)

internal func GetReg(_ r: inout LONGINT)
{
	var i: INTEGER
	
	i = 0
	while (i < FP) && (regs.contains(i)) {
		i += 1
	}
	INCL(&regs, i)
	r = LONGINT(i)
}

// This overload exists just to avoid lots explicit casts that aren't in the
// original, because Oberon does integer promotion.
internal func Put<T, U, V, W>(_ op: T, _ a: U, _ b: V, _ c: W)
	where T: FixedWidthInteger,
		U: FixedWidthInteger,
		V: FixedWidthInteger,
		W: FixedWidthInteger
{
	Put(LONGINT(op), LONGINT(a), LONGINT(b), LONGINT(c))
}

internal func Put(_ op: LONGINT, _ a: LONGINT, _ b: LONGINT, _ c: LONGINT)
{
	code[pc] = ASH(ASH(ASH(op, 4) + a, 4) + b, 18) + (c % 0x40000)
	pc += 1
}

internal func PutBR(_ op: LONGINT, _ disp: LONGINT)
{
	/* emit branch instruction */
	code[pc] = ASH(op - 0x40, 26) + (disp % 0x4000000)
	pc += 1
}

internal func TestRange(_ x: LONGINT)
{
	/* 18-bit entity */
	if (x >= 0x20000) || (x < -0x20000) {
		OSS.Mark("value too large")
	}
}

internal func load(_ x: inout Item)
{
	var r: LONGINT = 0
	
	/* x.mode # Reg */
	if x.mode == Var
	{
		if x.lev == 0 {
			x.a = x.a - LONGINT(pc) * 4
		}
		GetReg(&r)
		Put(LDW, r, x.r, x.a)
		EXCL(&regs, x.r)
		x.r = r
	}
	else if x.mode == Const
	{
		TestRange(x.a)
		GetReg(&x.r)
		Put(MOVI, x.r, 0, x.a)
	}
	x.mode = Reg
}

internal func loadBool(_ x: inout Item)
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

internal func PutOp(_ cd: LONGINT, _ x: inout Item, _ y: inout Item)
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
		EXCL(&regs, y.r)
	}
}

internal func negated(_ cond: LONGINT) -> LONGINT {
	return ODD(cond) ? cond - 1 : cond + 1
}

internal func merged(_ L0: LONGINT, _ L1: LONGINT) -> LONGINT
{
	var L2: LONGINT = 0
	var L3: LONGINT = 0
	
	if L0 != 0
	{
		L2 = L0
		while true
		{
			L3 = code[Int(L2)] % 0x40000
			if L3 == 0 { break }
			L2 = L3
		}
		code[Int(L2)] = code[Int(L2)] - L3 + L1
		return L0
	}
	else {
		return L1
	}
}

internal func fix(_ at: LONGINT, _ with: LONGINT) {
	code[Int(at)] = (code[Int(at)] / 0x400000) * 0x400000 + (with % 0x400000)
}

internal func FixWith(_ L0: LONGINT, _ L1: LONGINT)
{
	var L2: LONGINT
	var L0 = L0
	
	while L0 != 0
	{
		L2 = code[Int(L0)] % 0x40000
		fix(L0, L1 - L0)
		L0 = L2
	}
}

public func FixLink(_ L: LONGINT)
{
	var L1: LONGINT
	var L = L
	
	while L != 0
	{
		L1 = code[Int(L)] % 0x40000
		fix(L, LONGINT(pc) - L)
		L = L1
	}
}

/*-----------------------------------------------*/

public func IncLevel(_ n: INTEGER) {
	curlev += n
}

public func MakeConstItem(_ x: inout Item, _ typ: Type, _ val: LONGINT)
{
	x.mode = Const
	x.type = typ
	x.a = val
}

public func MakeItem(_ x: inout Item, _ y: Object)
{
	var r: LONGINT = 0
	
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
		Put(LDW, r, x.r, x.a)
		x.mode = Var
		x.r = r
		x.a = 0
	}
}

/* x := x.y */
public func Field(_ x: inout Item, _ y: Object)
{
	x.a += y!.val
	x.type = y!.type
}

/* x := x[y] */
public func Index(_ x: inout Item, _ y: inout Item)
{
	if y.type != intType {
		OSS.Mark("index not integer")
	}
	if y.mode == Const
	{
		if (y.a < 0) || (y.a >= x.type!.len) {
			OSS.Mark("bad index")
		}
		x.a += y.a * LONGINT(x.type!.base!.size)
	}
	else
	{
		if y.mode != Reg {
			load(&y)
		}
		Put(CHKI, y.r, 0, x.type!.len)
		Put(MULI, y.r, y.r, x.type!.base!.size)
		Put(ADD, y.r, x.r, y.r)
		EXCL(&regs, x.r)
		x.r = y.r
	}
	x.type = x.type!.base
}

public func Op1(_ op: INTEGER, _ x: inout Item) /* x := op x */
{
	var t: LONGINT

	if op == OSS.minus
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
			Put(MVN, x.r, 0, x.r)
		}
	}
	else if op == OSS.not
	{
		if x.mode != Cond {
			loadBool(&x)
		}
		x.c = negated(x.c)
		t = x.a
		x.a = x.b
		x.b = t
	}
	else if op == OSS.and
	{
		if x.mode != Cond {
			loadBool(&x)
		}
		PutBR(BEQ + negated(x.c), x.a)
		EXCL(&regs, x.r)
		x.a = LONGINT(pc - 1)
		FixLink(x.b)
		x.b = 0
	}
	else if op == OSS.or
	{
		if x.mode != Cond {
			loadBool(&x)
		}
		PutBR(BEQ + x.c, x.b)
		EXCL(&regs, x.r)
		x.b = LONGINT(pc - 1)
		FixLink(x.a);
		x.a = 0
	}
}

/* x := x op y */
public func Op2(_ op: INTEGER, _ x: inout Item, _ y: inout Item)
{
	if (x.type!.form == Integer) && (y.type!.form == Integer)
	{
		if (x.mode == Const) && (y.mode == Const)
		{
			/*overflow checks missing*/
			if op == OSS.plus {
				x.a += y.a
			}
			else if op == OSS.minus {
				x.a -= y.a
			}
			else if op == OSS.times {
				x.a = x.a * y.a
			}
			else if op == OSS.div {
				x.a = x.a / y.a
			}
			else if op == OSS.mod {
				x.a = x.a % y.a
			}
			else { OSS.Mark("bad type") }
		}
		else
		{
			if op == OSS.plus {
				PutOp(ADD, &x, &y)
			}
			else if op == OSS.minus {
				PutOp(SUB, &x, &y)
			}
			else if op == OSS.times {
				PutOp(MUL, &x, &y)
			}
			else if op == OSS.div {
				PutOp(Div, &x, &y)
			}
			else if op == OSS.mod {
				PutOp(Mod, &x, &y)
			}
			else { OSS.Mark("bad type") }
		}
	}
	else if (x.type!.form == Boolean) && (y.type!.form == Boolean)
	{
		if y.mode != Cond {
			loadBool(&y)
		}
		if op == OSS.or
		{
			x.a = y.a
			x.b = merged(y.b, x.b)
			x.c = y.c
		}
		else if op == OSS.and
		{
			x.a = merged(y.a, x.a)
			x.b = y.b
			x.c = y.c
		}
	}
	else { OSS.Mark("bad type") }
}

/* x := x ? y */
public func Relation(_ op: INTEGER, _ x: inout Item, _ y: inout Item)
{
	if (x.type!.form != Integer) || (y.type!.form != Integer) {
		OSS.Mark("bad type")
	}
	else
	{
		PutOp(CMP, &x, &y)
		x.c = LONGINT(op - OSS.eql)
		EXCL(&regs, y.r)
	}
	x.mode = Cond
	x.type = boolType
	x.a = 0
	x.b = 0
}

/* x := y */
public func Store(_ x: inout Item, _ y: inout Item)
{
	// this variable is declared in the original Oberon code, but never used
	// commented it out to silence Swift warning about unused variable
	// var r: LONGINT

	if [Boolean, Integer].contains(x.type!.form)
		&& (x.type!.form == y.type!.form)
	{
		if y.mode == Cond
		{
			Put(BEQ + negated(y.c), y.r, 0, y.a)
			EXCL(&regs, y.r)
			y.a = LONGINT(pc - 1)
			FixLink(y.b)
			GetReg(&y.r)
			Put(MOVI, y.r, 0, 1)
			PutBR(BR, 2)
			FixLink(y.a)
			Put(MOVI, y.r, 0, 0)
		}
		else if y.mode != Reg {
			load(&y)
		}
		if x.mode == Var
		{
			if x.lev == 0 {
				x.a = x.a - LONGINT(pc * 4)
			}
			Put(STW, y.r, x.r, x.a)
		}
		else { OSS.Mark("illegal assignment") }
		EXCL(&regs, x.r)
		EXCL(&regs, y.r)
	}
	else { OSS.Mark("incompatible assignment") }
}

public func Parameter(_ x: inout Item, _ ftyp: Type, _ `class`: INTEGER)
{
	var r: LONGINT = 0
	
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
					Put(ADDI, r, x.r, x.a)
				}
				else {
					r = x.r
				}
			}
			else { OSS.Mark("illegal parameter mode") }
			Put(PSH, r, SP, 4)
			EXCL(&regs, r)
		}
		else
		{
			/*value param*/
			if x.mode != Reg {
				load(&x)
			}
			Put(PSH, x.r, SP, 4)
			EXCL(&regs, x.r)
		}
	}
	else { OSS.Mark("bad parameter type") }
}

/*---------------------------------*/

public func CJump(_ x: inout Item)
{
	if x.type!.form == Boolean
	{
		if x.mode != Cond {
			loadBool(&x)
		}
		PutBR(BEQ + negated(x.c), x.a)
		EXCL(&regs, x.r)
		FixLink(x.b)
		x.a = LONGINT(pc - 1)
	}
	else
	{
		OSS.Mark("Boolean?")
		x.a = LONGINT(pc)
	}
}

public func BJump(_ L: LONGINT) {
	PutBR(BR, L - LONGINT(pc))
}

public func FJump(_ L: inout LONGINT)
{
	PutBR(BR, L)
	L = LONGINT(pc - 1)
}

public func Call(_ x: inout Item) {
	PutBR(BSR, x.a - LONGINT(pc))
}

public func IOCall(_ x: inout Item, _ y: inout Item)
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
		Put(RD, z.r, 0, 0)
		Store(&y, &z)
	}
	else if x.a == 2
	{
		load(&y)
		Put(WRD, 0, 0, y.r)
		EXCL(&regs, y.r)
	}
	else if x.a == 3
	{
		load(&y)
		Put(WRH, 0, 0, y.r)
		EXCL(&regs, y.r)
	}
	else {
		Put(WRL, 0, 0, 0)
	}
}

public func Header(_ size: LONGINT)
{
	entry = LONGINT(pc)
	Put(MOVI, SP, 0, RISC.MemSize - size)
	/*init SP*/
	Put(PSH, LNK, SP, 4)
}

public func Enter(_ size: LONGINT)
{
	Put(PSH, LNK, SP, 4)
	Put(PSH, FP, SP, 4)
	Put(MOV, FP, 0, SP)
	Put(SUBI, SP, SP, size)
}

public func Return(_ size: LONGINT)
{
	Put(MOV, SP, 0, FP)
	Put(POP, FP, SP, 4)
	Put(POP, LNK, SP, size + 4)
	PutBR(RET, LNK)
}

public func Open()
{
	curlev = 0
	pc = 0
	cno = 0
	regs = []
}

public func Close(_ S: inout Texts.Scanner, _ globals: LONGINT)
{
	Put(POP, LNK, SP, 4)
	PutBR(RET, LNK)
}

public func EnterCmd(_ name: inout ARRAY<CHAR>)
{
	COPY(name, &comname[cno])
	comadr[cno] = LONGINT(pc * 4)
	cno += 1
}

/*-------------------------------------------*/

public func Load(_ S: inout Texts.Scanner)
{
	RISC.Load(code, LONGINT(pc))
	Texts.WriteString(&W, " code loaded")
	Texts.WriteLn(&W)
	Texts.Append(OberonLog, &W.buf)
	RISC.Execute(entry * 4, &S, OberonLog)
}

public func Exec(_ S: inout Texts.Scanner)
{
	var i: INTEGER;
	
	i = 0
	while (i < cno) && (S.s != comname[i]) {
		i += 1
	}
	if i < cno {
		RISC.Execute(comadr[i], &S, OberonLog)
	}
}

public func Decode(_ T: inout Texts.Text)
{
	var i, op: LONGINT;
	
	Texts.WriteString(&W, "entry")
	Texts.WriteInt(&W, entry * 4, 6)
	Texts.WriteLn(&W)
	i = 0
	while i < pc
	{
		let w = UInt32(bitPattern: code[i])
		op = LONGINT(w / 0x4000000 % 0x40)
		Texts.WriteInt(&W, 4 * i, 4)
		Texts.Write(&W, 0x9)
		Texts.WriteString(&W, mnemo[op])
		
		var a: LONGINT
		if op < BEQ
		{
			a = LONGINT(bitPattern: w % 0x40000)
			if a >= 0x20000 {
				a -= 0x40000 /*sign extension*/
			}
			Texts.Write(&W, 0x9)
			Texts.WriteInt(&W, w / 0x400000 % 0x10, 4)
			Texts.Write(&W, ",")
			Texts.WriteInt(&W, w / 0x40000 % 0x10, 4)
			Texts.Write(&W, ",")
		}
		else
		{
			a = LONGINT(bitPattern: w % 0x4000000)
			if a >= 0x2000000 {
				a -= 0x4000000 /*sign extension*/
			}
		}
		Texts.WriteInt(&W, a, 6)
		Texts.WriteLn(&W)
		i += 1
	}
	Texts.WriteLn(&W)
	Texts.Append(T, &W.buf)
}

fileprivate func _moduleInit()
{
	Texts.OpenWriter(&W)
	mnemo[MOV] = "MOV "
	mnemo[MVN] = "MVN "
	mnemo[ADD] = "ADD "
	mnemo[SUB] = "SUB "
	mnemo[MUL] = "MUL "
	mnemo[Div] = "DIV "
	mnemo[Mod] = "MOD "
	mnemo[CMP] = "CMP "
	mnemo[MOVI] = "MOVI"
	mnemo[MVNI] = "MVNI"
	mnemo[ADDI] = "ADDI"
	mnemo[SUBI] = "SUBI"
	mnemo[MULI] = "MULI"
	mnemo[DIVI] = "DIVI"
	mnemo[MODI] = "MODI"
	mnemo[CMPI] = "CMPI"
	mnemo[CHKI] = "CHKI"
	mnemo[LDW] = "LDW "
	mnemo[LDB] = "LDB "
	mnemo[POP] = "POP "
	mnemo[STW] = "STW "
	mnemo[STB] = "STB "
	mnemo[PSH] = "PSH "
	mnemo[BEQ] = "BEQ "
	mnemo[BNE] = "BNE "
	mnemo[BLT] = "BLT "
	mnemo[BGE] = "BGE "
	mnemo[BLE] = "BLE "
	mnemo[BGT] = "BGT "
	mnemo[BR] = "BR "
	mnemo[BSR] = "BSR "
	mnemo[RET] = "RET "
	mnemo[RD] = "READ"
	mnemo[WRD] = "WRD "
	mnemo[WRH] = "WRH "
	mnemo[WRL] = "WRL "
}

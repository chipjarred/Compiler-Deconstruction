//
//  OSP.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Oberon
import Texts
import OSS
import OSG

internal let WordSize:LONGINT = 4

internal var sym: INTEGER = 0
internal var loaded: BOOLEAN = false

/* linked lists, end with guard */
internal var (topScope, universe) = makeTopScope()
internal var `guard`: OSG.Object = makeGuard()
internal var W = makeWriter()

internal func NewObj(_ obj: inout OSG.Object, _ class: INTEGER)
{
	var x = topScope
	`guard`!.name = OSS.id
	while x!.next!.name != OSS.id {
		x = x!.next
	}
	if x!.next == `guard`
	{
		var new: OSG.Object = nil
		NEW(&new)
		new!.name = OSS.id
		new!.class = `class`
		new!.next = `guard`
		x!.next = new
		obj = new
	}
	else
	{
		obj = x!.next
		OSS.Mark("mult def")
	}
}

func find(_ obj: inout OSG.Object)
{
	var s = topScope
	`guard`!.name = OSS.id
	while true
	{
		var x = s!.next
		while x!.name != OSS.id {
			x = x!.next
		}
		if x !== `guard`
		{
			obj = x
			break
		}
		if s === universe
		{
			obj = x
			OSS.Mark("undefined identifier: \(x!.name)")
			break
		}
		s = s!.dsc
	}
}

internal func FindField(_ obj: inout OSG.Object, _ list: OSG.Object)
{
	var list = list
	
	`guard`!.name = OSS.id
	while list!.name != OSS.id {
		list = list!.next
	}
	obj = list
}

internal func IsParam(_ obj: OSG.Object) -> Bool {
	return (obj!.class == OSG.Par) || (obj!.class == OSG.Var) && (obj!.val > 0)
}

internal func OpenScope(_ topScope: OSG.Object) -> OSG.Object
{
	var s: OSG.Object = nil
	NEW(&s)
	s!.class = OSG.Head
	s!.dsc = topScope
	s!.next = `guard`
	return s
}

internal func CloseScope(_ topScope: inout OSG.Object) {
	topScope = topScope!.dsc
}

/* -------------------- Parser ---------------------*/

internal func selector(_ x: inout OSG.Item)
{
	var y = OSG.Item()
	var obj: OSG.Object = nil

	while (sym == OSS.lbrak) || (sym == OSS.period)
	{
		if sym == OSS.lbrak
		{
			OSS.Get(&sym)
			expression(&y)
			
			if x.type!.form == OSG.Array {
				OSG.Index(&x, &y)
			}
			else { OSS.Mark("not an array") }
			
			if sym == OSS.rbrak {
				OSS.Get(&sym)
			}
			else { OSS.Mark("]?") }
		}
		else
		{
			OSS.Get(&sym)
			if sym == OSS.ident
			{
				if x.type!.form == OSG.Record
				{
					FindField(&obj, x.type!.fields)
					OSS.Get(&sym)
					if obj != `guard` {
						OSG.Field(&x, obj)
					}
					else { OSS.Mark("undef") }
				}
				else { OSS.Mark("not a record") }
			}
			else { OSS.Mark("ident?") }
		}
	}
}

internal func factor(_ x: inout OSG.Item)
{
	var obj: OSG.Object = nil
	
	/*sync*/
	if sym < OSS.lparen
	{
		OSS.Mark("ident?")
		repeat {
			OSS.Get(&sym)
		} while !(sym >= OSS.lparen)
	}

	if sym == OSS.ident
	{
		find(&obj)
		OSS.Get(&sym)
		OSG.MakeItem(&x, obj)
		selector(&x)
	}
	else if sym == OSS.number
	{
		OSG.MakeConstItem(&x, OSG.intType, OSS.val)
		OSS.Get(&sym)
	}
	else if sym == OSS.lparen
	{
		OSS.Get(&sym)
		expression(&x)
		if sym == OSS.rparen {
			OSS.Get(&sym)
		}
		else { OSS.Mark(")?") }
	}
	else if sym == OSS.not
	{
		OSS.Get(&sym)
		factor(&x)
		OSG.Op1(OSS.not, &x)
	}
	else
	{
		OSS.Mark("factor?")
		OSG.MakeItem(&x, `guard`)
	}
}

internal func term(_ x: inout OSG.Item)
{
	var y = OSG.Item()
	var op: INTEGER;
	
	factor(&x)
	while (sym >= OSS.times) && (sym <= OSS.and)
	{
		op = sym
		OSS.Get(&sym)
		if op == OSS.and {
			OSG.Op1(op, &x)
		}
		factor(&y)
		OSG.Op2(op, &x, &y)
	}
}

internal func SimpleExpression(_ x: inout OSG.Item)
{
	var y = OSG.Item()
	var op: INTEGER
	
	if sym == OSS.plus
	{
		OSS.Get(&sym)
		term(&x)
	}
	else if sym == OSS.minus
	{
		OSS.Get(&sym)
		term(&x)
		OSG.Op1(OSS.minus, &x)
	}
	else {
		term(&x)
	}
	while (sym >= OSS.plus) && (sym <= OSS.or)
	{
		op = sym
		OSS.Get(&sym)
		if op == OSS.or {
			OSG.Op1(op, &x)
		}
		term(&y)
		OSG.Op2(op, &x, &y)
	}
}

internal func expression(_ x: inout OSG.Item)
{
	var y = OSG.Item()
	var op: INTEGER
	
	SimpleExpression(&x)
	if (sym >= OSS.eql) && (sym <= OSS.gtr)
	{
		op = sym
		OSS.Get(&sym)
		SimpleExpression(&y)
		OSG.Relation(op, &x, &y)
	}
}

internal func parameter(_ fp: inout OSG.Object)
{
	var x = OSG.Item()
	
	expression(&x)
	if IsParam(fp)
	{
		OSG.Parameter(&x, fp!.type, fp!.class)
		fp = fp!.next
	}
	else { OSS.Mark("too many parameters") }
}

internal func StatSequence()
{
	var par, obj: OSG.Object
	var x = OSG.Item()
	var y = OSG.Item()
	var L: LONGINT

	func param(_ x: inout OSG.Item)
	{
		if sym == OSS.lparen {
			OSS.Get(&sym)
		}
		else { OSS.Mark(")?") }
		expression(&x)
		if sym == OSS.rparen {
			OSS.Get(&sym)
		}
		else { OSS.Mark(")?") }
	}

	while true /*sync*/
	{
		obj = `guard`
		
		if sym < OSS.ident
		{
			OSS.Mark("statement?")
			repeat {
				OSS.Get(&sym)
			} while !(sym >= OSS.ident)
		}
		if sym == OSS.ident
		{
			find(&obj)
			OSS.Get(&sym)
			OSG.MakeItem(&x, obj)
			selector(&x)
			
			if sym == OSS.becomes
			{
				OSS.Get(&sym)
				expression(&y)
				OSG.Store(&x, &y)
			}
			else if sym == OSS.eql
			{
				OSS.Mark(":= ?")
				OSS.Get(&sym)
				expression(&y)
			}
			else if x.mode == OSG.Proc
			{
				par = obj!.dsc
				if sym == OSS.lparen
				{
					OSS.Get(&sym)
					if sym == OSS.rparen {
						OSS.Get(&sym)
					}
					else
					{
						while true
						{
							parameter(&par)
							if sym == OSS.comma {
								OSS.Get(&sym)
							}
							else if sym == OSS.rparen
							{
								OSS.Get(&sym)
								break
							}
							else if sym >= OSS.semicolon {
								break
							}
							else { OSS.Mark(") or , ?") }
						}
					}
				}
				if obj!.val < 0 {
					OSS.Mark("forward call")
				}
				else if !IsParam(par) {
					OSG.Call(&x)
				}
				else { OSS.Mark("too few parameters") }
			}
			else if x.mode == OSG.SProc
			{
				if obj!.val <= 3 {
					param(&y)
				}
				OSG.IOCall(&x, &y)
			}
			else if obj!.class == OSG.Typ {
				OSS.Mark("illegal assignment?")
			}
			else { OSS.Mark("statement?") }
			
		}
		else if sym == OSS.if
		{
			OSS.Get(&sym)
			expression(&x)
			OSG.CJump(&x)
			if sym == OSS.then {
				OSS.Get(&sym)
			}
			else { OSS.Mark("THEN?") }
			StatSequence()
			L = 0
			while sym == OSS.elsif
			{
				OSS.Get(&sym)
				OSG.FJump(&L)
				OSG.FixLink(x.a)
				expression(&x)
				OSG.CJump(&x)
				if sym == OSS.then {
					OSS.Get(&sym)
				}
				else { OSS.Mark("THEN?") }
				StatSequence()
			}
			if sym == OSS.else
			{
				OSS.Get(&sym)
				OSG.FJump(&L)
				OSG.FixLink(x.a)
				StatSequence()
			}
			else { OSG.FixLink(x.a) }
			OSG.FixLink(L)
			if sym == OSS.end {
				OSS.Get(&sym)
			}
			else { OSS.Mark("END?") }
		}
		else if sym == OSS.while
		{
			OSS.Get(&sym)
			L = LONGINT(OSG.pc)
			expression(&x)
			OSG.CJump(&x)
			
			if sym == OSS.do {
				OSS.Get(&sym)
			}
			else { OSS.Mark("DO?") }
			StatSequence()
			OSG.BJump(L)
			OSG.FixLink(x.a)
			if sym == OSS.end {
				OSS.Get(&sym)
			}
			else { OSS.Mark("END?") }
		}
		if sym == OSS.semicolon {
			OSS.Get(&sym)
		}
		else if (sym >= OSS.semicolon) && (sym < OSS.if) || (sym >= OSS.array) {
			break
		}
		else { OSS.Mark("; ?") }
	}
}

internal func IdentList(_ class: INTEGER, _ first: inout OSG.Object)
{
	var obj: OSG.Object = nil
	
	if sym == OSS.ident
	{
		NewObj(&first, `class`)
		OSS.Get(&sym)
		while sym == OSS.comma
		{
			OSS.Get(&sym)
			if sym == OSS.ident
			{
				NewObj(&obj, `class`)
				OSS.Get(&sym)
			}
			else { OSS.Mark("ident?") }
		}
		if sym == OSS.colon {
			OSS.Get(&sym)
		}
		else { OSS.Mark(":?") }
	}
}

internal func Type(_ type: inout OSG.`Type`)
{
	var obj: OSG.Object = nil
	var first: OSG.Object = nil
	var x = OSG.Item()
	var tp: OSG.`Type` = nil

	type = OSG.intType /*sync*/
	if (sym != OSS.ident) && (sym < OSS.array)
	{
		OSS.Mark("type?")
		repeat {
			OSS.Get(&sym)
		} while !((sym == OSS.ident) || (sym >= OSS.array))
	}
	if sym == OSS.ident
	{
		find(&obj)
		OSS.Get(&sym)
		if obj!.class == OSG.Typ {
			type = obj!.type
		}
		else { OSS.Mark("type?") }
	}
	else if sym == OSS.array
	{
		OSS.Get(&sym)
		expression(&x)
		if (x.mode != OSG.Const) || (x.a < 0) {
			OSS.Mark("bad index")
		}
		if sym == OSS.of {
			OSS.Get(&sym)
		}
		else { OSS.Mark("OF?") }
		Type(&tp)
		NEW(&type)
		type!.form = OSG.Array
		type!.base = tp
		type!.len = SHORT(x.a)
		type!.size = type!.len * tp!.size
	}
	else if sym == OSS.record
	{
		OSS.Get(&sym)
		NEW(&type)
		type!.form = OSG.Record
		type!.size = 0
		topScope = OpenScope(topScope)
		while true
		{
			if sym == OSS.ident
			{
				IdentList(OSG.Fld, &first)
				Type(&tp)
				obj = first
				while obj != `guard`
				{
					obj!.type = tp
					obj!.val = LONGINT(type!.size)
					type!.size += obj!.type!.size
					obj = obj!.next
				}
			}
			if sym == OSS.semicolon {
				OSS.Get(&sym)
			}
			else if sym == OSS.ident {
				OSS.Mark("; ?")
			}
			else { break }
		}
		type!.fields = topScope!.next
		CloseScope(&topScope)
		if sym == OSS.end {
			OSS.Get(&sym)
		}
		else { OSS.Mark("END?") }
	}
	else { OSS.Mark("ident?") }
}

internal func declarations(_ varsize: inout LONGINT)
{
	var obj: OSG.Object = nil
	var first: OSG.Object = nil
	var x = OSG.Item()
	var tp: OSG.`Type` = nil
	
	/*sync*/
	if (sym < OSS.const) && (sym != OSS.end)
	{
		OSS.Mark("declaration?")
		repeat {
			OSS.Get(&sym)
		} while !((sym >= OSS.const) || (sym == OSS.end))
	}
	while true
	{
		if sym == OSS.const
		{
			OSS.Get(&sym)
			while sym == OSS.ident
			{
				NewObj(&obj, OSG.Const)
				OSS.Get(&sym)
				if sym == OSS.eql {
					OSS.Get(&sym)
				}
				else { OSS.Mark("=?") }
				expression(&x)
				if x.mode == OSG.Const
				{
					obj!.val = x.a
					obj!.type = x.type
				}
				else { OSS.Mark("expression not constant") }
				if sym == OSS.semicolon {
					OSS.Get(&sym)
				}
				else { OSS.Mark(";?") }
			}
		}
		if sym == OSS.type
		{
			OSS.Get(&sym)
			while sym == OSS.ident
			{
				NewObj(&obj, OSG.Typ)
				OSS.Get(&sym)
				if sym == OSS.eql {
					OSS.Get(&sym)
				}
				else { OSS.Mark("=?") }
				Type(&obj!.type)
				if sym == OSS.semicolon {
					OSS.Get(&sym)
				}
				else { OSS.Mark(";?") }
			}
		}
		if sym == OSS.var
		{
			OSS.Get(&sym)
			while sym == OSS.ident
			{
				IdentList(OSG.Var, &first)
				Type(&tp)
				obj = first
				while obj != `guard`
				{
					obj!.type = tp
					obj!.lev = OSG.curlev
					varsize = varsize + LONGINT( obj!.type!.size)
					obj!.val = -varsize
					obj = obj!.next
				}
				if sym == OSS.semicolon {
					OSS.Get(&sym)
				}
				else { OSS.Mark("; ?") }
			}
		}
		if (sym >= OSS.const) && (sym <= OSS.var) {
			OSS.Mark("declaration?")
		}
		else { break }
	}
}

internal func ProcedureDecl()
{
	let marksize: LONGINT = 8
	var proc: OSG.Object = nil
	var obj: OSG.Object = nil
	var procid: OSS.Ident
	var locblksize, parblksize: LONGINT
	
	func FPSection()
	{
		var obj: OSG.Object = nil
		var first: OSG.Object = nil
		var tp: OSG.`Type`
		var parsize: LONGINT
		
		if sym == OSS.var
		{
			OSS.Get(&sym)
			IdentList(OSG.Par, &first)
		}
		else {
			IdentList(OSG.Var, &first)
		}
		if sym == OSS.ident
		{
			find(&obj)
			OSS.Get(&sym)
			if obj!.class == OSG.Typ {
				tp = obj!.type
			}
			else
			{
				OSS.Mark("ident?")
				tp = OSG.intType
			}
		}
		else
		{
			OSS.Mark("ident?")
			tp = OSG.intType
		}
		if first!.class == OSG.Var
		{
			parsize = LONGINT(tp!.size)
			if tp!.form >= OSG.Array {
				OSS.Mark("no struct params")
			}
		}
		else {
			parsize = WordSize
		}
		obj = first
		while obj != `guard`
		{
			obj!.type = tp
			parblksize += parsize
			obj = obj!.next
		}
	}

	/* ProcedureDecl */
	OSS.Get(&sym)
	if sym == OSS.ident
	{
		procid = OSS.id
		NewObj(&proc, OSG.Proc)
		OSS.Get(&sym)
		parblksize = marksize
		OSG.IncLevel(1)
		topScope = OpenScope(topScope)
		proc!.val = -1
		if sym == OSS.lparen
		{
			OSS.Get(&sym)
			if sym == OSS.rparen {
				OSS.Get(&sym)
			}
			else
			{
				FPSection()
				while sym == OSS.semicolon
				{
					OSS.Get(&sym)
					FPSection()
				}
				if sym == OSS.rparen {
					OSS.Get(&sym)
				}
				else { OSS.Mark(")?") }
			}
		}
		else if OSG.curlev == 1 {
			OSG.EnterCmd(&procid)
		}
		obj = topScope!.next
		locblksize = parblksize
		while obj != `guard`
		{
			obj!.lev = OSG.curlev
			if obj!.class == OSG.Par {
				locblksize -= WordSize
			}
			else
			{
				obj!.val = locblksize
				obj = obj!.next
			}
		}
		proc!.dsc = topScope!.next
		if sym == OSS.semicolon {
			OSS.Get(&sym)
		}
		else { OSS.Mark(";?") }
		locblksize = 0
		declarations(&locblksize)
		while sym == OSS.procedure
		{
			ProcedureDecl()
			if sym == OSS.semicolon {
				OSS.Get(&sym)
			}
			else { OSS.Mark(";?") }
		}
		proc!.val = LONGINT(OSG.pc)
		OSG.Enter(locblksize)
		if sym == OSS.begin
		{
			OSS.Get(&sym)
			StatSequence()
		}
		if sym == OSS.end {
			OSS.Get(&sym)
		}
		else { OSS.Mark("END?") }
		if sym == OSS.ident
		{
			if procid != OSS.id {
				OSS.Mark("no match")
			}
			OSS.Get(&sym)
		}
		OSG.Return(parblksize - marksize)
		CloseScope(&topScope)
		OSG.IncLevel(-1)
	}
}

internal func Module(_ S: inout Texts.Scanner)
{
	var modid = OSS.makeIdent()
	var varsize: LONGINT

	Texts.WriteString(&W, " compiling ")
	if sym == OSS.module
	{
		OSS.Get(&sym)
		OSG.Open()
		topScope = OpenScope(topScope)
		varsize = 0
		if sym == OSS.ident
		{
			modid = OSS.id
			OSS.Get(&sym)
			Texts.WriteString(&W, modid)
			Texts.WriteLn(&W)
			Texts.Append(OberonLog, &W.buf)
		}
		else { OSS.Mark("ident?") }
		if sym == OSS.semicolon {
			OSS.Get(&sym)
		}
		else { OSS.Mark(";?") }
		declarations(&varsize)
		while sym == OSS.procedure
		{
			ProcedureDecl()
			if sym == OSS.semicolon {
				OSS.Get(&sym)
			}
			else { OSS.Mark(";?") }
		}
		OSG.Header(varsize)
		if sym == OSS.begin
		{
			OSS.Get(&sym)
			StatSequence()
		}
		if sym == OSS.end {
			OSS.Get(&sym)
		}
		else { OSS.Mark("END?") }
		if sym == OSS.ident
		{
			if modid != OSS.id {
				OSS.Mark("no match")
			}
			OSS.Get(&sym)
		}
		else { OSS.Mark("ident?") }
		if sym != OSS.period {
			OSS.Mark(". ?")
		}
		CloseScope(&topScope)
		if !OSS.error
		{
			COPY(modid, &S.s)
			OSG.Close(&S, varsize)
			Texts.WriteString(&W, "code generated")
			Texts.WriteInt(&W, LONGINT(OSG.pc), 6)
			Texts.WriteLn(&W)
			Texts.Append(OberonLog, &W.buf)
		}
	}
	else { OSS.Mark("MODULE?") }
}

public func Compile()
{
	#if false
	var beg, end, time: LONGINT
	var S: Texts.Scanner
	var T: Texts.Text
	var v: Viewers.Viewer

	loaded = false
	
	Texts.OpenScanner(&S, Oberon.Par.-text, Oberon.Par.pos)
	Texts.Scan(&S)
	if S.class == Texts.Char
	{
		if S.c == "*"
		{
			v = Oberon.MarkedViewer()
			if v.dsc != nil && (v.dsc.next is TextFrames.Frame)
			{
				OSS.Init(v.dsc.next(TextFrames.Frame).text, 0)
				OSS.Get(&sym);
				Module(&S)
			}
		}
		else if S.c == "@"
		{
			Oberon.GetSelection(T, beg, end, time)
			if time >= 0
			{
				OSS.Init(T, beg)
				OSS.Get(&sym)
				Module(&S)
			}
		}
	}
	else if S.class == Texts.Name
	{
		NEW(&T)
		Texts.Open(T, S.s)
		OSS.Init(T, 0)
		OSS.Get(&sym)
		Module(&S)
	}
	#else
	fatalError("Not running on actual Oberon system.")
	#endif
}

public func Decode()
{
	#if false
	var V: MenuViewers.Viewer
	var T: Texts.Text
	var X, Y: INTEGER
	
	T = TextFrames.Text("")
	Oberon.AllocateSystemViewer(Oberon.Par.frame.X, X, Y)
	V = MenuViewers.New(
		TextFrames.NewMenu(
			"Log.Text",
			"System.Close System.Copy System.Grow Edit.Search Edit.Store"
		),
		TextFrames.NewText(T, 0),
		TextFrames.menuH, X, Y
	)
	OSG.Decode(&T)
	#else
	fatalError("Not running on actual Oberon system.")
	#endif
}

public func Load()
{
	#if false
	var S: Texts.Scanner
	if !OSS.error && !loaded
	{
		Texts.OpenScanner(&S, Oberon.Par.text, Oberon.Par.pos)
		OSG.Load(&S)
		loaded = true
	}
	#else
	fatalError("Not running on actual Oberon system.")
	#endif
}

public func Exec()
{
	#if false
	var S: Texts.Scanner
	
	if loaded
	{
		Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos)
		Texts.Scan(&S)
		if S.class == Texts.Name {
			OSG.Exec(&S)
		}
	}
	#else
	fatalError("Not running on actual Oberon system.")
	#endif
}

fileprivate func enter(
	_ cl: INTEGER,
	_ n: LONGINT,
	_ name: OSS.Ident,
	_ type: OSG.`Type`,
	in topScope: inout OSG.Object)
{
	var obj: OSG.Object = nil
	
	NEW(&obj)
	obj!.class = cl
	obj!.val = n
	obj!.name = name
	obj!.type = type
	obj!.dsc = nil
	obj!.next = topScope!.next
	topScope!.next = obj
}

fileprivate func makeWriter() -> Texts.Writer
{
	var W = Texts.Writer()
	
	Texts.OpenWriter(&W)
	Texts.WriteString(&W, "Oberon0 Compiler 9.2.95")
	Texts.WriteLn(&W)
	Texts.Append(OberonLog, &W.buf)
	
	return W
}

fileprivate func makeGuard() -> OSG.Object
{
	var `guard`: OSG.Object = nil
	NEW(&`guard`)
	`guard`!.class = OSG.Var
	`guard`!.type = OSG.intType
	`guard`!.val = 0
	
	return `guard`
}

fileprivate func makeTopScope() -> (OSG.Object, OSG.Object)
{
	var topScope = OpenScope(nil)
	let universe = topScope
	
	enter(OSG.Typ, 1, "BOOLEAN", OSG.boolType, in: &topScope)
	enter(OSG.Typ, 2, "INTEGER", OSG.intType, in: &topScope)
	enter(OSG.Const, 1, "TRUE", OSG.boolType, in: &topScope)
	enter(OSG.Const, 0, "FALSE", OSG.boolType, in: &topScope)
	enter(OSG.SProc, 1, "Read", nil, in: &topScope)
	enter(OSG.SProc, 2, "Write", nil, in: &topScope)
	enter(OSG.SProc, 3, "WriteHex", nil, in: &topScope)
	enter(OSG.SProc, 4, "WriteLn", nil, in: &topScope)
	
	return (topScope, universe)
}

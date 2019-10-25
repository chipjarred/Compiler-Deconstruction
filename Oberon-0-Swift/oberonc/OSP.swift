//
//  OSP.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

public struct OSP
{
	internal static let WordSize:Int = 4

	internal static var sym: Int = 0
	internal static var loaded: Bool = false

	/* linked lists, end with guard */
	internal static var (topScope, universe) = makeTopScope()
	internal static var `guard`: OSG.Object = makeGuard()
	internal static var W = makeWriter()

	internal static func NewObj(_ obj: inout OSG.Object, _ class: Int)
	{
		var x = topScope
		`guard`!.name = OSS.id
		while x!.next!.name != OSS.id {
			x = x!.next
		}
		if x!.next == `guard`
		{
			let new: OSG.Object = OSG.ObjDesc()
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

	internal static func find(_ obj: inout OSG.Object)
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

	internal static func FindField(_ obj: inout OSG.Object, _ list: OSG.Object)
	{
		var list = list
		
		`guard`!.name = OSS.id
		while list!.name != OSS.id {
			list = list!.next
		}
		obj = list
	}

	internal static func IsParam(_ obj: OSG.Object) -> Bool {
		return (obj!.class == OSG.Par) || (obj!.class == OSG.Var) && (obj!.val > 0)
	}

	internal static func OpenScope(_ topScope: OSG.Object) -> OSG.Object
	{
		let s:OSG.Object = OSG.ObjDesc()
		s!.class = OSG.Head
		s!.dsc = topScope
		s!.next = `guard`
		return s
	}

	internal static func CloseScope(_ topScope: inout OSG.Object) {
		topScope = topScope!.dsc
	}

	/* -------------------- Parser ---------------------*/

	internal static func selector(_ x: inout OSG.Item)
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

	internal static func factor(_ x: inout OSG.Item)
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

	internal static func term(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: Int;
		
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

	internal static func SimpleExpression(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: Int
		
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

	internal static func expression(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: Int
		
		SimpleExpression(&x)
		if (sym >= OSS.eql) && (sym <= OSS.gtr)
		{
			op = sym
			OSS.Get(&sym)
			SimpleExpression(&y)
			OSG.Relation(op, &x, &y)
		}
	}

	internal static func parameter(_ fp: inout OSG.Object)
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

	internal static func StatSequence()
	{
		var par, obj: OSG.Object
		var x = OSG.Item()
		var y = OSG.Item()
		var L: Int

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
				L = Int(OSG.pc)
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

	internal static func IdentList(_ class: Int, _ first: inout OSG.Object)
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

	internal static func Type(_ type: inout OSG.`Type`)
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
			type = OSG.TypeDesc()
			type!.form = OSG.Array
			type!.base = tp
			type!.len = x.a
			type!.size = type!.len * tp!.size
		}
		else if sym == OSS.record
		{
			OSS.Get(&sym)
			type = OSG.TypeDesc()
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
						obj!.val = Int(type!.size)
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

	internal static func declarations(_ varsize: inout Int)
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
						varsize = varsize + Int( obj!.type!.size)
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

	internal static func ProcedureDecl()
	{
		let marksize: Int = 8
		var proc: OSG.Object = nil
		var obj: OSG.Object = nil
		var procid: OSS.Ident
		var locblksize, parblksize: Int
		
		func FPSection()
		{
			var obj: OSG.Object = nil
			var first: OSG.Object = nil
			var tp: OSG.`Type`
			var parsize: Int
			
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
				parsize = Int(tp!.size)
				if tp!.form >= OSG.Array {
					OSS.Mark("no struct params")
				}
			}
			else {
				parsize = WordSize
			}
			obj = first
			while obj !== `guard`
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
				else {
					locblksize -= Int(obj!.type!.size)
				}
				obj!.val = locblksize
				obj = obj!.next
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
			proc!.val = Int(OSG.pc)
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

	internal static func Module(_ S: inout Texts.Scanner)
	{
		var modid = ""
		var varsize: Int

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
				S.s = modid
				OSG.Close(&S, varsize)
				Texts.WriteString(&W, "code generated")
				Texts.WriteInt(&W, Int(OSG.pc), 6)
				Texts.WriteLn(&W)
				Texts.Append(OberonLog, &W.buf)
			}
		}
		else { OSS.Mark("MODULE?") }
	}

	static func Compile()
	{
		#if false
		var beg, end, time: Int
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

	// ---------------------------------------------------
	fileprivate static func enter(
		_ cl: Int,
		_ n: Int,
		_ name: OSS.Ident,
		_ type: OSG.`Type`,
		in topScope: inout OSG.Object)
	{
		let obj: OSG.Object = OSG.ObjDesc()
		obj!.class = cl
		obj!.val = n
		obj!.name = name
		obj!.type = type
		obj!.dsc = nil
		obj!.next = topScope!.next
		topScope!.next = obj
	}

	fileprivate static func makeWriter() -> Texts.Writer
	{
		var W = Texts.Writer()
		
		Texts.OpenWriter(&W)
		Texts.WriteString(&W, "Oberon0 Compiler 9.2.95")
		Texts.WriteLn(&W)
		Texts.Append(OberonLog, &W.buf)
		
		return W
	}

	fileprivate static func makeGuard() -> OSG.Object
	{
		let `guard`: OSG.Object = OSG.ObjDesc()
		`guard`!.class = OSG.Var
		`guard`!.type = OSG.intType
		`guard`!.val = 0
		
		return `guard`
	}

	fileprivate static func makeTopScope() -> (OSG.Object, OSG.Object)
	{
		var topScope = OpenScope(nil)
		let universe = topScope
		
		enter(OSG.Typ, 1, "Bool", OSG.boolType, in: &topScope)
		enter(OSG.Typ, 2, "Int", OSG.intType, in: &topScope)
		enter(OSG.Const, 1, "TRUE", OSG.boolType, in: &topScope)
		enter(OSG.Const, 0, "FALSE", OSG.boolType, in: &topScope)
		enter(OSG.SProc, 1, "Read", nil, in: &topScope)
		enter(OSG.SProc, 2, "Write", nil, in: &topScope)
		enter(OSG.SProc, 3, "WriteHex", nil, in: &topScope)
		enter(OSG.SProc, 4, "WriteLn", nil, in: &topScope)
		
		return (topScope, universe)
	}
}


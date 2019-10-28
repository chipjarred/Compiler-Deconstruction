//
//  OSP.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
fileprivate func printLog()
{
	print(
		"\n\n ---- Oberon Log ----\n"
		+ "\(OberonLog.isEmpty ? "-- EMPTY LOG --" : OberonLog.description)"
	)
	OberonLog = ""
}

// ---------------------------------------------------
public struct OSP
{
	internal static let WordSize:Int = 4

	internal static var sym: OberonSymbol = .null
	internal static var loaded: Bool = false

	/* linked lists, end with guard */
	internal static var (topScope, universe) = makeTopScope()
	internal static var `guard`: OSG.Object = makeGuard()

	// ---------------------------------------------------
	internal static func newObj(_ obj: inout OSG.Object, _ class: Int)
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

	// ---------------------------------------------------
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

	// ---------------------------------------------------
	internal static func findField(_ obj: inout OSG.Object, _ list: OSG.Object)
	{
		var list = list
		
		`guard`!.name = OSS.id
		while list!.name != OSS.id {
			list = list!.next
		}
		obj = list
	}

	// ---------------------------------------------------
	internal static func isParameter(_ obj: OSG.Object) -> Bool {
		return (obj!.class == OSG.Par) || (obj!.class == OSG.Var) && (obj!.val > 0)
	}

	// ---------------------------------------------------
	internal static func openScope(_ topScope: OSG.Object) -> OSG.Object
	{
		let s:OSG.Object = OSG.ObjDesc()
		s!.class = OSG.Head
		s!.dsc = topScope
		s!.next = `guard`
		return s
	}

	// ---------------------------------------------------
	internal static func closeScope(_ topScope: inout OSG.Object) {
		topScope = topScope!.dsc
	}

	// MARK:- Parser
	// ---------------------------------------------------
	internal static func selector(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var obj: OSG.Object = nil

		while (sym == .lbrak) || (sym == .period)
		{
			if sym == .lbrak
			{
				OSS.Get(&sym)
				parseExpression(&y)
				
				if x.type!.form == OSG.Array {
					OSG.Index(&x, &y)
				}
				else { OSS.Mark("not an array") }
				
				if sym == .rbrak {
					OSS.Get(&sym)
				}
				else { OSS.Mark("]?") }
			}
			else
			{
				OSS.Get(&sym)
				if sym == .ident
				{
					if x.type!.form == OSG.Record
					{
						findField(&obj, x.type!.fields)
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

	// ---------------------------------------------------
	internal static func factor(_ x: inout OSG.Item)
	{
		var obj: OSG.Object = nil
		
		// sync
		if sym < .lparen
		{
			OSS.Mark("ident?")
			repeat {
				OSS.Get(&sym)
			} while !(sym >= .lparen)
		}

		if sym == .ident
		{
			find(&obj)
			OSS.Get(&sym)
			OSG.MakeItem(&x, obj)
			selector(&x)
		}
		else if sym == .number
		{
			OSG.MakeConstItem(&x, OSG.intType, OSS.val)
			OSS.Get(&sym)
		}
		else if sym == .lparen
		{
			OSS.Get(&sym)
			parseExpression(&x)
			if sym == .rparen {
				OSS.Get(&sym)
			}
			else { OSS.Mark(")?") }
		}
		else if sym == .not
		{
			OSS.Get(&sym)
			factor(&x)
			OSG.Op1(.not, &x)
		}
		else
		{
			OSS.Mark("factor?")
			OSG.MakeItem(&x, `guard`)
		}
	}

	// ---------------------------------------------------
	internal static func parseTerminalSymbol(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: OberonSymbol;
		
		factor(&x)
		while (sym >= .times) && (sym <= .and)
		{
			op = sym
			OSS.Get(&sym)
			if op == .and {
				OSG.Op1(op, &x)
			}
			factor(&y)
			OSG.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseSimpleExpression(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: OberonSymbol
		
		if sym == .plus
		{
			OSS.Get(&sym)
			parseTerminalSymbol(&x)
		}
		else if sym == .minus
		{
			OSS.Get(&sym)
			parseTerminalSymbol(&x)
			OSG.Op1(.minus, &x)
		}
		else {
			parseTerminalSymbol(&x)
		}
		while (sym >= .plus) && (sym <= .or)
		{
			op = sym
			OSS.Get(&sym)
			if op == .or {
				OSG.Op1(op, &x)
			}
			parseTerminalSymbol(&y)
			OSG.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseExpression(_ x: inout OSG.Item)
	{
		var y = OSG.Item()
		var op: OberonSymbol
		
		parseSimpleExpression(&x)
		if (sym >= .eql) && (sym <= .gtr)
		{
			op = sym
			OSS.Get(&sym)
			parseSimpleExpression(&y)
			OSG.Relation(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseParameter(_ fp: inout OSG.Object)
	{
		var x = OSG.Item()
		
		parseExpression(&x)
		if isParameter(fp)
		{
			OSG.Parameter(&x, fp!.type, fp!.class)
			fp = fp!.next
		}
		else { OSS.Mark("too many parameters") }
	}

	// ---------------------------------------------------
	internal static func parseStatementSequence()
	{
		var par, obj: OSG.Object
		var x = OSG.Item()
		var y = OSG.Item()
		var L: Int

		func param(_ x: inout OSG.Item)
		{
			if sym == .lparen {
				OSS.Get(&sym)
			}
			else { OSS.Mark(")?") }
			parseExpression(&x)
			if sym == .rparen {
				OSS.Get(&sym)
			}
			else { OSS.Mark(")?") }
		}

		while true /*sync*/
		{
			obj = `guard`
			
			if sym < .ident
			{
				OSS.Mark("statement?")
				repeat {
					OSS.Get(&sym)
				} while !(sym >= .ident)
			}
			if sym == .ident
			{
				find(&obj)
				OSS.Get(&sym)
				OSG.MakeItem(&x, obj)
				selector(&x)
				
				if sym == .becomes
				{
					OSS.Get(&sym)
					parseExpression(&y)
					OSG.Store(&x, &y)
				}
				else if sym == .eql
				{
					OSS.Mark(":= ?")
					OSS.Get(&sym)
					parseExpression(&y)
				}
				else if x.mode == OSG.Proc
				{
					par = obj!.dsc
					if sym == .lparen
					{
						OSS.Get(&sym)
						if sym == .rparen {
							OSS.Get(&sym)
						}
						else
						{
							while true
							{
								parseParameter(&par)
								if sym == .comma {
									OSS.Get(&sym)
								}
								else if sym == .rparen
								{
									OSS.Get(&sym)
									break
								}
								else if sym >= .semicolon {
									break
								}
								else { OSS.Mark(") or , ?") }
							}
						}
					}
					if obj!.val < 0 {
						OSS.Mark("forward call")
					}
					else if !isParameter(par) {
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
			else if sym == .if
			{
				OSS.Get(&sym)
				parseExpression(&x)
				OSG.CJump(&x)
				if sym == .then {
					OSS.Get(&sym)
				}
				else { OSS.Mark("THEN?") }
				parseStatementSequence()
				L = 0
				while sym == .elsif
				{
					OSS.Get(&sym)
					OSG.FJump(&L)
					OSG.FixLink(x.a)
					parseExpression(&x)
					OSG.CJump(&x)
					if sym == .then {
						OSS.Get(&sym)
					}
					else { OSS.Mark("THEN?") }
					parseStatementSequence()
				}
				if sym == .else
				{
					OSS.Get(&sym)
					OSG.FJump(&L)
					OSG.FixLink(x.a)
					parseStatementSequence()
				}
				else { OSG.FixLink(x.a) }
				OSG.FixLink(L)
				if sym == .end {
					OSS.Get(&sym)
				}
				else { OSS.Mark("END?") }
			}
			else if sym == .while
			{
				OSS.Get(&sym)
				L = Int(OSG.pc)
				parseExpression(&x)
				OSG.CJump(&x)
				
				if sym == .do {
					OSS.Get(&sym)
				}
				else { OSS.Mark("DO?") }
				parseStatementSequence()
				OSG.BJump(L)
				OSG.FixLink(x.a)
				if sym == .end {
					OSS.Get(&sym)
				}
				else { OSS.Mark("END?") }
			}
			if sym == .semicolon {
				OSS.Get(&sym)
			}
			else if (sym >= .semicolon) && (sym < .if) || (sym >= .array) {
				break
			}
			else { OSS.Mark("; ?") }
		}
	}

	// ---------------------------------------------------
	internal static func parseIdentifierList(
		_ class: Int,
		_ first: inout OSG.Object)
	{
		var obj: OSG.Object = nil
		
		if sym == .ident
		{
			newObj(&first, `class`)
			OSS.Get(&sym)
			while sym == .comma
			{
				OSS.Get(&sym)
				if sym == .ident
				{
					newObj(&obj, `class`)
					OSS.Get(&sym)
				}
				else { OSS.Mark("ident?") }
			}
			if sym == .colon {
				OSS.Get(&sym)
			}
			else { OSS.Mark(":?") }
		}
	}

	// ---------------------------------------------------
	internal static func parseType(_ type: inout OSG.`Type`)
	{
		var obj: OSG.Object = nil
		var first: OSG.Object = nil
		var x = OSG.Item()
		var tp: OSG.`Type` = nil

		type = OSG.intType /*sync*/
		if (sym != .ident) && (sym < .array)
		{
			OSS.Mark("type?")
			repeat {
				OSS.Get(&sym)
			} while !((sym == .ident) || (sym >= .array))
		}
		if sym == .ident
		{
			find(&obj)
			OSS.Get(&sym)
			if obj!.class == OSG.Typ {
				type = obj!.type
			}
			else { OSS.Mark("type?") }
		}
		else if sym == .array
		{
			OSS.Get(&sym)
			parseExpression(&x)
			if (x.mode != OSG.Const) || (x.a < 0) {
				OSS.Mark("bad index")
			}
			if sym == .of {
				OSS.Get(&sym)
			}
			else { OSS.Mark("OF?") }
			parseType(&tp)
			type = OSG.TypeDesc()
			type!.form = OSG.Array
			type!.base = tp
			type!.len = x.a
			type!.size = type!.len * tp!.size
		}
		else if sym == .record
		{
			OSS.Get(&sym)
			type = OSG.TypeDesc()
			type!.form = OSG.Record
			type!.size = 0
			topScope = openScope(topScope)
			while true
			{
				if sym == .ident
				{
					parseIdentifierList(OSG.Fld, &first)
					parseType(&tp)
					obj = first
					while obj != `guard`
					{
						obj!.type = tp
						obj!.val = Int(type!.size)
						type!.size += obj!.type!.size
						obj = obj!.next
					}
				}
				if sym == .semicolon {
					OSS.Get(&sym)
				}
				else if sym == .ident {
					OSS.Mark("; ?")
				}
				else { break }
			}
			type!.fields = topScope!.next
			closeScope(&topScope)
			if sym == .end {
				OSS.Get(&sym)
			}
			else { OSS.Mark("END?") }
		}
		else { OSS.Mark("ident?") }
	}

	// ---------------------------------------------------
	internal static func parseDeclarations(_ varsize: inout Int)
	{
		var obj: OSG.Object = nil
		var first: OSG.Object = nil
		var x = OSG.Item()
		var tp: OSG.`Type` = nil
		
		/*sync*/
		if (sym < .const) && (sym != .end)
		{
			OSS.Mark("declaration?")
			repeat {
				OSS.Get(&sym)
			} while !((sym >= .const) || (sym == .end))
		}
		while true
		{
			if sym == .const
			{
				OSS.Get(&sym)
				while sym == .ident
				{
					newObj(&obj, OSG.Const)
					OSS.Get(&sym)
					if sym == .eql {
						OSS.Get(&sym)
					}
					else { OSS.Mark("=?") }
					parseExpression(&x)
					if x.mode == OSG.Const
					{
						obj!.val = x.a
						obj!.type = x.type
					}
					else { OSS.Mark("expression not constant") }
					if sym == .semicolon {
						OSS.Get(&sym)
					}
					else { OSS.Mark(";?") }
				}
			}
			if sym == .type
			{
				OSS.Get(&sym)
				while sym == .ident
				{
					newObj(&obj, OSG.Typ)
					OSS.Get(&sym)
					if sym == .eql {
						OSS.Get(&sym)
					}
					else { OSS.Mark("=?") }
					parseType(&obj!.type)
					if sym == .semicolon {
						OSS.Get(&sym)
					}
					else { OSS.Mark(";?") }
				}
			}
			if sym == .var
			{
				OSS.Get(&sym)
				while sym == .ident
				{
					parseIdentifierList(OSG.Var, &first)
					parseType(&tp)
					obj = first
					while obj != `guard`
					{
						obj!.type = tp
						obj!.lev = OSG.curlev
						varsize = varsize + Int( obj!.type!.size)
						obj!.val = -varsize
						obj = obj!.next
					}
					if sym == .semicolon {
						OSS.Get(&sym)
					}
					else { OSS.Mark("; ?") }
				}
			}
			if (sym >= .const) && (sym <= .var) {
				OSS.Mark("declaration?")
			}
			else { break }
		}
	}

	// ---------------------------------------------------
	internal static func parseProcedureDeclaration()
	{
		let marksize: Int = 8
		var proc: OSG.Object = nil
		var obj: OSG.Object = nil
		var procid: String
		var locblksize, parblksize: Int
		
		func FPSection()
		{
			var obj: OSG.Object = nil
			var first: OSG.Object = nil
			var tp: OSG.`Type`
			var parsize: Int
			
			if sym == .var
			{
				OSS.Get(&sym)
				parseIdentifierList(OSG.Par, &first)
			}
			else {
				parseIdentifierList(OSG.Var, &first)
			}
			if sym == .ident
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

		// ProcedureDecl
		OSS.Get(&sym)
		if sym == .ident
		{
			procid = OSS.id
			newObj(&proc, OSG.Proc)
			OSS.Get(&sym)
			parblksize = marksize
			OSG.IncLevel(1)
			topScope = openScope(topScope)
			proc!.val = -1
			if sym == .lparen
			{
				OSS.Get(&sym)
				if sym == .rparen {
					OSS.Get(&sym)
				}
				else
				{
					FPSection()
					while sym == .semicolon
					{
						OSS.Get(&sym)
						FPSection()
					}
					if sym == .rparen {
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
			if sym == .semicolon {
				OSS.Get(&sym)
			}
			else { OSS.Mark(";?") }
			locblksize = 0
			parseDeclarations(&locblksize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					OSS.Get(&sym)
				}
				else { OSS.Mark(";?") }
			}
			proc!.val = Int(OSG.pc)
			OSG.Enter(locblksize)
			if sym == .begin
			{
				OSS.Get(&sym)
				parseStatementSequence()
			}
			if sym == .end {
				OSS.Get(&sym)
			}
			else { OSS.Mark("END?") }
			if sym == .ident
			{
				if procid != OSS.id {
					OSS.Mark("no match")
				}
				OSS.Get(&sym)
			}
			OSG.Return(parblksize - marksize)
			closeScope(&topScope)
			OSG.IncLevel(-1)
		}
	}

	// ---------------------------------------------------
	internal static func parseModule()
	{
		var modid = ""
		var varsize: Int

		print(" compiling ", terminator: "", to: &OberonLog)
		if sym == .module
		{
			OSS.Get(&sym)
			OSG.Open()
			topScope = openScope(topScope)
			varsize = 0
			if sym == .ident
			{
				modid = OSS.id
				OSS.Get(&sym)
				print("\(modid)", to: &OberonLog)
			}
			else { OSS.Mark("ident?") }
			if sym == .semicolon {
				OSS.Get(&sym)
			}
			else { OSS.Mark(";?") }
			parseDeclarations(&varsize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					OSS.Get(&sym)
				}
				else { OSS.Mark(";?") }
			}
			OSG.Header(varsize)
			if sym == .begin
			{
				OSS.Get(&sym)
				parseStatementSequence()
			}
			if sym == .end {
				OSS.Get(&sym)
			}
			else { OSS.Mark("END?") }
			if sym == .ident
			{
				if modid != OSS.id {
					OSS.Mark("no match")
				}
				OSS.Get(&sym)
			}
			else { OSS.Mark("ident?") }
			if sym != .period {
				OSS.Mark(". ?")
			}
			closeScope(&topScope)
			if !OSS.error
			{
				OSG.Close()
				print("code generated\(OSG.pc, pad: 6)", to: &OberonLog)
			}
		}
		else { OSS.Mark("MODULE?") }
	}

	// MARK:- Public Interface
	// ---------------------------------------------------
	/// Program signature/marker
	static var magic: UInt32 {
		return UInt32(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	/**
	Returns the compiled program
	*/
	static var program: [UInt32]
	{
		let objCode = OSG.getObjectCode()
		var program = [UInt32]()
		program.reserveCapacity(objCode.count + 2)
		program.append(OSP.magic)
		program.append(UInt32(OSG.entry * 4))
		program.append(contentsOf: objCode)
		return program
	}

	// ---------------------------------------------------
	/**
	Compile Oberon-0 code from a `String`
	*/
	static func compile(source: String)
	{
		defer { printLog() }
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		OSS.Init(sourceStream: sourceStream)
		OSS.Get(&sym)
		parseModule()
	}

	// ---------------------------------------------------
	/**
	Disassemble a compiled Oberon-0 program to a `String`
	*/
	static func disassemble() -> String
	{
		defer { printLog() }
		var result = ""
		OSG.decode(to: &result)
		return result.isEmpty ? "!!!!! NO OUTPUT !!!!!" : result.description
	}

	// MARK:- Support functions
	// ---------------------------------------------------
	fileprivate static func enter(
		_ cl: Int,
		_ n: Int,
		_ name: String,
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
		var topScope = openScope(nil)
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


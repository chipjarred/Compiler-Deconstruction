//
//  Oberon0Parser.swift
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
public struct Oberon0Parser
{
	internal static let WordSize:Int = 4

	internal static var sym: OberonSymbol = .null
	internal static var loaded: Bool = false

	/* linked lists, end with guard */
	internal static var (topScope, universe) = makeTopScope()
	internal static var `guard`: RISCCodeGenerator.Object = makeGuard()

	// ---------------------------------------------------
	internal static func newObj(_ obj: inout RISCCodeGenerator.Object, _ kind: Int)
	{
		var x = topScope
		`guard`!.name = Oberon0Lexer.id
		while x!.next!.name != Oberon0Lexer.id {
			x = x!.next
		}
		if x!.next == `guard`
		{
			let new: RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
			new!.name = Oberon0Lexer.id
			new!.kind = kind
			new!.next = `guard`
			x!.next = new
			obj = new
		}
		else
		{
			obj = x!.next
			Oberon0Lexer.mark("mult def")
		}
	}

	// ---------------------------------------------------
	internal static func find(_ obj: inout RISCCodeGenerator.Object)
	{
		var s = topScope
		`guard`!.name = Oberon0Lexer.id
		while true
		{
			var x = s!.next
			while x!.name != Oberon0Lexer.id {
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
				Oberon0Lexer.mark("undefined identifier: \(x!.name)")
				break
			}
			s = s!.dsc
		}
	}

	// ---------------------------------------------------
	internal static func findField(_ obj: inout RISCCodeGenerator.Object, _ list: RISCCodeGenerator.Object)
	{
		var list = list
		
		`guard`!.name = Oberon0Lexer.id
		while list!.name != Oberon0Lexer.id {
			list = list!.next
		}
		obj = list
	}

	// ---------------------------------------------------
	internal static func isParameter(_ obj: RISCCodeGenerator.Object) -> Bool {
		return (obj!.kind == RISCCodeGenerator.Par) || (obj!.kind == RISCCodeGenerator.Var) && (obj!.val > 0)
	}

	// ---------------------------------------------------
	internal static func openScope(_ topScope: RISCCodeGenerator.Object) -> RISCCodeGenerator.Object
	{
		let s:RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
		s!.kind = RISCCodeGenerator.Head
		s!.dsc = topScope
		s!.next = `guard`
		return s
	}

	// ---------------------------------------------------
	internal static func closeScope(_ topScope: inout RISCCodeGenerator.Object) {
		topScope = topScope!.dsc
	}

	// MARK:- Parser
	// ---------------------------------------------------
	internal static func selector(_ x: inout RISCCodeGenerator.Item)
	{
		var y = RISCCodeGenerator.Item()
		var obj: RISCCodeGenerator.Object = nil

		while (sym == .lbrak) || (sym == .period)
		{
			if sym == .lbrak
			{
				Oberon0Lexer.get(&sym)
				parseExpression(&y)
				
				if x.type!.form == RISCCodeGenerator.Array {
					RISCCodeGenerator.Index(&x, &y)
				}
				else { Oberon0Lexer.mark("not an array") }
				
				if sym == .rbrak {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("]?") }
			}
			else
			{
				Oberon0Lexer.get(&sym)
				if sym == .ident
				{
					if x.type!.form == RISCCodeGenerator.Record
					{
						findField(&obj, x.type!.fields)
						Oberon0Lexer.get(&sym)
						if obj != `guard` {
							RISCCodeGenerator.Field(&x, obj)
						}
						else { Oberon0Lexer.mark("undef") }
					}
					else { Oberon0Lexer.mark("not a record") }
				}
				else { Oberon0Lexer.mark("ident?") }
			}
		}
	}

	// ---------------------------------------------------
	internal static func factor(_ x: inout RISCCodeGenerator.Item)
	{
		var obj: RISCCodeGenerator.Object = nil
		
		// sync
		if sym < .lparen
		{
			Oberon0Lexer.mark("ident?")
			repeat {
				Oberon0Lexer.get(&sym)
			} while !(sym >= .lparen)
		}

		if sym == .ident
		{
			find(&obj)
			Oberon0Lexer.get(&sym)
			RISCCodeGenerator.MakeItem(&x, obj!.symbolInfo)
			selector(&x)
		}
		else if sym == .number
		{
			RISCCodeGenerator.MakeConstItem(&x, RISCCodeGenerator.intType, Oberon0Lexer.val)
			Oberon0Lexer.get(&sym)
		}
		else if sym == .lparen
		{
			Oberon0Lexer.get(&sym)
			parseExpression(&x)
			if sym == .rparen {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(")?") }
		}
		else if sym == .not
		{
			Oberon0Lexer.get(&sym)
			factor(&x)
			RISCCodeGenerator.Op1(.not, &x)
		}
		else
		{
			Oberon0Lexer.mark("factor?")
			RISCCodeGenerator.MakeItem(&x, `guard`!.symbolInfo)
		}
	}

	// ---------------------------------------------------
	internal static func parseTerminalSymbol(_ x: inout RISCCodeGenerator.Item)
	{
		var y = RISCCodeGenerator.Item()
		var op: OberonSymbol;
		
		factor(&x)
		while (sym >= .times) && (sym <= .and)
		{
			op = sym
			Oberon0Lexer.get(&sym)
			if op == .and {
				RISCCodeGenerator.Op1(op, &x)
			}
			factor(&y)
			RISCCodeGenerator.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseSimpleExpression(_ x: inout RISCCodeGenerator.Item)
	{
		var y = RISCCodeGenerator.Item()
		var op: OberonSymbol
		
		if sym == .plus
		{
			Oberon0Lexer.get(&sym)
			parseTerminalSymbol(&x)
		}
		else if sym == .minus
		{
			Oberon0Lexer.get(&sym)
			parseTerminalSymbol(&x)
			RISCCodeGenerator.Op1(.minus, &x)
		}
		else {
			parseTerminalSymbol(&x)
		}
		while (sym >= .plus) && (sym <= .or)
		{
			op = sym
			Oberon0Lexer.get(&sym)
			if op == .or {
				RISCCodeGenerator.Op1(op, &x)
			}
			parseTerminalSymbol(&y)
			RISCCodeGenerator.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseExpression(_ x: inout RISCCodeGenerator.Item)
	{
		var y = RISCCodeGenerator.Item()
		var op: OberonSymbol
		
		parseSimpleExpression(&x)
		if (sym >= .eql) && (sym <= .gtr)
		{
			op = sym
			Oberon0Lexer.get(&sym)
			parseSimpleExpression(&y)
			RISCCodeGenerator.Relation(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	internal static func parseParameter(_ fp: inout RISCCodeGenerator.Object)
	{
		var x = RISCCodeGenerator.Item()
		
		parseExpression(&x)
		if isParameter(fp)
		{
			RISCCodeGenerator.Parameter(&x, fp!.type, fp!.kind)
			fp = fp!.next
		}
		else { Oberon0Lexer.mark("too many parameters") }
	}

	// ---------------------------------------------------
	internal static func parseStatementSequence()
	{
		var par, obj: RISCCodeGenerator.Object
		var x = RISCCodeGenerator.Item()
		var y = RISCCodeGenerator.Item()
		var L: Int

		func param(_ x: inout RISCCodeGenerator.Item)
		{
			if sym == .lparen {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(")?") }
			parseExpression(&x)
			if sym == .rparen {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(")?") }
		}

		while true /*sync*/
		{
			obj = `guard`
			
			if sym < .ident
			{
				Oberon0Lexer.mark("statement?")
				repeat {
					Oberon0Lexer.get(&sym)
				} while !(sym >= .ident)
			}
			if sym == .ident
			{
				find(&obj)
				Oberon0Lexer.get(&sym)
				RISCCodeGenerator.MakeItem(&x, obj!.symbolInfo)
				selector(&x)
				
				if sym == .becomes
				{
					Oberon0Lexer.get(&sym)
					parseExpression(&y)
					RISCCodeGenerator.Store(&x, &y)
				}
				else if sym == .eql
				{
					Oberon0Lexer.mark(":= ?")
					Oberon0Lexer.get(&sym)
					parseExpression(&y)
				}
				else if x.mode == RISCCodeGenerator.Proc
				{
					par = obj!.dsc
					if sym == .lparen
					{
						Oberon0Lexer.get(&sym)
						if sym == .rparen {
							Oberon0Lexer.get(&sym)
						}
						else
						{
							while true
							{
								parseParameter(&par)
								if sym == .comma {
									Oberon0Lexer.get(&sym)
								}
								else if sym == .rparen
								{
									Oberon0Lexer.get(&sym)
									break
								}
								else if sym >= .semicolon {
									break
								}
								else { Oberon0Lexer.mark(") or , ?") }
							}
						}
					}
					if obj!.val < 0 {
						Oberon0Lexer.mark("forward call")
					}
					else if !isParameter(par) {
						RISCCodeGenerator.call(&x)
					}
					else { Oberon0Lexer.mark("too few parameters") }
				}
				else if x.mode == RISCCodeGenerator.SProc
				{
					if obj!.val <= 3 {
						param(&y)
					}
					RISCCodeGenerator.ioCall(&x, &y)
				}
				else if obj!.kind == RISCCodeGenerator.Typ {
					Oberon0Lexer.mark("illegal assignment?")
				}
				else { Oberon0Lexer.mark("statement?") }
				
			}
			else if sym == .if
			{
				Oberon0Lexer.get(&sym)
				parseExpression(&x)
				RISCCodeGenerator.conditionalJump(&x)
				if sym == .then {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("THEN?") }
				parseStatementSequence()
				L = 0
				while sym == .elsif
				{
					Oberon0Lexer.get(&sym)
					RISCCodeGenerator.jumpForward(&L)
					RISCCodeGenerator.fixLink(x.a)
					parseExpression(&x)
					RISCCodeGenerator.conditionalJump(&x)
					if sym == .then {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark("THEN?") }
					parseStatementSequence()
				}
				if sym == .else
				{
					Oberon0Lexer.get(&sym)
					RISCCodeGenerator.jumpForward(&L)
					RISCCodeGenerator.fixLink(x.a)
					parseStatementSequence()
				}
				else { RISCCodeGenerator.fixLink(x.a) }
				RISCCodeGenerator.fixLink(L)
				if sym == .end {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("END?") }
			}
			else if sym == .while
			{
				Oberon0Lexer.get(&sym)
				L = Int(RISCCodeGenerator.pc)
				parseExpression(&x)
				RISCCodeGenerator.conditionalJump(&x)
				
				if sym == .do {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("DO?") }
				parseStatementSequence()
				RISCCodeGenerator.jumpBack(L)
				RISCCodeGenerator.fixLink(x.a)
				if sym == .end {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("END?") }
			}
			if sym == .semicolon {
				Oberon0Lexer.get(&sym)
			}
			else if (sym >= .semicolon) && (sym < .if) || (sym >= .array) {
				break
			}
			else { Oberon0Lexer.mark("; ?") }
		}
	}

	// ---------------------------------------------------
	internal static func parseIdentifierList(
		_ kind: Int,
		_ first: inout RISCCodeGenerator.Object)
	{
		var obj: RISCCodeGenerator.Object = nil
		
		if sym == .ident
		{
			newObj(&first, kind)
			Oberon0Lexer.get(&sym)
			while sym == .comma
			{
				Oberon0Lexer.get(&sym)
				if sym == .ident
				{
					newObj(&obj, kind)
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark("ident?") }
			}
			if sym == .colon {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(":?") }
		}
	}

	// ---------------------------------------------------
	internal static func parseType(_ type: inout RISCCodeGenerator.`Type`)
	{
		var obj: RISCCodeGenerator.Object = nil
		var first: RISCCodeGenerator.Object = nil
		var x = RISCCodeGenerator.Item()
		var tp: RISCCodeGenerator.`Type` = nil

		type = RISCCodeGenerator.intType /*sync*/
		if (sym != .ident) && (sym < .array)
		{
			Oberon0Lexer.mark("type?")
			repeat {
				Oberon0Lexer.get(&sym)
			} while !((sym == .ident) || (sym >= .array))
		}
		if sym == .ident
		{
			find(&obj)
			Oberon0Lexer.get(&sym)
			if obj!.kind == RISCCodeGenerator.Typ {
				type = obj!.type
			}
			else { Oberon0Lexer.mark("type?") }
		}
		else if sym == .array
		{
			Oberon0Lexer.get(&sym)
			parseExpression(&x)
			if (x.mode != RISCCodeGenerator.Const) || (x.a < 0) {
				Oberon0Lexer.mark("bad index")
			}
			if sym == .of {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark("OF?") }
			parseType(&tp)
			type = RISCCodeGenerator.TypeDesc()
			type!.form = RISCCodeGenerator.Array
			type!.base = tp
			type!.len = x.a
			type!.size = type!.len * tp!.size
		}
		else if sym == .record
		{
			Oberon0Lexer.get(&sym)
			type = RISCCodeGenerator.TypeDesc()
			type!.form = RISCCodeGenerator.Record
			type!.size = 0
			topScope = openScope(topScope)
			while true
			{
				if sym == .ident
				{
					parseIdentifierList(RISCCodeGenerator.Fld, &first)
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
					Oberon0Lexer.get(&sym)
				}
				else if sym == .ident {
					Oberon0Lexer.mark("; ?")
				}
				else { break }
			}
			type!.fields = topScope!.next
			closeScope(&topScope)
			if sym == .end {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark("END?") }
		}
		else { Oberon0Lexer.mark("ident?") }
	}

	// ---------------------------------------------------
	internal static func parseDeclarations(_ varsize: inout Int)
	{
		var obj: RISCCodeGenerator.Object = nil
		var first: RISCCodeGenerator.Object = nil
		var x = RISCCodeGenerator.Item()
		var tp: RISCCodeGenerator.`Type` = nil
		
		/*sync*/
		if (sym < .const) && (sym != .end)
		{
			Oberon0Lexer.mark("declaration?")
			repeat {
				Oberon0Lexer.get(&sym)
			} while !((sym >= .const) || (sym == .end))
		}
		while true
		{
			if sym == .const
			{
				Oberon0Lexer.get(&sym)
				while sym == .ident
				{
					newObj(&obj, RISCCodeGenerator.Const)
					Oberon0Lexer.get(&sym)
					if sym == .eql {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark("=?") }
					parseExpression(&x)
					if x.mode == RISCCodeGenerator.Const
					{
						obj!.val = x.a
						obj!.type = x.type
					}
					else { Oberon0Lexer.mark("expression not constant") }
					if sym == .semicolon {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark(";?") }
				}
			}
			if sym == .type
			{
				Oberon0Lexer.get(&sym)
				while sym == .ident
				{
					newObj(&obj, RISCCodeGenerator.Typ)
					Oberon0Lexer.get(&sym)
					if sym == .eql {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark("=?") }
					parseType(&obj!.type)
					if sym == .semicolon {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark(";?") }
				}
			}
			if sym == .var
			{
				Oberon0Lexer.get(&sym)
				while sym == .ident
				{
					parseIdentifierList(RISCCodeGenerator.Var, &first)
					parseType(&tp)
					obj = first
					while obj != `guard`
					{
						obj!.type = tp
						obj!.level = RISCCodeGenerator.curlev
						varsize = varsize + Int( obj!.type!.size)
						obj!.val = -varsize
						obj = obj!.next
					}
					if sym == .semicolon {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark("; ?") }
				}
			}
			if (sym >= .const) && (sym <= .var) {
				Oberon0Lexer.mark("declaration?")
			}
			else { break }
		}
	}

	// ---------------------------------------------------
	internal static func parseProcedureDeclaration()
	{
		let marksize: Int = 8
		var proc: RISCCodeGenerator.Object = nil
		var obj: RISCCodeGenerator.Object = nil
		var procid: String
		var locblksize, parblksize: Int
		
		func FPSection()
		{
			var obj: RISCCodeGenerator.Object = nil
			var first: RISCCodeGenerator.Object = nil
			var tp: RISCCodeGenerator.`Type`
			var parsize: Int
			
			if sym == .var
			{
				Oberon0Lexer.get(&sym)
				parseIdentifierList(RISCCodeGenerator.Par, &first)
			}
			else {
				parseIdentifierList(RISCCodeGenerator.Var, &first)
			}
			if sym == .ident
			{
				find(&obj)
				Oberon0Lexer.get(&sym)
				if obj!.kind == RISCCodeGenerator.Typ {
					tp = obj!.type
				}
				else
				{
					Oberon0Lexer.mark("ident?")
					tp = RISCCodeGenerator.intType
				}
			}
			else
			{
				Oberon0Lexer.mark("ident?")
				tp = RISCCodeGenerator.intType
			}
			if first!.kind == RISCCodeGenerator.Var
			{
				parsize = Int(tp!.size)
				if tp!.form >= RISCCodeGenerator.Array {
					Oberon0Lexer.mark("no struct params")
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
		Oberon0Lexer.get(&sym)
		if sym == .ident
		{
			procid = Oberon0Lexer.id
			newObj(&proc, RISCCodeGenerator.Proc)
			Oberon0Lexer.get(&sym)
			parblksize = marksize
			RISCCodeGenerator.IncLevel(1)
			topScope = openScope(topScope)
			proc!.val = -1
			if sym == .lparen
			{
				Oberon0Lexer.get(&sym)
				if sym == .rparen {
					Oberon0Lexer.get(&sym)
				}
				else
				{
					FPSection()
					while sym == .semicolon
					{
						Oberon0Lexer.get(&sym)
						FPSection()
					}
					if sym == .rparen {
						Oberon0Lexer.get(&sym)
					}
					else { Oberon0Lexer.mark(")?") }
				}
			}
			else if RISCCodeGenerator.curlev == 1 {
				RISCCodeGenerator.enterCmd(&procid)
			}
			obj = topScope!.next
			locblksize = parblksize
			while obj != `guard`
			{
				obj!.level = RISCCodeGenerator.curlev
				if obj!.kind == RISCCodeGenerator.Par {
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
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(";?") }
			locblksize = 0
			parseDeclarations(&locblksize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark(";?") }
			}
			proc!.val = Int(RISCCodeGenerator.pc)
			RISCCodeGenerator.enter(locblksize)
			if sym == .begin
			{
				Oberon0Lexer.get(&sym)
				parseStatementSequence()
			}
			if sym == .end {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark("END?") }
			if sym == .ident
			{
				if procid != Oberon0Lexer.id {
					Oberon0Lexer.mark("no match")
				}
				Oberon0Lexer.get(&sym)
			}
			RISCCodeGenerator.procedureReturn(parblksize - marksize)
			closeScope(&topScope)
			RISCCodeGenerator.IncLevel(-1)
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
			Oberon0Lexer.get(&sym)
			RISCCodeGenerator.open()
			topScope = openScope(topScope)
			varsize = 0
			if sym == .ident
			{
				modid = Oberon0Lexer.id
				Oberon0Lexer.get(&sym)
				print("\(modid)", to: &OberonLog)
			}
			else { Oberon0Lexer.mark("ident?") }
			if sym == .semicolon {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark(";?") }
			parseDeclarations(&varsize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					Oberon0Lexer.get(&sym)
				}
				else { Oberon0Lexer.mark(";?") }
			}
			RISCCodeGenerator.header(varsize)
			if sym == .begin
			{
				Oberon0Lexer.get(&sym)
				parseStatementSequence()
			}
			if sym == .end {
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark("END?") }
			if sym == .ident
			{
				if modid != Oberon0Lexer.id {
					Oberon0Lexer.mark("no match")
				}
				Oberon0Lexer.get(&sym)
			}
			else { Oberon0Lexer.mark("ident?") }
			if sym != .period {
				Oberon0Lexer.mark(". ?")
			}
			closeScope(&topScope)
			if !Oberon0Lexer.error
			{
				RISCCodeGenerator.close()
				print("code generated\(RISCCodeGenerator.pc, pad: 6)", to: &OberonLog)
			}
		}
		else { Oberon0Lexer.mark("MODULE?") }
	}

	// MARK:- Public Interface
	// ---------------------------------------------------
	/// Program signature/marker
	static var magic: UInt32 {
		return UInt32(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	/**
	procedureReturns the compiled program
	*/
	static var program: [UInt32]
	{
		let objCode = RISCCodeGenerator.getObjectCode()
		var program = [UInt32]()
		program.reserveCapacity(objCode.count + 2)
		program.append(Oberon0Parser.magic)
		program.append(UInt32(RISCCodeGenerator.entry * 4))
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
		Oberon0Lexer.Init(sourceStream: sourceStream)
		Oberon0Lexer.get(&sym)
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
		RISCCodeGenerator.decode(to: &result)
		return result.isEmpty ? "!!!!! NO OUTPUT !!!!!" : result.description
	}

	// MARK:- Support functions
	// ---------------------------------------------------
	fileprivate static func enter(
		_ kind: Int,
		_ n: Int,
		_ name: String,
		_ type: RISCCodeGenerator.`Type`,
		in topScope: inout RISCCodeGenerator.Object)
	{
		let obj: RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
		obj!.kind = kind
		obj!.val = n
		obj!.name = name
		obj!.type = type
		obj!.dsc = nil
		obj!.next = topScope!.next
		topScope!.next = obj
	}

	fileprivate static func makeGuard() -> RISCCodeGenerator.Object
	{
		let `guard`: RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
		`guard`!.kind = RISCCodeGenerator.Var
		`guard`!.type = RISCCodeGenerator.intType
		`guard`!.val = 0
		
		return `guard`
	}

	fileprivate static func makeTopScope() -> (RISCCodeGenerator.Object, RISCCodeGenerator.Object)
	{
		var topScope = openScope(nil)
		let universe = topScope
		
		enter(RISCCodeGenerator.Typ, 1, "Bool", RISCCodeGenerator.boolType, in: &topScope)
		enter(RISCCodeGenerator.Typ, 2, "Int", RISCCodeGenerator.intType, in: &topScope)
		enter(RISCCodeGenerator.Const, 1, "TRUE", RISCCodeGenerator.boolType, in: &topScope)
		enter(RISCCodeGenerator.Const, 0, "FALSE", RISCCodeGenerator.boolType, in: &topScope)
		enter(RISCCodeGenerator.SProc, 1, "Read", nil, in: &topScope)
		enter(RISCCodeGenerator.SProc, 2, "Write", nil, in: &topScope)
		enter(RISCCodeGenerator.SProc, 3, "WriteHex", nil, in: &topScope)
		enter(RISCCodeGenerator.SProc, 4, "WriteLn", nil, in: &topScope)
		
		return (topScope, universe)
	}
}


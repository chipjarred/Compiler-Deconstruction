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
	internal typealias SymbolInfo = RISCCodeGenerator.SymbolInfo
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
		`guard`!.symbolInfo.name = Oberon0Lexer.id
		while x!.next!.symbolInfo.name != Oberon0Lexer.id {
			x = x!.next
		}
		if x!.next == `guard`
		{
			let new: RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
			new!.symbolInfo = SymbolInfo(kind: kind)
			new!.symbolInfo.name = Oberon0Lexer.id
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
		`guard`!.symbolInfo.name = Oberon0Lexer.id
		while true
		{
			var x = s!.next
			while x!.symbolInfo.name != Oberon0Lexer.id {
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
				Oberon0Lexer.mark("undefined identifier: \(x!.symbolInfo.name)")
				break
			}
			s = s!.dsc
		}
	}

	// ---------------------------------------------------
	internal static func findField(_ obj: inout RISCCodeGenerator.Object, _ list: RISCCodeGenerator.Object)
	{
		var list = list
		
		`guard`!.symbolInfo.name = Oberon0Lexer.id
		while list!.symbolInfo.name != Oberon0Lexer.id {
			list = list!.next
		}
		obj = list
	}

	// ---------------------------------------------------
	internal static func openScope(_ topScope: RISCCodeGenerator.Object) -> RISCCodeGenerator.Object
	{
		let scope:RISCCodeGenerator.Object = RISCCodeGenerator.ObjDesc()
		scope!.symbolInfo = SymbolInfo(kind: RISCCodeGenerator.Head)
		scope!.dsc = topScope
		scope!.next = `guard`
		return scope
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
							RISCCodeGenerator.Field(&x, obj!.symbolInfo)
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
		if fp!.symbolInfo.isParameter
		{
			RISCCodeGenerator.Parameter(&x, fp!.symbolInfo)
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
					if obj!.symbolInfo.value < 0 {
						Oberon0Lexer.mark("forward call")
					}
					else if !(par!.symbolInfo.isParameter) {
						RISCCodeGenerator.call(&x)
					}
					else { Oberon0Lexer.mark("too few parameters") }
				}
				else if x.mode == RISCCodeGenerator.SProc
				{
					if obj!.symbolInfo.value <= 3 {
						param(&y)
					}
					RISCCodeGenerator.ioCall(&x, &y)
				}
				else if obj!.symbolInfo.kind == RISCCodeGenerator.Typ {
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
			if obj!.symbolInfo.kind == RISCCodeGenerator.Typ {
				type = obj!.symbolInfo.type
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
						obj!.symbolInfo.type = tp
						obj!.symbolInfo.value = Int(type!.size)
						type!.size += obj!.symbolInfo.type!.size
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
						obj!.symbolInfo.value = x.a
						obj!.symbolInfo.type = x.type
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
					parseType(&obj!.symbolInfo.type)
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
						obj!.symbolInfo.type = tp
						obj!.symbolInfo.level = RISCCodeGenerator.curlev
						varsize = varsize + Int( obj!.symbolInfo.type!.size)
						obj!.symbolInfo.value = -varsize
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
				if obj!.symbolInfo.kind == RISCCodeGenerator.Typ {
					tp = obj!.symbolInfo.type
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
			if first!.symbolInfo.kind == RISCCodeGenerator.Var
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
				obj!.symbolInfo.type = tp
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
			proc!.symbolInfo.value = -1
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
				obj!.symbolInfo.level = RISCCodeGenerator.curlev
				if obj!.symbolInfo.kind == RISCCodeGenerator.Par {
					locblksize -= WordSize
				}
				else {
					locblksize -= Int(obj!.symbolInfo.type!.size)
				}
				obj!.symbolInfo.value = locblksize
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
			proc!.symbolInfo.value = Int(RISCCodeGenerator.pc)
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
		_ value: Int,
		_ name: String,
		_ type: RISCCodeGenerator.`Type`,
		in topScope: inout RISCCodeGenerator.Object)
	{
		let obj = RISCCodeGenerator.ObjDesc()
		obj.symbolInfo = SymbolInfo(
			name: name,
			kind: kind,
			type: type,
			value: value
		)
		obj.dsc = nil
		obj.next = topScope!.next
		topScope!.next = obj
	}

	// ---------------------------------------------------
	fileprivate static func makeGuard() -> RISCCodeGenerator.Object
	{
		let `guard` = RISCCodeGenerator.ObjDesc()
		`guard`.symbolInfo = SymbolInfo(
			kind: RISCCodeGenerator.Var,
			type: RISCCodeGenerator.intType,
			value: 0
		)
		
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


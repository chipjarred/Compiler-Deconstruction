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
	internal typealias SymbolInfo = SymbolTable.SymbolInfo
	internal static let WordSize:Int = 4

	internal static var sym: OberonSymbol = .null
	internal static var loaded: Bool = false

	// ---------------------------------------------------
	internal static func findField(_ obj: inout RISCCodeGenerator.Object, _ list: RISCCodeGenerator.Object)
	{
		var list = list
		
		SymbolTable.sentinel!.symbolInfo.name = Oberon0Lexer.identifier
		while list!.symbolInfo.name != Oberon0Lexer.identifier {
			list = list!.next
		}
		obj = list
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
				sym = Oberon0Lexer.getSymbol()
				parseExpression(&y)
				
				if x.type!.form == RISCCodeGenerator.Array {
					RISCCodeGenerator.Index(&x, &y)
				}
				else { Oberon0Lexer.mark("not an array") }
				
				if sym == .rbrak {
					sym = Oberon0Lexer.getSymbol()
				}
				else { Oberon0Lexer.mark("]?") }
			}
			else
			{
				sym = Oberon0Lexer.getSymbol()
				if sym == .ident
				{
					if x.type!.form == RISCCodeGenerator.Record
					{
						findField(&obj, x.type!.fields)
						sym = Oberon0Lexer.getSymbol()
						if obj != SymbolTable.sentinel {
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
				sym = Oberon0Lexer.getSymbol()
			} while !(sym >= .lparen)
		}

		if sym == .ident
		{
			obj = SymbolTable.find(name: Oberon0Lexer.identifier)
			sym = Oberon0Lexer.getSymbol()
			x = RISCCodeGenerator.makeItem(obj!.symbolInfo)
			selector(&x)
		}
		else if sym == .number
		{
			x = RISCCodeGenerator.makeConstItem(
				RISCCodeGenerator.intType,
				Oberon0Lexer.value
			)
			sym = Oberon0Lexer.getSymbol()
		}
		else if sym == .lparen
		{
			sym = Oberon0Lexer.getSymbol()
			parseExpression(&x)
			if sym == .rparen {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(")?") }
		}
		else if sym == .not
		{
			sym = Oberon0Lexer.getSymbol()
			factor(&x)
			RISCCodeGenerator.Op1(.not, &x)
		}
		else
		{
			Oberon0Lexer.mark("factor?")
			x = RISCCodeGenerator.makeItem(SymbolTable.sentinel!.symbolInfo)
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
			sym = Oberon0Lexer.getSymbol()
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
			sym = Oberon0Lexer.getSymbol()
			parseTerminalSymbol(&x)
		}
		else if sym == .minus
		{
			sym = Oberon0Lexer.getSymbol()
			parseTerminalSymbol(&x)
			RISCCodeGenerator.Op1(.minus, &x)
		}
		else {
			parseTerminalSymbol(&x)
		}
		while (sym >= .plus) && (sym <= .or)
		{
			op = sym
			sym = Oberon0Lexer.getSymbol()
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
			sym = Oberon0Lexer.getSymbol()
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
		// ---------------------------------------------------
		func param(_ x: inout RISCCodeGenerator.Item)
		{
			if sym == .lparen {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(")?") }
			parseExpression(&x)
			if sym == .rparen {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(")?") }
		}
		
		// ---------------------------------------------------
		func advanceLexerToAtLeastIdentifier() -> OberonSymbol
		{
			var symbol = OberonSymbol.null
			repeat {
				symbol = Oberon0Lexer.getSymbol()
			} while symbol < .ident
			
			return symbol
		}
		
		// ---------------------------------------------------
		func parseAssignment(_ x: RISCCodeGenerator.Item)
		{
			sym = Oberon0Lexer.getSymbol()
			var y = RISCCodeGenerator.Item()
			parseExpression(&y)
			var newX = x
			RISCCodeGenerator.Store(&newX, &y)
		}
		
		// ---------------------------------------------------
		func parseErroneousEquality()
		{
			sym = Oberon0Lexer.getSymbol()
			var y = RISCCodeGenerator.Item()
			parseExpression(&y)
		}
		
		// ---------------------------------------------------
		func parseProcedureCall(
			_ obj: SymbolTable.ListNode?,
			_ x: RISCCodeGenerator.Item)
		{
			var par = obj!.parentScope
			if sym == .lparen
			{
				sym = Oberon0Lexer.getSymbol()
				if sym == .rparen {
					sym = Oberon0Lexer.getSymbol()
				}
				else
				{
					while true
					{
						parseParameter(&par)
						if sym == .comma {
							sym = Oberon0Lexer.getSymbol()
						}
						else if sym == .rparen
						{
							sym = Oberon0Lexer.getSymbol()
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
			else if !(par!.symbolInfo.isParameter)
			{
				var newX = x
				RISCCodeGenerator.call(&newX)
			}
			else { Oberon0Lexer.mark("too few parameters") }
		}
		
		// ---------------------------------------------------
		func parseStandardProcedureCall(
			_ obj: SymbolTable.ListNode?,
			_ x: RISCCodeGenerator.Item)
		{
			var y = RISCCodeGenerator.Item()
			if obj!.symbolInfo.value <= 3 {
				param(&y)
			}
			
			var newX = x
			RISCCodeGenerator.ioCall(&newX, &y)
		}

		// ---------------------------------------------------
		func parseThen()
		{
			if sym == .then {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("THEN?") }
			parseStatementSequence()
		}
		
		// ---------------------------------------------------
		func parseIfStatement()
		{
			sym = Oberon0Lexer.getSymbol()
			var x = RISCCodeGenerator.Item()
			parseExpression(&x)
			RISCCodeGenerator.conditionalJump(&x)
			parseThen()
			var L = 0
			
			while sym == .elsif
			{
				sym = Oberon0Lexer.getSymbol()
				RISCCodeGenerator.jumpForward(&L)
				RISCCodeGenerator.fixLink(x.a)
				parseExpression(&x)
				RISCCodeGenerator.conditionalJump(&x)
				parseThen()
			}
			
			if sym == .else
			{
				sym = Oberon0Lexer.getSymbol()
				RISCCodeGenerator.jumpForward(&L)
				RISCCodeGenerator.fixLink(x.a)
				parseStatementSequence()
			}
			else {
				RISCCodeGenerator.fixLink(x.a)
			}
			
			RISCCodeGenerator.fixLink(L)
			
			if sym == .end {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("END?") }
		}
		
		// ---------------------------------------------------
		func parseWhileStatement()
		{
			sym = Oberon0Lexer.getSymbol()
			let L = Int(RISCCodeGenerator.pc)
			var x = RISCCodeGenerator.Item()
			parseExpression(&x)
			RISCCodeGenerator.conditionalJump(&x)
			
			if sym == .do {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("DO?") }
			parseStatementSequence()
			RISCCodeGenerator.jumpBack(L)
			RISCCodeGenerator.fixLink(x.a)
			if sym == .end {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("END?") }
		}
		
		// ---------------------------------------------------
		while true // sync
		{
			if sym < .ident
			{
				Oberon0Lexer.mark("statement?")
				sym = advanceLexerToAtLeastIdentifier()
			}
			
			switch sym
			{
				case .ident:
					let obj = SymbolTable.find(name: Oberon0Lexer.identifier)
					sym = Oberon0Lexer.getSymbol()
					var x = RISCCodeGenerator.makeItem(obj!.symbolInfo)
					selector(&x)
					
					if sym == .becomes {
						parseAssignment(x)
					}
					else if sym == .eql
					{
						Oberon0Lexer.mark(":= ?")
						parseErroneousEquality()
					}
					else if x.mode == .procedure {
						parseProcedureCall(obj, x)
					}
					else if x.mode == .standardProcedure {
						parseStandardProcedureCall(obj, x)
					}
					else if obj!.symbolInfo.kind == .type {
						Oberon0Lexer.mark("illegal assignment?")
					}
					else { Oberon0Lexer.mark("statement?") }
				
				case .if: parseIfStatement()
				case .while: parseWhileStatement()
				
				default: break
			}

			if sym == .semicolon {
				sym = Oberon0Lexer.getSymbol()
			}
			else if (sym >= .semicolon) && (sym < .if) || (sym >= .array) {
				break
			}
			else { Oberon0Lexer.mark("; ?") }
		}
	}

	// ---------------------------------------------------
	internal static func parseIdentifierList(_ kind: SymbolInfo.Kind)
		-> SymbolTable.ListNode?
	{
		if sym == .ident
		{
			let first = SymbolTable.newNode(named: Oberon0Lexer.identifier, kind: kind)
			sym = Oberon0Lexer.getSymbol()
			while sym == .comma
			{
				sym = Oberon0Lexer.getSymbol()
				if sym == .ident
				{
					let _ = SymbolTable.insert(
						named: Oberon0Lexer.identifier,
						kind: kind
					)
					sym = Oberon0Lexer.getSymbol()
				}
				else { Oberon0Lexer.mark("ident?") }
			}
			if sym == .colon {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(":?") }
			
			return first
		}
		
		return nil
	}

	// ---------------------------------------------------
	internal static func parseType() -> RISCCodeGenerator.`Type`
	{
		var obj: RISCCodeGenerator.Object = nil
		var x = RISCCodeGenerator.Item()

		var type = RISCCodeGenerator.intType // sync
		if (sym != .ident) && (sym < .array)
		{
			Oberon0Lexer.mark("type?")
			repeat {
				sym = Oberon0Lexer.getSymbol()
			} while !((sym == .ident) || (sym >= .array))
		}
		if sym == .ident
		{
			obj = SymbolTable.find(name: Oberon0Lexer.identifier)
			sym = Oberon0Lexer.getSymbol()
			if obj!.symbolInfo.kind == .type {
				type = obj!.symbolInfo.type
			}
			else { Oberon0Lexer.mark("type?") }
		}
		else if sym == .array
		{
			sym = Oberon0Lexer.getSymbol()
			parseExpression(&x)
			if (x.mode != .constant) || (x.a < 0) {
				Oberon0Lexer.mark("bad index")
			}
			if sym == .of {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("OF?") }
			
			let tp = parseType()
			type = RISCCodeGenerator.TypeDesc()
			type!.form = RISCCodeGenerator.Array
			type!.base = tp
			type!.len = x.a
			type!.size = type!.len * tp!.size
		}
		else if sym == .record
		{
			sym = Oberon0Lexer.getSymbol()
			type = RISCCodeGenerator.TypeDesc()
			type!.form = RISCCodeGenerator.Record
			type!.size = 0
			SymbolTable.openScope()
			while true
			{
				if sym == .ident
				{
					let first = parseIdentifierList(.field)
					let tp = parseType()
					obj = first
					while obj != SymbolTable.sentinel
					{
						obj!.symbolInfo.type = tp
						obj!.symbolInfo.value = Int(type!.size)
						type!.size += obj!.symbolInfo.type!.size
						obj = obj!.next
					}
				}
				if sym == .semicolon {
					sym = Oberon0Lexer.getSymbol()
				}
				else if sym == .ident {
					Oberon0Lexer.mark("; ?")
				}
				else { break }
			}
			type!.fields = SymbolTable.topScope!.next
			SymbolTable.closeScope()
			if sym == .end {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("END?") }
		}
		else { Oberon0Lexer.mark("ident?") }
		
		return type
	}

	// ---------------------------------------------------
	internal static func parseDeclarations(_ varsize: inout Int)
	{
		var obj: SymbolTable.ListNode? = nil
		var x = RISCCodeGenerator.Item()
		
		/*sync*/
		if (sym < .const) && (sym != .end)
		{
			Oberon0Lexer.mark("declaration?")
			repeat {
				sym = Oberon0Lexer.getSymbol()
			} while !((sym >= .const) || (sym == .end))
		}
		while true
		{
			if sym == .const
			{
				sym = Oberon0Lexer.getSymbol()
				while sym == .ident
				{
					let symbolInfo = SymbolTable.insert(
						named: Oberon0Lexer.identifier,
						kind: .constant
					)
					sym = Oberon0Lexer.getSymbol()
					if sym == .eql {
						sym = Oberon0Lexer.getSymbol()
					}
					else { Oberon0Lexer.mark("=?") }
					parseExpression(&x)
					if x.mode == .constant
					{
						symbolInfo!.value = x.a
						symbolInfo!.type = x.type
					}
					else { Oberon0Lexer.mark("expression not constant") }
					if sym == .semicolon {
						sym = Oberon0Lexer.getSymbol()
					}
					else { Oberon0Lexer.mark(";?") }
				}
			}
			if sym == .type
			{
				sym = Oberon0Lexer.getSymbol()
				while sym == .ident
				{
					let symbolInfo = SymbolTable.insert(
						named: Oberon0Lexer.identifier,
						kind: .type
					)
					sym = Oberon0Lexer.getSymbol()
					if sym == .eql {
						sym = Oberon0Lexer.getSymbol()
					}
					else { Oberon0Lexer.mark("=?") }
					if let symInfo = symbolInfo {
						symInfo.type = parseType()
					}
					if sym == .semicolon {
						sym = Oberon0Lexer.getSymbol()
					}
					else { Oberon0Lexer.mark(";?") }
				}
			}
			if sym == .var
			{
				sym = Oberon0Lexer.getSymbol()
				while sym == .ident
				{
					let first = parseIdentifierList(.variable)
					let tp = parseType()
					obj = first
					while obj != SymbolTable.sentinel
					{
						obj!.symbolInfo.type = tp
						obj!.symbolInfo.level = RISCCodeGenerator.curlev
						varsize = varsize + Int( obj!.symbolInfo.type!.size)
						obj!.symbolInfo.value = -varsize
						obj = obj!.next
					}
					if sym == .semicolon {
						sym = Oberon0Lexer.getSymbol()
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
		
		// ---------------------------------------------------
		func FPSection()
		{
			// ---------------------------------------------------
			func getType(for symbol: OberonSymbol) -> RISCCodeGenerator.`Type`
			{
				if sym == .ident
				{
					obj = SymbolTable.find(name: Oberon0Lexer.identifier)
					sym = Oberon0Lexer.getSymbol()
					if obj!.symbolInfo.kind == .type {
						return obj!.symbolInfo.type
					}
				}

				Oberon0Lexer.mark("ident?")
				return RISCCodeGenerator.intType
			}
			
			var first: RISCCodeGenerator.Object
			if sym == .var
			{
				sym = Oberon0Lexer.getSymbol()
				first = parseIdentifierList(.parameter)
			}
			else { first = parseIdentifierList(.variable) }
			
			let tp = getType(for: sym)
			
			let parsize: Int
			if first!.symbolInfo.kind == .variable
			{
				parsize = tp!.size
				if tp!.form >= RISCCodeGenerator.Array {
					Oberon0Lexer.mark("no struct params")
				}
			}
			else {
				parsize = WordSize
			}
			
			var obj = first
			while obj !== SymbolTable.sentinel
			{
				obj!.symbolInfo.type = tp
				parblksize += parsize
				obj = obj!.next
			}
		}

		// ProcedureDecl
		sym = Oberon0Lexer.getSymbol()
		if sym == .ident
		{
			procid = Oberon0Lexer.identifier
			proc = SymbolTable.newNode(named: Oberon0Lexer.identifier, kind: .procedure)
			sym = Oberon0Lexer.getSymbol()
			parblksize = marksize
			RISCCodeGenerator.IncLevel(1)
			SymbolTable.openScope()
			proc!.symbolInfo.value = -1
			
			if sym == .lparen
			{
				sym = Oberon0Lexer.getSymbol()
				if sym == .rparen {
					sym = Oberon0Lexer.getSymbol()
				}
				else
				{
					FPSection()
					while sym == .semicolon
					{
						sym = Oberon0Lexer.getSymbol()
						FPSection()
					}
					if sym == .rparen {
						sym = Oberon0Lexer.getSymbol()
					}
					else { Oberon0Lexer.mark(")?") }
				}
			}
			else if RISCCodeGenerator.curlev == 1 {
				RISCCodeGenerator.enterCmd(&procid)
			}
			
			obj = SymbolTable.topScope!.next
			locblksize = parblksize
			
			while obj != SymbolTable.sentinel
			{
				obj!.symbolInfo.level = RISCCodeGenerator.curlev
				if obj!.symbolInfo.kind == .parameter {
					locblksize -= WordSize
				}
				else {
					locblksize -= Int(obj!.symbolInfo.type!.size)
				}
				obj!.symbolInfo.value = locblksize
				obj = obj!.next
			}
			
			proc!.parentScope = SymbolTable.topScope!.next
			
			if sym == .semicolon {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(";?") }
			
			locblksize = 0
			parseDeclarations(&locblksize)
			
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					sym = Oberon0Lexer.getSymbol()
				}
				else { Oberon0Lexer.mark(";?") }
			}
			
			proc!.symbolInfo.value = Int(RISCCodeGenerator.pc)
			RISCCodeGenerator.enter(locblksize)
			
			if sym == .begin
			{
				sym = Oberon0Lexer.getSymbol()
				parseStatementSequence()
			}
			
			if sym == .end {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("END?") }
			
			if sym == .ident
			{
				if procid != Oberon0Lexer.identifier {
					Oberon0Lexer.mark("no match")
				}
				sym = Oberon0Lexer.getSymbol()
			}
			
			RISCCodeGenerator.procedureReturn(parblksize - marksize)
			SymbolTable.closeScope()
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
			sym = Oberon0Lexer.getSymbol()
			RISCCodeGenerator.open()
			SymbolTable.openScope()
			varsize = 0
			if sym == .ident
			{
				modid = Oberon0Lexer.identifier
				sym = Oberon0Lexer.getSymbol()
				print("\(modid)", to: &OberonLog)
			}
			else { Oberon0Lexer.mark("ident?") }
			if sym == .semicolon {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark(";?") }
			parseDeclarations(&varsize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					sym = Oberon0Lexer.getSymbol()
				}
				else { Oberon0Lexer.mark(";?") }
			}
			RISCCodeGenerator.header(varsize)
			if sym == .begin
			{
				sym = Oberon0Lexer.getSymbol()
				parseStatementSequence()
			}
			if sym == .end {
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("END?") }
			if sym == .ident
			{
				if modid != Oberon0Lexer.identifier {
					Oberon0Lexer.mark("no match")
				}
				sym = Oberon0Lexer.getSymbol()
			}
			else { Oberon0Lexer.mark("ident?") }
			if sym != .period {
				Oberon0Lexer.mark(". ?")
			}
			SymbolTable.closeScope()
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
		sym = Oberon0Lexer.getSymbol()
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
}


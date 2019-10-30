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
	private typealias CodeGen = RISCCodeGenerator
	private typealias Lexer = Oberon0Lexer
	internal static let WordSize:Int = 4

	internal static var sym: OberonSymbol = .null
	internal static var loaded: Bool = false

	// MARK:- Parser
	// ---------------------------------------------------
	private static func selector(_ x: inout CodeGen.Item)
	{
		while (sym == .lbrak) || (sym == .period)
		{
			if sym == .lbrak
			{
				sym = Lexer.getSymbol()
				var y = CodeGen.Item()
				parseExpression(&y)
				
				if x.type!.form == .array {
					x.index(at: y)
				}
				else { Lexer.mark("not an array") }
				
				if sym == .rbrak {
					sym = Lexer.getSymbol()
				}
				else { Lexer.mark("]?") }
			}
			else
			{
				sym = Lexer.getSymbol()
				if sym == .ident
				{
					if x.type!.form == .record
					{
						if let fieldInfo = x.type!.symbolInfoForField(
							named: Lexer.identifier)
						{
							x.setFieldInfo(from: fieldInfo)
						}
						else { Lexer.mark("undef") }
						
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark("not a record") }
				}
				else { Lexer.mark("ident?") }
			}
		}
	}

	// ---------------------------------------------------
	private static func factor(_ x: inout CodeGen.Item)
	{
		var obj: SymbolTable.ListNode? = nil
		
		// sync
		if sym < .lparen
		{
			Lexer.mark("ident?")
			repeat {
				sym = Lexer.getSymbol()
			} while !(sym >= .lparen)
		}

		if sym == .ident
		{
			obj = SymbolTable.find(name: Lexer.identifier)
			sym = Lexer.getSymbol()
			x = CodeGen.makeItem(obj!.symbolInfo)
			selector(&x)
		}
		else if sym == .number
		{
			x = CodeGen.makeConstItem(
				CodeGen.intType,
				Lexer.value
			)
			sym = Lexer.getSymbol()
		}
		else if sym == .lparen
		{
			sym = Lexer.getSymbol()
			parseExpression(&x)
			if sym == .rparen {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(")?") }
		}
		else if sym == .not
		{
			sym = Lexer.getSymbol()
			factor(&x)
			CodeGen.Op1(.not, &x)
		}
		else
		{
			Lexer.mark("factor?")
			x = CodeGen.makeItem(SymbolTable.sentinel!.symbolInfo)
		}
	}

	// ---------------------------------------------------
	private static func parseTerminalSymbol(_ x: inout CodeGen.Item)
	{
		var y = CodeGen.Item()
		var op: OberonSymbol;
		
		factor(&x)
		while (sym >= .times) && (sym <= .and)
		{
			op = sym
			sym = Lexer.getSymbol()
			if op == .and {
				CodeGen.Op1(op, &x)
			}
			factor(&y)
			CodeGen.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	private static func parseSimpleExpression(_ x: inout CodeGen.Item)
	{
		var y = CodeGen.Item()
		var op: OberonSymbol
		
		if sym == .plus
		{
			sym = Lexer.getSymbol()
			parseTerminalSymbol(&x)
		}
		else if sym == .minus
		{
			sym = Lexer.getSymbol()
			parseTerminalSymbol(&x)
			CodeGen.Op1(.minus, &x)
		}
		else {
			parseTerminalSymbol(&x)
		}
		while (sym >= .plus) && (sym <= .or)
		{
			op = sym
			sym = Lexer.getSymbol()
			if op == .or {
				CodeGen.Op1(op, &x)
			}
			parseTerminalSymbol(&y)
			CodeGen.Op2(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	private static func parseExpression(_ x: inout CodeGen.Item)
	{
		var y = CodeGen.Item()
		var op: OberonSymbol
		
		parseSimpleExpression(&x)
		if (sym >= .eql) && (sym <= .gtr)
		{
			op = sym
			sym = Lexer.getSymbol()
			parseSimpleExpression(&y)
			CodeGen.Relation(op, &x, &y)
		}
	}

	// ---------------------------------------------------
	private static func parseParameter(_ fp: inout SymbolTable.ListNode?)
	{
		var x = CodeGen.Item()
		
		parseExpression(&x)
		if fp!.symbolInfo.isParameter
		{
			CodeGen.Parameter(&x, fp!.symbolInfo)
			fp = fp!.next
		}
		else { Lexer.mark("too many parameters") }
	}

	// ---------------------------------------------------
	private static func param(_ x: inout CodeGen.Item)
	{
		if sym == .lparen {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(")?") }
		parseExpression(&x)
		if sym == .rparen {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(")?") }
	}
	
	// ---------------------------------------------------
	private static func advanceLexerToAtLeastIdentifier() -> OberonSymbol
	{
		var symbol = OberonSymbol.null
		repeat {
			symbol = Lexer.getSymbol()
		} while symbol < .ident
		
		return symbol
	}
	
	// ---------------------------------------------------
	private static func parseAssignment(_ x: CodeGen.Item)
	{
		sym = Lexer.getSymbol()
		var y = CodeGen.Item()
		parseExpression(&y)
		var newX = x
		CodeGen.Store(&newX, &y)
	}
	
	// ---------------------------------------------------
	private static func parseErroneousEquality()
	{
		sym = Lexer.getSymbol()
		var y = CodeGen.Item()
		parseExpression(&y)
	}
	
	// ---------------------------------------------------
	private static func parseProcedureCall(
		_ obj: SymbolTable.ListNode?,
		_ x: CodeGen.Item)
	{
		var par = obj!.parentScope
		if sym == .lparen
		{
			sym = Lexer.getSymbol()
			if sym == .rparen {
				sym = Lexer.getSymbol()
			}
			else
			{
				while true
				{
					parseParameter(&par)
					if sym == .comma {
						sym = Lexer.getSymbol()
					}
					else if sym == .rparen
					{
						sym = Lexer.getSymbol()
						break
					}
					else if sym >= .semicolon {
						break
					}
					else { Lexer.mark(") or , ?") }
				}
			}
		}
		if obj!.symbolInfo.value < 0 {
			Lexer.mark("forward call")
		}
		else if !(par!.symbolInfo.isParameter)
		{
			var newX = x
			CodeGen.call(&newX)
		}
		else { Lexer.mark("too few parameters") }
	}
	
	// ---------------------------------------------------
	private static func parseStandardProcedureCall(
		_ obj: SymbolTable.ListNode?,
		_ x: CodeGen.Item)
	{
		var y = CodeGen.Item()
		if obj!.symbolInfo.value <= 3 {
			param(&y)
		}
		
		var newX = x
		CodeGen.ioCall(&newX, &y)
	}

	// ---------------------------------------------------
	private static func parseThen()
	{
		if sym == .then {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("THEN?") }
		parseStatementSequence()
	}
	
	// ---------------------------------------------------
	private static func parseIfStatement()
	{
		sym = Lexer.getSymbol()
		var x = CodeGen.Item()
		parseExpression(&x)
		CodeGen.conditionalJump(&x)
		parseThen()
		var L = 0
		
		while sym == .elsif
		{
			sym = Lexer.getSymbol()
			CodeGen.jumpForward(&L)
			CodeGen.fixLink(x.a)
			parseExpression(&x)
			CodeGen.conditionalJump(&x)
			parseThen()
		}
		
		if sym == .else
		{
			sym = Lexer.getSymbol()
			CodeGen.jumpForward(&L)
			CodeGen.fixLink(x.a)
			parseStatementSequence()
		}
		else {
			CodeGen.fixLink(x.a)
		}
		
		CodeGen.fixLink(L)
		
		if sym == .end {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private static func parseWhileStatement()
	{
		sym = Lexer.getSymbol()
		let L = Int(CodeGen.pc)
		var x = CodeGen.Item()
		parseExpression(&x)
		CodeGen.conditionalJump(&x)
		
		if sym == .do {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("DO?") }
		parseStatementSequence()
		CodeGen.jumpBack(L)
		CodeGen.fixLink(x.a)
		if sym == .end {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private static func parseStatementSequence()
	{
		// ---------------------------------------------------
		while true // sync
		{
			if sym < .ident
			{
				Lexer.mark("statement?")
				sym = advanceLexerToAtLeastIdentifier()
			}
			
			switch sym
			{
				case .ident:
					let obj = SymbolTable.find(name: Lexer.identifier)
					sym = Lexer.getSymbol()
					var x = CodeGen.makeItem(obj!.symbolInfo)
					selector(&x)
					
					if sym == .becomes {
						parseAssignment(x)
					}
					else if sym == .eql
					{
						Lexer.mark(":= ?")
						parseErroneousEquality()
					}
					else if x.mode == .procedure {
						parseProcedureCall(obj, x)
					}
					else if x.mode == .standardProcedure {
						parseStandardProcedureCall(obj, x)
					}
					else if obj!.symbolInfo.kind == .type {
						Lexer.mark("illegal assignment?")
					}
					else { Lexer.mark("statement?") }
				
				case .if: parseIfStatement()
				case .while: parseWhileStatement()
				
				default: break
			}

			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else if (sym >= .semicolon) && (sym < .if) || (sym >= .array) {
				break
			}
			else { Lexer.mark("; ?") }
		}
	}

	// ---------------------------------------------------
	private static func parseIdentifierListAsArray(_ kind: SymbolInfo.Kind)
		-> [SymbolInfo]
	{
		var fields = [SymbolInfo]()
		
		if sym == .ident
		{
			fields.append(
				SymbolInfo(name: Lexer.identifier, kind: kind)
			)

			sym = Lexer.getSymbol()
			while sym == .comma
			{
				sym = Lexer.getSymbol()
				if sym == .ident
				{
					fields.append(
						SymbolInfo(name: Lexer.identifier, kind: kind)
					)
					sym = Lexer.getSymbol()
				}
				else { Lexer.mark("expected field identifier") }
			}
			if sym == .colon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("expected \":\"") }
		}
		
		return fields
	}

	// ---------------------------------------------------
	private static func parseIdentifierList(_ kind: SymbolInfo.Kind)
		-> SymbolTable.ListNode?
	{
		if sym == .ident
		{
			let first = SymbolTable.newNode(named: Lexer.identifier, kind: kind)
			sym = Lexer.getSymbol()
			while sym == .comma
			{
				sym = Lexer.getSymbol()
				if sym == .ident
				{
					let _ = SymbolTable.insert(
						named: Lexer.identifier,
						kind: kind
					)
					sym = Lexer.getSymbol()
				}
				else { Lexer.mark("expected field identifier") }
			}
			if sym == .colon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("expected \":\"") }
			
			return first
		}
		
		return nil
	}

	// ---------------------------------------------------
	private static func parseType() -> TypeInfo?
	{
		var obj: SymbolTable.ListNode? = nil
		var x = CodeGen.Item()

		var type:TypeInfo? = CodeGen.intType // sync
		if (sym != .ident) && (sym < .array)
		{
			Lexer.mark("type?")
			repeat {
				sym = Lexer.getSymbol()
			} while !((sym == .ident) || (sym >= .array))
		}
		if sym == .ident
		{
			obj = SymbolTable.find(name: Lexer.identifier)
			sym = Lexer.getSymbol()
			if obj!.symbolInfo.kind == .type {
				type = obj!.symbolInfo.type
			}
			else { Lexer.mark("type?") }
		}
		else if sym == .array
		{
			sym = Lexer.getSymbol()
			parseExpression(&x)
			if (x.mode != .constant) || (x.a < 0) {
				Lexer.mark("bad index")
			}
			if sym == .of {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("OF?") }
			
			let tp = parseType()
			type = TypeInfo()
			type!.form = .array
			type!.base = tp
			type!.len = x.a
			type!.size = type!.len * tp!.size
		}
		else if sym == .record
		{
			sym = Lexer.getSymbol()
			type = TypeInfo()
			type!.form = .record
			type!.size = 0
			SymbolTable.openScope()
			while true
			{
				if sym == .ident
				{
					let fields = parseIdentifierListAsArray(.field)
					let tp = parseType()
					
					for field in fields
					{
						field.type = tp
						field.value = type!.size
						type!.size += field.type!.size
					}
					
					type!.fields = fields
				}
				if sym == .semicolon {
					sym = Lexer.getSymbol()
				}
				else if sym == .ident {
					Lexer.mark("Expected \":\"")
				}
				else { break }
			}
			SymbolTable.closeScope()
			if sym == .end {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("Expected END for record") }
		}
		else { Lexer.mark("Expected an identifier") }
		
		return type
	}

	// ---------------------------------------------------
	private static func parseDeclarations(_ varsize: Int) -> Int
	{
		var obj: SymbolTable.ListNode? = nil
		var x = CodeGen.Item()
		
		var varsize = varsize
		
		// sync
		if (sym < .const) && (sym != .end)
		{
			Lexer.mark("declaration?")
			repeat {
				sym = Lexer.getSymbol()
			} while !((sym >= .const) || (sym == .end))
		}
		while true
		{
			if sym == .const
			{
				sym = Lexer.getSymbol()
				while sym == .ident
				{
					let symbolInfo = SymbolTable.insert(
						named: Lexer.identifier,
						kind: .constant
					)
					sym = Lexer.getSymbol()
					if sym == .eql {
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark("=?") }
					parseExpression(&x)
					if x.mode == .constant
					{
						symbolInfo!.value = x.a
						symbolInfo!.type = x.type
					}
					else { Lexer.mark("expression not constant") }
					if sym == .semicolon {
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark(";?") }
				}
			}
			if sym == .type
			{
				sym = Lexer.getSymbol()
				while sym == .ident
				{
					let symbolInfo = SymbolTable.insert(
						named: Lexer.identifier,
						kind: .type
					)
					sym = Lexer.getSymbol()
					if sym == .eql {
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark("=?") }
					if let symInfo = symbolInfo {
						symInfo.type = parseType()
					}
					if sym == .semicolon {
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark(";?") }
				}
			}
			if sym == .var
			{
				sym = Lexer.getSymbol()
				while sym == .ident
				{
					let first = parseIdentifierList(.variable)
					let tp = parseType()
					obj = first
					while obj != SymbolTable.sentinel
					{
						obj!.symbolInfo.type = tp
						obj!.symbolInfo.level = CodeGen.curlev
						varsize = varsize + Int( obj!.symbolInfo.type!.size)
						obj!.symbolInfo.value = -varsize
						obj = obj!.next
					}
					if sym == .semicolon {
						sym = Lexer.getSymbol()
					}
					else { Lexer.mark("; ?") }
				}
			}
			if (sym >= .const) && (sym <= .var) {
				Lexer.mark("declaration?")
			}
			else { break }
		}
		
		return varsize
	}

	// ---------------------------------------------------
	/*
	FIXME: Need a better name.  I *think* FPSection refers to "frame
	pointer section".  Wirth doesn't talk about what this functon is doing
	in Compiler Construction, so I'm having to infer the meaning from the
	code, since the name doesn't tell me anything.  It looks like it's
	computing the stack space needed for the parameters, which is part of
	setting up the frame pointer.  It also looks like Oberon uses the Pascal
	calling convention (ie. the subroutine is responsible for cleaning
	up the stack allocated to the parameters, as opposed to the C calling
	convention where the caller is responsible).  That's not surprising
	since Wirth created Pascal, and Oberon is a descendent of it.
	*/
	private static func FPSection(_ startingParameterBlockSize: Int) -> Int
	{
		// ---------------------------------------------------
		func getType(for symbol: OberonSymbol) -> TypeInfo?
		{
			if sym == .ident
			{
				let obj = SymbolTable.find(name: Lexer.identifier)
				sym = Lexer.getSymbol()
				if obj!.symbolInfo.kind == .type {
					return obj!.symbolInfo.type
				}
			}

			Lexer.mark("ident?")
			return CodeGen.intType
		}
		
		var first: SymbolTable.ListNode?
		if sym == .var
		{
			sym = Lexer.getSymbol()
			first = parseIdentifierList(.parameter)
		}
		else { first = parseIdentifierList(.variable) }
		
		let tp = getType(for: sym)
		
		let parsize: Int
		if first!.symbolInfo.kind == .variable
		{
			parsize = tp!.size
			if tp!.form >= .array {
				Lexer.mark("no struct params")
			}
		}
		else {
			parsize = WordSize
		}
		
		var parameterBlockSize = startingParameterBlockSize
		var obj = first
		while obj !== SymbolTable.sentinel
		{
			obj!.symbolInfo.type = tp
			parameterBlockSize += parsize
			obj = obj!.next
		}
		
		return parameterBlockSize
	}
	
	// ---------------------------------------------------
	private static func parseParameterList(_ startingParameterBlockSize: Int) -> Int
	{
		var parameterBlockSize = startingParameterBlockSize
		sym = Lexer.getSymbol()
		if sym == .rparen {
			sym = Lexer.getSymbol()
		}
		else
		{
			parameterBlockSize = FPSection(parameterBlockSize)
			while sym == .semicolon
			{
				sym = Lexer.getSymbol()
				parameterBlockSize = FPSection(parameterBlockSize)
			}
			
			if sym == .rparen {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(")?") }
		}
		
		return parameterBlockSize
	}
	
	// ---------------------------------------------------
	private static func setLocalBlockSizeInSymbolTable(_ parameterBlockSize: Int)
	{
		var obj = SymbolTable.topScope!.next
		var localBlockSize = parameterBlockSize
		
		while obj != SymbolTable.sentinel
		{
			obj!.symbolInfo.level = CodeGen.curlev
			if obj!.symbolInfo.kind == .parameter {
				localBlockSize -= WordSize
			}
			else {
				localBlockSize -= Int(obj!.symbolInfo.type!.size)
			}
			obj!.symbolInfo.value = localBlockSize
			obj = obj!.next
		}
	}
	
	// ---------------------------------------------------
	private static func parseNestedProcedures()
	{
		while sym == .procedure
		{
			parseProcedureDeclaration()
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private static func parseProcedureBody(
		_ procInfo: SymbolInfo,
		_ localBlockSize: Int,
		_ parameterBlockSize: Int,
		_ markSize: Int) -> SymbolInfo
	{
		procInfo.value = Int(CodeGen.pc)
		CodeGen.enter(localBlockSize)
		
		if sym == .begin
		{
			sym = Lexer.getSymbol()
			parseStatementSequence()
		}
		else
		{
			Lexer.mark(
				"Expected BEGIN for procedure, \(procInfo.name)"
			)
		}
		
		if sym == .end {
			sym = Lexer.getSymbol()
		}
		else
		{
			Lexer.mark(
				"Expected END for procedure, \(procInfo.name)"
			)
		}
		
		if sym == .ident
		{
			if procInfo.name != Lexer.identifier
			{
				Lexer.mark(
					"Procedure end identifier, \(Lexer.identifier), "
					+ "doesn't match procedure name, \(procInfo.name)")
			}
			sym = Lexer.getSymbol()
		}
		
		CodeGen.procedureReturn(parameterBlockSize - markSize)
		
		return procInfo
	}
	
	// ---------------------------------------------------
	private static func openProcedureDeclaration(_ markSize: Int)
		-> (proc: SymbolTable.ListNode?, parameterBlockSize: Int)
	{
		let proc = SymbolTable.newNode(
			named: Lexer.identifier,
			kind: .procedure
		)
		sym = Lexer.getSymbol()
		CodeGen.IncLevel(1)
		SymbolTable.openScope()
		proc!.symbolInfo.value = -1
		
		var parameterBlockSize = markSize

		if sym == .lparen {
			parameterBlockSize = parseParameterList(parameterBlockSize)
		}
		else if CodeGen.curlev == 1 {
			CodeGen.enterCmd(proc!.symbolInfo.name)
		}
		
		setLocalBlockSizeInSymbolTable(parameterBlockSize)
		
		proc!.parentScope = SymbolTable.topScope!.next
		
		if sym == .semicolon {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(";?") }
		
		return (proc, parameterBlockSize)
	}
	
	// ---------------------------------------------------
	private static func closeProcedureDeclaration()
	{
		SymbolTable.closeScope()
		CodeGen.IncLevel(-1)
	}
	
	// ---------------------------------------------------
	private static func parseProcedureDeclaration()
	{
		// ---------------------------------------------------
		// ProcedureDecl
		sym = Lexer.getSymbol()
		if sym == .ident
		{
			let markSize: Int = 8
			
			let (proc, parameterBlockSize) = openProcedureDeclaration(markSize)
			defer { closeProcedureDeclaration() }
									
			let localBlockSize = parseDeclarations(0)
			parseNestedProcedures()
			proc!.symbolInfo = parseProcedureBody(
				proc!.symbolInfo,
				localBlockSize,
				parameterBlockSize,
				markSize
			)
		}
		else {
			Lexer.mark("Expected procedure name.")
		}
	}

	// ---------------------------------------------------
	private static func parseModule()
	{
		var moduleName = ""

		print(" compiling ", terminator: "", to: &OberonLog)
		if sym == .module
		{
			sym = Lexer.getSymbol()
			CodeGen.open()
			SymbolTable.openScope()
			var varsize = 0
			if sym == .ident
			{
				moduleName = Lexer.identifier
				sym = Lexer.getSymbol()
				print("\(moduleName)", to: &OberonLog)
			}
			else { Lexer.mark("ident?") }
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(";?") }
			
			varsize = parseDeclarations(varsize)
			while sym == .procedure
			{
				parseProcedureDeclaration()
				if sym == .semicolon {
					sym = Lexer.getSymbol()
				}
				else { Lexer.mark(";?") }
			}
			CodeGen.header(varsize)
			if sym == .begin
			{
				sym = Lexer.getSymbol()
				parseStatementSequence()
			}
			if sym == .end {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("END?") }
			if sym == .ident
			{
				if moduleName != Lexer.identifier {
					Lexer.mark("no match")
				}
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("ident?") }
			if sym != .period {
				Lexer.mark(". ?")
			}
			SymbolTable.closeScope()
			if !Lexer.error
			{
				CodeGen.close()
				print("code generated\(CodeGen.pc, pad: 6)", to: &OberonLog)
			}
		}
		else { Lexer.mark("MODULE?") }
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
		let objCode = CodeGen.getObjectCode()
		var program = [UInt32](capacity: objCode.count + 2)
		program.append(Oberon0Parser.magic)
		program.append(UInt32(CodeGen.entry * 4))
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
		Lexer.Init(sourceStream: sourceStream)
		sym = Lexer.getSymbol()
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
		CodeGen.decode(to: &result)
		return result.isEmpty ? "!!!!! NO OUTPUT !!!!!" : result.description
	}
}


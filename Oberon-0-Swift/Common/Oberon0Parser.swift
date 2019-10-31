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
	
	internal static var globalScope = SymbolScope.makeGlobalScope()
	internal static var currentScope = globalScope

	// MARK:- Parser
	// ---------------------------------------------------
	private static func arrayElementSelector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		sym = Lexer.getSymbol()
		let y = parseExpression()
		
		if x.type!.form == .array {
			x.index(at: y)
		}
		else { Lexer.mark("not an array") }
		
		if sym == .rbrak {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("Expected \"]\"") }
		
		return x
	}
	
	// ---------------------------------------------------
	private static func recordFieldSelector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		
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
				else {
					Lexer.mark("Undefined record field, \(Lexer.identifier)")
				}
				
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("Not a record") }
		}
		else { Lexer.mark("Expected an identifier") }
		
		return x
	}
	
	// ---------------------------------------------------
	private static func selector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		while (sym == .lbrak) || (sym == .period)
		{
			if sym == .lbrak {
				x = arrayElementSelector(x)
			}
			else {
				x = recordFieldSelector(x)
			}
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func factor(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
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
			let identifierInfo =
				currentScope.hierarchy[Lexer.identifier]
			sym = Lexer.getSymbol()
			x = CodeGen.makeItem(identifierInfo!)
			x = selector(x)
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
			x = parseExpression()
			if sym == .rparen {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(")?") }
		}
		else if sym == .not
		{
			sym = Lexer.getSymbol()
			x = factor(x)
			CodeGen.Op1(.not, &x)
		}
		else
		{
			Lexer.mark("factor?")
			x = CodeGen.makeDefaultItem()
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseTerminalSymbol(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		var y = CodeGen.Item()
		var op: OberonSymbol;
		
		x = factor(x)
		while (sym >= .times) && (sym <= .and)
		{
			op = sym
			sym = Lexer.getSymbol()
			if op == .and {
				CodeGen.Op1(op, &x)
			}
			y = factor(y)
			CodeGen.Op2(op, &x, &y)
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseSimpleExpression(_ x: CodeGen.Item)
		-> CodeGen.Item
	{
		var x = x
		var op: OberonSymbol
		
		if sym == .plus
		{
			sym = Lexer.getSymbol()
			x = parseTerminalSymbol(x)
		}
		else if sym == .minus
		{
			sym = Lexer.getSymbol()
			x = parseTerminalSymbol(x)
			CodeGen.Op1(.minus, &x)
		}
		else {
			x = parseTerminalSymbol(x)
		}
		while (sym >= .plus) && (sym <= .or)
		{
			op = sym
			sym = Lexer.getSymbol()
			if op == .or {
				CodeGen.Op1(op, &x)
			}
			var y = parseTerminalSymbol(CodeGen.Item())
			CodeGen.Op2(op, &x, &y)
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseExpression() -> CodeGen.Item
	{
		var op: OberonSymbol
		
		var x = parseSimpleExpression(CodeGen.Item())
		if (sym >= .eql) && (sym <= .gtr)
		{
			op = sym
			sym = Lexer.getSymbol()
			var y = parseSimpleExpression(CodeGen.Item())
			CodeGen.Relation(op, &x, &y)
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseParameter(_ fp: SymbolInfo) -> Bool
	{
		var x = CodeGen.Item()
		
		x = parseExpression()
		if fp.isParameter
		{
			CodeGen.Parameter(&x, fp)
			return true
		}

		Lexer.mark("Too many parameters")
		return false
	}

	// ---------------------------------------------------
	private static func param() -> CodeGen.Item
	{
		if sym == .lparen {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(")?") }
		let x = parseExpression()
		if sym == .rparen {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(")?") }
		
		return x
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
		y = parseExpression()
		var newX = x
		CodeGen.Store(&newX, &y)
	}
	
	// ---------------------------------------------------
	private static func parseErroneousEquality()
	{
		sym = Lexer.getSymbol()
		let _ = parseExpression()
	}
	
	// ---------------------------------------------------
	/**
	- Returns: `true` if a symbol has been encountered that is not a parameter, indicating that all
		parameters have been parsed, or `false` otherwise.
	*/
	private static func parseActualParameters(for procedure: SymbolInfo) -> Bool
	{
		assert(procedure.ownedScope != nil)
		let procedureScope = procedure.ownedScope!
		
		sym = Lexer.getSymbol()
		if sym == .rparen
		{
			sym = Lexer.getSymbol()
			return true
		}
		else
		{
			var isAParameter = false
			loop: for par in procedureScope.symbols
			{
				isAParameter = parseParameter(par)
				switch sym
				{
					case .comma:
						sym = Lexer.getSymbol()
						
					case .rparen:
						sym = Lexer.getSymbol();
						return true
						
					default:
						if sym >= .semicolon { break loop }
						Lexer.mark("Expected \")\" or \",\"")
				}
				
				if !isAParameter { break loop }
			}
			
			return !isAParameter
		}
	}
	
	// ---------------------------------------------------
	private static func parseProcedureCall(
		procedureInfo procInfo: SymbolInfo,
		_ x: CodeGen.Item)
	{
		var allParametersParsed = true
		if sym == .lparen {
			allParametersParsed = parseActualParameters(for: procInfo)
		}
		
		if procInfo.value < 0 {
			Lexer.mark("forward call")
		}
		else if allParametersParsed
		{
			var newX = x
			CodeGen.call(&newX)
		}
		else { Lexer.mark("too few parameters") }
	}
	
	// ---------------------------------------------------
	private static func parseStandardProcedureCall(
		_ procInfo: SymbolInfo,
		_ x: CodeGen.Item)
	{
		var y = CodeGen.Item()
		if procInfo.value <= 3 {
			y = param()
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
		x = parseExpression()
		CodeGen.conditionalJump(&x)
		parseThen()
		var L = 0
		
		while sym == .elsif
		{
			sym = Lexer.getSymbol()
			CodeGen.jumpForward(&L)
			CodeGen.fixLink(x.a)
			x = parseExpression()
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
		x = parseExpression()
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
					let identiferInfo =
						currentScope.hierarchy[Lexer.identifier]!
					sym = Lexer.getSymbol()
					
					var x = CodeGen.makeItem(identiferInfo)
					x = selector(x)
					
					if sym == .becomes {
						parseAssignment(x)
					}
					else if sym == .eql
					{
						Lexer.mark(":= ?")
						parseErroneousEquality()
					}
					else if x.mode == .procedure {
						parseProcedureCall(procedureInfo: identiferInfo, x)
					}
					else if x.mode == .standardProcedure {
						parseStandardProcedureCall(identiferInfo, x)
					}
					else if identiferInfo.kind == .type {
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
	private static func parseIdentifierListAsArray(
		symbol: inout OberonSymbol,
		_ kind: SymbolInfo.Kind) -> [SymbolInfo]
	{
		var fields = [SymbolInfo]()
		
		if symbol == .ident
		{
			fields.append(
				SymbolInfo(name: Lexer.identifier, kind: kind)
			)

			symbol = Lexer.getSymbol()
			while symbol == .comma
			{
				symbol = Lexer.getSymbol()
				if symbol == .ident
				{
					fields.append(
						SymbolInfo(name: Lexer.identifier, kind: kind)
					)
					symbol = Lexer.getSymbol()
				}
				else { Lexer.mark("expected field identifier") }
			}
			if symbol == .colon {
				symbol = Lexer.getSymbol()
			}
			else { Lexer.mark("expected \":\"") }
		}
		
		return fields
	}
	
	// ---------------------------------------------------
	private static func parseRecordTypeDeclaration() -> TypeInfo
	{
		sym = Lexer.getSymbol()
		let type = TypeInfo()
		type.form = .record
		type.size = 0
		
		currentScope = currentScope.openScope()

		while true
		{
			if sym == .ident
			{
				let fields = parseIdentifierListAsArray(symbol: &sym, .field)
				let tp = parseType()
				
				for field in fields
				{
					field.type = tp
					field.value = type.size
					type.size += field.type!.size
				}
				
				type.fields = fields
			}
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else if sym == .ident {
				Lexer.mark("Expected \":\"")
			}
			else { break }
		}
		
		currentScope = currentScope.closeScope()

		if sym == .end {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("Expected END for record") }
		
		return type
	}
	
	// ---------------------------------------------------
	private static func parseArrayDeclaration() -> TypeInfo
	{
		sym = Lexer.getSymbol()
		var x = CodeGen.Item()
		x = parseExpression()
		if (x.mode != .constant) || (x.a < 0) {
			Lexer.mark("bad index")
		}
		if sym == .of {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark("Expected OF") }
		
		let tp = parseType()
		let type = TypeInfo()
		type.form = .array
		type.base = tp
		type.len = x.a
		type.size = type.len * tp!.size
		
		return type
	}
	
	// ---------------------------------------------------
	private static func parseTypeAlias() -> TypeInfo
	{
		let symbolInfo = currentScope.hierarchy[Lexer.identifier]
		sym = Lexer.getSymbol()
		
		if let symInfo = symbolInfo, symInfo.kind == .type {
			return symInfo.type!
		}
		else { Lexer.mark("Expected a type") }
		
		return CodeGen.intType
	}

	// ---------------------------------------------------
	private static func parseType() -> TypeInfo?
	{
		if (sym != .ident) && (sym < .array)
		{
			Lexer.mark("Expected a type")
			repeat {
				sym = Lexer.getSymbol()
			} while !((sym == .ident) || (sym >= .array))
		}
		
		switch sym
		{
			case .ident: return parseTypeAlias()
			case .array: return parseArrayDeclaration()
			case .record: return parseRecordTypeDeclaration()
			default: Lexer.mark("Expected an identifier")
		}
		
		return CodeGen.intType // sync
	}
	
	// ---------------------------------------------------
	private static func parseConstantDeclarations()
	{
		sym = Lexer.getSymbol()
		while sym == .ident
		{
			let symbolInfo = currentScope.defineSymbol(
				named: Lexer.identifier,
				kind: .constant
			)

			sym = Lexer.getSymbol()
			if sym == .eql {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("=?") }
			
			var x = CodeGen.Item()
			x = parseExpression()
			if x.mode == .constant
			{
				symbolInfo.value = x.a
				symbolInfo.type = x.type
			}
			else { Lexer.mark("expression not constant") }
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private static func parseTypeDeclarations()
	{
		sym = Lexer.getSymbol()
		while sym == .ident
		{
			let symbolInfo = currentScope.defineSymbol(
				named: Lexer.identifier,
				kind: .type
			)
			
			sym = Lexer.getSymbol()
			if sym == .eql {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("=?") }
			
			symbolInfo.type = parseType()
			
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private static func parseVariableDeclarations(varsize: Int) -> Int
	{
		var varsize = varsize
		
		sym = Lexer.getSymbol()
		while sym == .ident
		{
			let variables = parseIdentifierListAsArray(symbol: &sym, .variable)
			let tp = parseType()
			
			for variable in variables
			{
				variable.type = tp
				variable.level = CodeGen.curlev
				varsize = varsize + variable.type!.size
				variable.value = -varsize
			}
			
			if sym == .semicolon {
				sym = Lexer.getSymbol()
			}
			else { Lexer.mark("; ?") }
			
			currentScope.append(variables)
		}
		
		return varsize
	}

	// ---------------------------------------------------
	private static func parseDeclarations(_ varsize: Int) -> Int
	{
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
			if sym == .const {
				parseConstantDeclarations()
			}
			
			if sym == .type {
				parseTypeDeclarations()
			}
			
			if sym == .var {
				varsize = parseVariableDeclarations(varsize: varsize)
			}
			
			if (sym >= .const) && (sym <= .var) {
				Lexer.mark("Expected declaration (ie. CONST, VAR or TYPE)")
			}
			else { break }
		}
		
		return varsize
	}

	// ---------------------------------------------------
	private static func parseParameterList(for symbol: inout OberonSymbol)
		-> [SymbolInfo]
	{
		if symbol == .var
		{
			symbol = Lexer.getSymbol()
			return parseIdentifierListAsArray(symbol: &symbol, .parameter)
		}
		
		return parseIdentifierListAsArray(symbol: &symbol, .variable)
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
				let identifierInfo = currentScope.hierarchy[Lexer.identifier]
				sym = Lexer.getSymbol()
				
				if identifierInfo?.kind == .type {
					return identifierInfo!.type
				}
			}

			Lexer.mark("Expected identifier")
			return CodeGen.intType
		}
		
		let parameters = parseParameterList(for: &sym)

		let tp = getType(for: sym)
		
		let parsize: Int
		if parameters.first!.kind == .variable
		{
			parsize = tp!.size
			if tp!.form >= .array {
				Lexer.mark("Struct parameters are not supported")
			}
		}
		else { parsize = WordSize }
		
		var parameterBlockSize = startingParameterBlockSize
		
		for parameter in parameters
		{
			parameter.type = tp
			parameterBlockSize += parsize
		}
		
		currentScope.append(parameters)
		
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
		var localBlockSize = parameterBlockSize
		
		currentScope.modifyEach
		{
			$0.level = CodeGen.curlev
			if $0.kind == .parameter {
				localBlockSize -= WordSize
			}
			else {
				localBlockSize -= Int($0.type!.size)
			}
			$0.value = localBlockSize
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
		procedureInfo procInfo: SymbolInfo,
		localBlockSize: Int,
		parameterBlockSize: Int,
		markSize: Int) -> SymbolInfo
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
		-> (proc: SymbolInfo, parameterBlockSize: Int)
	{
		let proc = currentScope.defineSymbol(
			named: Lexer.identifier,
			kind: .procedure
		)
		sym = Lexer.getSymbol()
		CodeGen.IncLevel(1)
		currentScope = currentScope.openScope()
		proc.value = -1
		
		var parameterBlockSize = markSize

		if sym == .lparen {
			parameterBlockSize = parseParameterList(parameterBlockSize)
		}
		else if CodeGen.curlev == 1 {
			CodeGen.enterCmd(proc.name)
		}
		
		setLocalBlockSizeInSymbolTable(parameterBlockSize)
		
		proc.ownedScope = currentScope

		if sym == .semicolon {
			sym = Lexer.getSymbol()
		}
		else { Lexer.mark(";?") }
		
		return (proc, parameterBlockSize)
	}
	
	// ---------------------------------------------------
	private static func closeProcedureDeclaration()
	{
		currentScope = currentScope.closeScope()
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
			let _ = parseProcedureBody(
				procedureInfo: proc,
				localBlockSize: localBlockSize,
				parameterBlockSize: parameterBlockSize,
				markSize: markSize
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
			currentScope = currentScope.openScope()
			
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
			
			let varsize = parseDeclarations(0)
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
			
			currentScope = currentScope.closeScope()
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


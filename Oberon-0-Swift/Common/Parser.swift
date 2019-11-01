//
//  Oberon0Parser.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct Parser
{
	typealias CodeGen = RISCCodeGenerator
	
	internal static let WordSize:Int = 4

	internal static var currentToken: Token = Token(.null)
	internal static var loaded: Bool = false
	
	internal static var globalScope = SymbolScope.makeGlobalScope()
	internal static var currentScope = globalScope
	internal static var standardOutput = FileHandleOutputStream(FileHandle.standardOutput)
	
	private static var codeGenerator = RISCCodeGenerator()

	// MARK:- Parser
	// ---------------------------------------------------
	private static func arrayElementSelector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		currentToken = Lexer.getToken()
		let y = parseExpression()
		
		if x.type!.form == .array {
			x.index(at: y, for: &codeGenerator)
		}
		else { Lexer.mark("not an array") }
		
		if currentToken.symbol == .closeBracket {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("Expected \"]\"") }
		
		return x
	}
	
	// ---------------------------------------------------
	private static func recordFieldSelector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		
		currentToken = Lexer.getToken()
		if currentToken.symbol == .identifier
		{
			if x.type!.form == .record
			{
				if let fieldInfo = x.type!.symbolInfoForField(
					named: currentToken.identifier)
				{
					x.setFieldInfo(from: fieldInfo)
				}
				else
				{
					Lexer.mark(
						"Undefined record field, \(currentToken.identifier)"
					)
				}
				
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark("Not a record") }
		}
		else { Lexer.mark("Expected an identifier") }
		
		return x
	}
	
	fileprivate static let selectors: [TokenType] = [.openBracket, .period]
	// ---------------------------------------------------
	private static func selector(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		while Parser.selectors.contains(currentToken.symbol)
		{
			if currentToken.symbol == .openBracket {
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
		if currentToken.symbol < .openParen
		{
			Lexer.mark("ident?")
			repeat {
				currentToken = Lexer.getToken()
			} while !(currentToken.symbol >= .openParen)
		}
		
		switch currentToken.symbol
		{
			case .identifier:
				let identifierInfo =
					currentScope.hierarchy[currentToken.identifier]
				currentToken = Lexer.getToken()
				x = codeGenerator.makeItem(identifierInfo!)
				x = selector(x)
			case .number:
				x = codeGenerator.makeConstItem(
					CodeGen.intType,
					currentToken.value
				)
				currentToken = Lexer.getToken()
			case .openParen:
				currentToken = Lexer.getToken()
				x = parseExpression()
				if currentToken.symbol == .closeParen {
					currentToken = Lexer.getToken()
				}
				else { Lexer.mark(")?") }
			case .not:
				currentToken = Lexer.getToken()
				x = factor(x)
				codeGenerator.Op1(.not, &x)
			default:
				Lexer.mark("factor?")
				x = codeGenerator.makeDefaultItem()
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseTerminalSymbol(_ x: CodeGen.Item) -> CodeGen.Item
	{
		var x = x
		var y = CodeGen.Item()
		var op: TokenType;
		
		x = factor(x)
		while (currentToken.symbol >= .times) && (currentToken.symbol <= .and)
		{
			op = currentToken.symbol
			currentToken = Lexer.getToken()
			if op == TokenType.and {
				codeGenerator.Op1(op, &x)
			}
			y = factor(y)
			codeGenerator.Op2(op, &x, &y)
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseSimpleExpression(_ x: CodeGen.Item)
		-> CodeGen.Item
	{
		var x = x
		var op: TokenType
		
		if currentToken.symbol == .plus
		{
			currentToken = Lexer.getToken()
			x = parseTerminalSymbol(x)
		}
		else if currentToken.symbol == .minus
		{
			currentToken = Lexer.getToken()
			x = parseTerminalSymbol(x)
			codeGenerator.Op1(.minus, &x)
		}
		else {
			x = parseTerminalSymbol(x)
		}
		while (currentToken.symbol >= .plus) && (currentToken.symbol <= .or)
		{
			op = currentToken.symbol
			currentToken = Lexer.getToken()
			if op == TokenType.or {
				codeGenerator.Op1(op, &x)
			}
			var y = parseTerminalSymbol(CodeGen.Item())
			codeGenerator.Op2(op, &x, &y)
		}
		
		return x
	}

	// ---------------------------------------------------
	private static func parseExpression() -> CodeGen.Item
	{
		var op: TokenType
		
		var x = parseSimpleExpression(CodeGen.Item())
		if (currentToken.symbol >= .isEqualTo)
			&& (currentToken.symbol <= .greaterThan)
		{
			op = currentToken.symbol
			currentToken = Lexer.getToken()
			var y = parseSimpleExpression(CodeGen.Item())
			codeGenerator.relation(op, &x, &y)
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
			codeGenerator.parameter(&x, fp)
			return true
		}

		Lexer.mark("Too many parameters")
		return false
	}

	// ---------------------------------------------------
	private static func param() -> CodeGen.Item
	{
		if currentToken.symbol == .openParen {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark(")?") }
		
		let x = parseExpression()
		if currentToken.symbol == .closeParen {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark(")?") }
		
		return x
	}
	
	// ---------------------------------------------------
	private static func advanceLexerToAtLeastIdentifier() -> Token
	{
		var token = Token.null
		repeat {
			token = Lexer.getToken()
		} while token.symbol < .identifier
		
		return token
	}
	
	// ---------------------------------------------------
	private static func parseAssignment(_ x: CodeGen.Item)
	{
		currentToken = Lexer.getToken()
		var y = CodeGen.Item()
		y = parseExpression()
		var newX = x
		codeGenerator.store(&newX, &y)
	}
	
	// ---------------------------------------------------
	private static func parseErroneousEquality()
	{
		currentToken = Lexer.getToken()
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
		
		currentToken = Lexer.getToken()
		if currentToken.symbol == .closeParen
		{
			currentToken = Lexer.getToken()
			return true
		}
		else
		{
			var isAParameter = false
			loop: for par in procedureScope.symbols
			{
				isAParameter = parseParameter(par)
				switch currentToken.symbol
				{
					case .comma:
						currentToken = Lexer.getToken()
						
					case .closeParen:
						currentToken = Lexer.getToken();
						return true
						
					default:
						if currentToken.symbol >= .semicolon { break loop }
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
		if currentToken.symbol == .openParen {
			allParametersParsed = parseActualParameters(for: procInfo)
		}
		
		if procInfo.value < 0 {
			Lexer.mark("forward call")
		}
		else if allParametersParsed
		{
			var newX = x
			codeGenerator.call(&newX)
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
		codeGenerator.ioCall(&newX, &y)
	}

	// ---------------------------------------------------
	private static func parseThen()
	{
		if currentToken.symbol == .then {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("THEN?") }
		parseStatementSequence()
	}
	
	// ---------------------------------------------------
	private static func parseIfStatement()
	{
		currentToken = Lexer.getToken()
		var x = CodeGen.Item()
		x = parseExpression()
		codeGenerator.conditionalJump(&x)
		parseThen()
		var L = 0
		
		while currentToken.symbol == .elsif
		{
			currentToken = Lexer.getToken()
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			x = parseExpression()
			codeGenerator.conditionalJump(&x)
			parseThen()
		}
		
		if currentToken.symbol == .else
		{
			currentToken = Lexer.getToken()
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			parseStatementSequence()
		}
		else {
			codeGenerator.fixLink(x.a)
		}
		
		codeGenerator.fixLink(L)
		
		if currentToken.symbol == .end {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private static func parseWhileStatement()
	{
		currentToken = Lexer.getToken()
		let L = Int(codeGenerator.pc)
		var x = CodeGen.Item()
		x = parseExpression()
		codeGenerator.conditionalJump(&x)
		
		if currentToken.symbol == .do {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("DO?") }
		
		parseStatementSequence()
		codeGenerator.jumpBack(L)
		codeGenerator.fixLink(x.a)
		
		if currentToken.symbol == .end {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private static func parseStatementSequence()
	{
		// ---------------------------------------------------
		while true // sync
		{
			if currentToken.symbol < .identifier
			{
				Lexer.mark("statement?")
				currentToken = advanceLexerToAtLeastIdentifier()
			}
			
			switch currentToken.symbol
			{
				case .identifier:
					let identiferInfo =
						currentScope.hierarchy[currentToken.identifier]!
					currentToken = Lexer.getToken()
					
					var x = codeGenerator.makeItem(identiferInfo)
					x = selector(x)
					
					if currentToken.symbol == .becomes {
						parseAssignment(x)
					}
					else if currentToken.symbol == .isEqualTo
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

			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
			}
			else if (currentToken.symbol >= .semicolon)
				&& (currentToken.symbol < .if)
				|| (currentToken.symbol >= .array)
			{
				break
			}
			else { Lexer.mark("; ?") }
		}
	}

	// ---------------------------------------------------
	private static func parseIdentifierListAsArray(
		token: inout Token,
		_ kind: SymbolInfo.Kind) -> [SymbolInfo]
	{
		var fields = [SymbolInfo]()
		
		if token.symbol == .identifier
		{
			fields.append(SymbolInfo(name: token.identifier, kind: kind))

			token = Lexer.getToken()
			while token.symbol == .comma
			{
				token = Lexer.getToken()
				if token.symbol == .identifier
				{
					fields.append(
						SymbolInfo(name: token.identifier, kind: kind)
					)
					token = Lexer.getToken()
				}
				else { Lexer.mark("expected field identifier") }
			}
			if token.symbol == .colon {
				token = Lexer.getToken()
			}
			else { Lexer.mark("expected \":\"") }
		}
		
		return fields
	}
	
	// ---------------------------------------------------
	private static func parseRecordTypeDeclaration() -> TypeInfo
	{
		currentToken = Lexer.getToken()
		let type = TypeInfo()
		type.form = .record
		type.size = 0
		
		currentScope = currentScope.openScope()

		while true
		{
			if currentToken.symbol == .identifier
			{
				let fields =
					parseIdentifierListAsArray(token: &currentToken, .field)
				let tp = parseType()
				
				for field in fields
				{
					field.type = tp
					field.value = type.size
					type.size += field.type!.size
				}
				
				type.fields = fields
			}
			
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
			}
			else if currentToken.symbol == .identifier {
				Lexer.mark("Expected \":\"")
			}
			else { break }
		}
		
		currentScope = currentScope.closeScope()

		if currentToken.symbol == .end {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark("Expected END for record") }
		
		return type
	}
	
	// ---------------------------------------------------
	private static func parseArrayDeclaration() -> TypeInfo
	{
		currentToken = Lexer.getToken()
		var x = CodeGen.Item()
		x = parseExpression()
		
		if (x.mode != .constant) || (x.a < 0) {
			Lexer.mark("bad index")
		}
		
		if currentToken.symbol == .of {
			currentToken = Lexer.getToken()
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
		let symbolInfo = currentScope.hierarchy[currentToken.identifier]
		currentToken = Lexer.getToken()
		
		if let symInfo = symbolInfo, symInfo.kind == .type {
			return symInfo.type!
		}
		else { Lexer.mark("Expected a type") }
		
		return CodeGen.intType
	}

	// ---------------------------------------------------
	private static func parseType() -> TypeInfo?
	{
		if (currentToken.symbol != .identifier) && (currentToken.symbol < .array)
		{
			Lexer.mark("Expected a type")
			repeat {
				currentToken = Lexer.getToken()
			} while currentToken.symbol != .identifier
				&& currentToken.symbol < .array
		}
		
		switch currentToken.symbol
		{
			case .identifier: return parseTypeAlias()
			case .array: return parseArrayDeclaration()
			case .record: return parseRecordTypeDeclaration()
			default: Lexer.mark("Expected an identifier")
		}
		
		return CodeGen.intType // sync
	}
	
	// ---------------------------------------------------
	private static func parseConstantDeclarations()
	{
		currentToken = Lexer.getToken()
		while currentToken.symbol == .identifier
		{
			let symbolInfo = currentScope.defineSymbol(
				named: currentToken.identifier,
				kind: .constant
			)

			currentToken = Lexer.getToken()
			if currentToken.symbol == .isEqualTo {
				currentToken = Lexer.getToken()
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
			
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private static func parseTypeDeclarations()
	{
		currentToken = Lexer.getToken()
		while currentToken.symbol == .identifier
		{
			let symbolInfo = currentScope.defineSymbol(
				named: currentToken.identifier,
				kind: .type
			)
			
			currentToken = Lexer.getToken()
			if currentToken.symbol == .isEqualTo {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark("=?") }
			
			symbolInfo.type = parseType()
			
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private static func parseVariableDeclarations(varsize: Int) -> Int
	{
		var varsize = varsize
		
		currentToken = Lexer.getToken()
		while currentToken.symbol == .identifier
		{
			let variables =
				parseIdentifierListAsArray(token: &currentToken, .variable)
			let tp = parseType()
			
			for variable in variables
			{
				variable.type = tp
				variable.level = codeGenerator.curlev
				varsize = varsize + variable.type!.size
				variable.value = -varsize
			}
			
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
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
		if currentToken.symbol < .const && currentToken.symbol != .end
		{
			Lexer.mark("declaration?")
			repeat {
				currentToken = Lexer.getToken()
			} while currentToken.symbol < .const && currentToken.symbol != .end
		}
		
		while true
		{
			if currentToken.symbol == .const {
				parseConstantDeclarations()
			}
			
			if currentToken.symbol == .type {
				parseTypeDeclarations()
			}
			
			if currentToken.symbol == .var {
				varsize = parseVariableDeclarations(varsize: varsize)
			}
			
			if (currentToken.symbol >= .const) && (currentToken.symbol <= .var)
			{
				Lexer.mark("Expected declaration (ie. CONST, VAR or TYPE)")
			}
			else { break }
		}
		
		return varsize
	}

	// ---------------------------------------------------
	private static func parseParameterList(for token: inout Token)
		-> [SymbolInfo]
	{
		if token.symbol == .var
		{
			token = Lexer.getToken()
			return parseIdentifierListAsArray(token: &token, .parameter)
		}
		
		return parseIdentifierListAsArray(token: &token, .variable)
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
		func getType(for token: inout Token) -> TypeInfo?
		{
			if token.symbol == .identifier
			{
				let identifierInfo = currentScope.hierarchy[token.identifier]
				token = Lexer.getToken()
				
				if identifierInfo?.kind == .type {
					return identifierInfo!.type
				}
			}

			Lexer.mark("Expected identifier")
			return CodeGen.intType
		}
		
		let parameters = parseParameterList(for: &currentToken)

		let tp = getType(for: &currentToken)
		
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
	private static func parseParameterList(
		_ startingParameterBlockSize: Int) -> Int
	{
		var parameterBlockSize = startingParameterBlockSize
		currentToken = Lexer.getToken()
		if currentToken.symbol == .closeParen {
			currentToken = Lexer.getToken()
		}
		else
		{
			parameterBlockSize = FPSection(parameterBlockSize)
			while currentToken.symbol == .semicolon
			{
				currentToken = Lexer.getToken()
				parameterBlockSize = FPSection(parameterBlockSize)
			}
			
			if currentToken.symbol == .closeParen {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark(")?") }
		}
		
		return parameterBlockSize
	}
	
	// ---------------------------------------------------
	private static func setLocalBlockSizeInSymbolTable(
		_ parameterBlockSize: Int)
	{
		var localBlockSize = parameterBlockSize
		
		currentScope.modifyEach
		{
			$0.level = codeGenerator.curlev
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
		while currentToken.symbol == .procedure
		{
			parseProcedureDeclaration()
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
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
		procInfo.value = Int(codeGenerator.pc)
		codeGenerator.enter(localBlockSize)
		
		if currentToken.symbol == .begin
		{
			currentToken = Lexer.getToken()
			parseStatementSequence()
		}
		else
		{
			Lexer.mark(
				"Expected BEGIN for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .end {
			currentToken = Lexer.getToken()
		}
		else
		{
			Lexer.mark(
				"Expected END for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .identifier
		{
			if procInfo.name != currentToken.identifier
			{
				Lexer.mark(
					"Procedure end identifier, \(currentToken.identifier), "
					+ "doesn't match procedure name, \(procInfo.name)")
			}
			currentToken = Lexer.getToken()
		}
		
		codeGenerator.procedureReturn(parameterBlockSize - markSize)
		
		return procInfo
	}
	
	// ---------------------------------------------------
	private static func openProcedureDeclaration(_ markSize: Int)
		-> (proc: SymbolInfo, parameterBlockSize: Int)
	{
		let proc = currentScope.defineSymbol(
			named: currentToken.identifier,
			kind: .procedure
		)
		currentToken = Lexer.getToken()
		codeGenerator.IncLevel(1)
		currentScope = currentScope.openScope()
		proc.value = -1
		
		var parameterBlockSize = markSize

		if currentToken.symbol == .openParen {
			parameterBlockSize = parseParameterList(parameterBlockSize)
		}
		else if codeGenerator.curlev == 1 {
			codeGenerator.enterCmd(proc.name)
		}
		
		setLocalBlockSizeInSymbolTable(parameterBlockSize)
		
		proc.ownedScope = currentScope

		if currentToken.symbol == .semicolon {
			currentToken = Lexer.getToken()
		}
		else { Lexer.mark(";?") }
		
		return (proc, parameterBlockSize)
	}
	
	// ---------------------------------------------------
	private static func closeProcedureDeclaration()
	{
		currentScope = currentScope.closeScope()
		codeGenerator.IncLevel(-1)
	}
	
	// ---------------------------------------------------
	private static func parseProcedureDeclaration()
	{
		// ---------------------------------------------------
		// ProcedureDecl
		currentToken = Lexer.getToken()
		if currentToken.symbol == .identifier
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

		print(" compiling ", terminator: "", to: &standardOutput)
		if currentToken.symbol == .module
		{
			currentToken = Lexer.getToken()
			codeGenerator.open()
			currentScope = currentScope.openScope()
			
			if currentToken.symbol == .identifier
			{
				moduleName = currentToken.identifier
				currentToken = Lexer.getToken()
				print("\(moduleName)", to: &standardOutput)
			}
			else { Lexer.mark("ident?") }
			
			if currentToken.symbol == .semicolon {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark(";?") }
			
			let varsize = parseDeclarations(0)
			while currentToken.symbol == .procedure
			{
				parseProcedureDeclaration()
				if currentToken.symbol == .semicolon {
					currentToken = Lexer.getToken()
				}
				else { Lexer.mark(";?") }
			}
			codeGenerator.header(varsize)
			
			if currentToken.symbol == .begin
			{
				currentToken = Lexer.getToken()
				parseStatementSequence()
			}
			
			if currentToken.symbol == .end {
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark("END?") }
			
			if currentToken.symbol == .identifier
			{
				if moduleName != currentToken.identifier {
					Lexer.mark("no match")
				}
				currentToken = Lexer.getToken()
			}
			else { Lexer.mark("ident?") }
			
			if currentToken.symbol != .period {
				Lexer.mark(". ?")
			}
			
			currentScope = currentScope.closeScope()
			if !Lexer.error
			{
				codeGenerator.close()
				print(
					"code generated\(codeGenerator.pc, pad: 6)",
					to: &standardOutput
				)
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
		let objCode = codeGenerator.getObjectCode()
		var program = [UInt32](capacity: objCode.count + 2)
		program.append(Parser.magic)
		program.append(UInt32(codeGenerator.entry * 4))
		program.append(contentsOf: objCode)
		return program
	}

	// ---------------------------------------------------
	/**
	Compile Oberon-0 code from a `String`
	*/
	static func compile(source: String)
	{
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		Lexer.Init(sourceStream: sourceStream)
		currentToken = Lexer.getToken()
		parseModule()
	}

	// ---------------------------------------------------
	/**
	Disassemble a compiled Oberon-0 program to a `String`
	*/
	static func disassemble() -> String
	{
		var result = ""
		codeGenerator.decode(to: &result)
		return result.isEmpty ? "!!!!! NO OUTPUT !!!!!" : result.description
	}
}


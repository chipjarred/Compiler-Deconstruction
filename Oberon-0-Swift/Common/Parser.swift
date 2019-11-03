//
//  Oberon0Parser.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public final class Parser
{
	typealias CodeGen = RISCCodeGenerator
	
	internal static let WordSize:Int = 4

	internal var currentToken: Token = Token(.null)
	internal var loaded: Bool = false
	
	internal static var globalScope = SymbolScope.makeGlobalScope()
	internal var currentScope = globalScope
	internal var standardOutput =
		FileHandleOutputStream(FileHandle.standardOutput)
	
	private var codeGenerator = RISCCodeGenerator()
	
	internal var lexer = Lexer(sourceStream: InputStream.emptyStream)

	// ---------------------------------------------------
	private func emitErrorOnThrow(for block: () throws -> Void)
	{
		do { return try block() }
		catch {
			lexer.mark(error.localizedDescription)
		}
	}

	// MARK:- Parser
	// ---------------------------------------------------
	private func arrayElementSelector(_ x: CodeGen.RISCOperand) -> CodeGen.RISCOperand
	{
		var x = x
		currentToken = lexer.getToken()
		let y = parseExpression()
		
		if x.type!.form == .array {
			emitErrorOnThrow { try x.index(at: y, for: &codeGenerator) }
		}
		else { lexer.mark("not an array") }
		
		if currentToken.symbol == .closeBracket {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("Expected \"]\"") }
		
		return x
	}
	
	// ---------------------------------------------------
	private func recordFieldSelector(_ x: CodeGen.RISCOperand) -> CodeGen.RISCOperand
	{
		var x = x
		
		currentToken = lexer.getToken()
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
					lexer.mark(
						"Undefined record field, \(currentToken.identifier)"
					)
				}
				
				currentToken = lexer.getToken()
			}
			else { lexer.mark("Not a record") }
		}
		else { lexer.mark("Expected an identifier") }
		
		return x
	}
	
	fileprivate static let selectors: [TokenType] = [.openBracket, .period]
	// ---------------------------------------------------
	private func selector(_ x: CodeGen.RISCOperand) -> CodeGen.RISCOperand
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
	private func factor(_ x: CodeGen.RISCOperand) -> CodeGen.RISCOperand
	{
		var x = x
		// sync
		if currentToken.symbol < .openParen
		{
			lexer.mark("ident?")
			repeat {
				currentToken = lexer.getToken()
			} while !(currentToken.symbol >= .openParen)
		}
		
		switch currentToken.symbol
		{
			case .identifier:
				let identifierInfo =
					currentScope.hierarchy[currentToken.identifier]
				currentToken = lexer.getToken()
				emitErrorOnThrow {
					x = try codeGenerator.makeOperand(identifierInfo!)
				}
				x = selector(x)
			case .number:
				x = codeGenerator.makeConstItem(
					CodeGen.intType,
					currentToken.value
				)
				currentToken = lexer.getToken()
			case .openParen:
				currentToken = lexer.getToken()
				x = parseExpression()
				if currentToken.symbol == .closeParen {
					currentToken = lexer.getToken()
				}
				else { lexer.mark(")?") }
			case .not:
				currentToken = lexer.getToken()
				x = factor(x)
				emitErrorOnThrow { try codeGenerator.Op1(.not, &x) }
			default:
				lexer.mark("factor?")
				x = codeGenerator.makeDefaultOperand()
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseTerminalSymbol(_ x: CodeGen.RISCOperand) -> CodeGen.RISCOperand
	{
		var x = x
		var y = CodeGen.RISCOperand()
		var op: TokenType;
		
		x = factor(x)
		while (currentToken.symbol >= .times) && (currentToken.symbol <= .and)
		{
			op = currentToken.symbol
			currentToken = lexer.getToken()
			if op == TokenType.and {
				emitErrorOnThrow { try codeGenerator.Op1(op, &x) }
			}
			y = factor(y)
			emitErrorOnThrow { try codeGenerator.Op2(op, &x, &y) }
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseSimpleExpression(_ x: CodeGen.RISCOperand)
		-> CodeGen.RISCOperand
	{
		var x = x
		var op: TokenType
		
		if currentToken.symbol == .plus
		{
			currentToken = lexer.getToken()
			x = parseTerminalSymbol(x)
		}
		else if currentToken.symbol == .minus
		{
			currentToken = lexer.getToken()
			x = parseTerminalSymbol(x)
			emitErrorOnThrow { try codeGenerator.Op1(.minus, &x) }
		}
		else {
			x = parseTerminalSymbol(x)
		}
		while (currentToken.symbol >= .plus) && (currentToken.symbol <= .or)
		{
			op = currentToken.symbol
			currentToken = lexer.getToken()
			if op == TokenType.or {
				emitErrorOnThrow { try codeGenerator.Op1(op, &x) }
			}
			var y = parseTerminalSymbol(CodeGen.RISCOperand())
			emitErrorOnThrow { try codeGenerator.Op2(op, &x, &y) }
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseExpression() -> CodeGen.RISCOperand
	{
		var op: TokenType
		
		var x = parseSimpleExpression(CodeGen.RISCOperand())
		if (currentToken.symbol >= .isEqualTo)
			&& (currentToken.symbol <= .greaterThan)
		{
			op = currentToken.symbol
			currentToken = lexer.getToken()
			var y = parseSimpleExpression(CodeGen.RISCOperand())
			emitErrorOnThrow { try codeGenerator.relation(op, &x, &y) }
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseParameter(_ fp: SymbolInfo) -> Bool
	{
		var x = CodeGen.RISCOperand()
		
		x = parseExpression()
		if fp.isParameter
		{
			emitErrorOnThrow { try codeGenerator.parameter(&x, fp) }
			return true
		}

		lexer.mark("Too many parameters")
		return false
	}

	// ---------------------------------------------------
	private func param() -> CodeGen.RISCOperand
	{
		if currentToken.symbol == .openParen {
			currentToken = lexer.getToken()
		}
		else { lexer.mark(")?") }
		
		let x = parseExpression()
		if currentToken.symbol == .closeParen {
			currentToken = lexer.getToken()
		}
		else { lexer.mark(")?") }
		
		return x
	}
	
	// ---------------------------------------------------
	private func advanceLexerToAtLeastIdentifier() -> Token
	{
		var token = Token.null
		repeat {
			token = lexer.getToken()
		} while token.symbol < .identifier
		
		return token
	}
	
	// ---------------------------------------------------
	private func parseAssignment(_ x: CodeGen.RISCOperand)
	{
		currentToken = lexer.getToken()
		var y = CodeGen.RISCOperand()
		y = parseExpression()
		var newX = x
		emitErrorOnThrow { try codeGenerator.store(into: &newX, from: &y) }
	}
	
	// ---------------------------------------------------
	private func parseErroneousEquality()
	{
		currentToken = lexer.getToken()
		let _ = parseExpression()
	}
	
	// ---------------------------------------------------
	/**
	- Returns: `true` if a symbol has been encountered that is not a parameter, indicating that all
		parameters have been parsed, or `false` otherwise.
	*/
	private func parseActualParameters(for procedure: SymbolInfo)
		-> Bool
	{
		assert(procedure.ownedScope != nil)
		let procedureScope = procedure.ownedScope!
		
		currentToken = lexer.getToken()
		if currentToken.symbol == .closeParen
		{
			currentToken = lexer.getToken()
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
						currentToken = lexer.getToken()
						
					case .closeParen:
						currentToken = lexer.getToken();
						return true
						
					default:
						if currentToken.symbol >= .semicolon { break loop }
						lexer.mark("Expected \")\" or \",\"")
				}
				
				if !isAParameter { break loop }
			}
			
			return !isAParameter
		}
	}
	
	// ---------------------------------------------------
	private func parseProcedureCall(
		procedureInfo procInfo: SymbolInfo,
		_ x: CodeGen.RISCOperand)
	{
		var allParametersParsed = true
		if currentToken.symbol == .openParen {
			allParametersParsed = parseActualParameters(for: procInfo)
		}
		
		if procInfo.value < 0 {
			lexer.mark("forward call")
		}
		else if allParametersParsed
		{
			var newX = x
			codeGenerator.call(&newX)
		}
		else { lexer.mark("too few parameters") }
	}
	
	// ---------------------------------------------------
	private func parseStandardProcedureCall(
		_ procInfo: SymbolInfo,
		_ x: CodeGen.RISCOperand)
	{
		var y = CodeGen.RISCOperand()
		if procInfo.value <= 3 {
			y = param()
		}
		
		var newX = x
		emitErrorOnThrow { try codeGenerator.ioCall(&newX, &y) }
	}

	// ---------------------------------------------------
	private func parseThen()
	{
		if currentToken.symbol == .then {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("THEN?") }
		parseStatementSequence()
	}
	
	// ---------------------------------------------------
	private func parseIfStatement()
	{
		currentToken = lexer.getToken()
		var x = CodeGen.RISCOperand()
		x = parseExpression()
		emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
		parseThen()
		var L = 0
		
		while currentToken.symbol == .elsif
		{
			currentToken = lexer.getToken()
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			x = parseExpression()
			emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
			parseThen()
		}
		
		if currentToken.symbol == .else
		{
			currentToken = lexer.getToken()
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			parseStatementSequence()
		}
		else {
			codeGenerator.fixLink(x.a)
		}
		
		codeGenerator.fixLink(L)
		
		if currentToken.symbol == .end {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private func parseWhileStatement()
	{
		currentToken = lexer.getToken()
		let L = Int(codeGenerator.pc)
		var x = CodeGen.RISCOperand()
		x = parseExpression()
		emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
		
		if currentToken.symbol == .do {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("DO?") }
		
		parseStatementSequence()
		codeGenerator.jumpBack(L)
		codeGenerator.fixLink(x.a)
		
		if currentToken.symbol == .end {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("END?") }
	}
	
	// ---------------------------------------------------
	private func parseStatementSequence()
	{
		func makeItem(_ symbolInfo: SymbolInfo) -> CodeGen.RISCOperand
		{
			do { return try codeGenerator.makeOperand(symbolInfo) }
			catch { emitErrorOnThrow { throw error } }
			
			return codeGenerator.makeDefaultOperand()
		}

		// ---------------------------------------------------
		while true // sync
		{
			if currentToken.symbol < .identifier
			{
				lexer.mark("Expected a statement.")
				currentToken = advanceLexerToAtLeastIdentifier()
			}
			
			switch currentToken.symbol
			{
				case .identifier:
					let identiferInfo =
						currentScope.hierarchy[currentToken.identifier]!
					currentToken = lexer.getToken()
					
					var x = makeItem(identiferInfo)
					x = selector(x)
					
					if currentToken.symbol == .becomes {
						parseAssignment(x)
					}
					else if currentToken.symbol == .isEqualTo
					{
						lexer.mark(":= ?")
						parseErroneousEquality()
					}
					else if x.mode == .procedure {
						parseProcedureCall(procedureInfo: identiferInfo, x)
					}
					else if x.mode == .standardProcedure {
						parseStandardProcedureCall(identiferInfo, x)
					}
					else if identiferInfo.kind == .type {
						lexer.mark("Illegal assignment.")
					}
					else { lexer.mark("Expected a statement.") }
				
				case .if: parseIfStatement()
				case .while: parseWhileStatement()
				
				default: break
			}

			if currentToken.symbol == .semicolon {
				currentToken = lexer.getToken()
			}
			else if (currentToken.symbol >= .semicolon)
				&& (currentToken.symbol < .if)
				|| (currentToken.symbol >= .array)
			{
				break
			}
			else { lexer.mark("; ?") }
		}
	}

	// ---------------------------------------------------
	private func parseIdentifierListAsArray(
		token: inout Token,
		_ kind: SymbolInfo.Kind) -> [SymbolInfo]
	{
		var fields = [SymbolInfo]()
		
		if token.symbol == .identifier
		{
			fields.append(SymbolInfo(name: token.identifier, kind: kind))

			token = lexer.getToken()
			while token.symbol == .comma
			{
				token = lexer.getToken()
				if token.symbol == .identifier
				{
					fields.append(
						SymbolInfo(name: token.identifier, kind: kind)
					)
					token = lexer.getToken()
				}
				else { lexer.mark("expected field identifier") }
			}
			if token.symbol == .colon {
				token = lexer.getToken()
			}
			else { lexer.mark("expected \":\"") }
		}
		
		return fields
	}
	
	// ---------------------------------------------------
	private func parseRecordTypeDeclaration() -> TypeInfo
	{
		currentToken = lexer.getToken()
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
				currentToken = lexer.getToken()
			}
			else if currentToken.symbol == .identifier {
				lexer.mark("Expected \":\"")
			}
			else { break }
		}
		
		currentScope = currentScope.closeScope()

		if currentToken.symbol == .end {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("Expected END for record") }
		
		return type
	}
	
	// ---------------------------------------------------
	private func parseArrayDeclaration() -> TypeInfo
	{
		currentToken = lexer.getToken()
		var x = CodeGen.RISCOperand()
		x = parseExpression()
		
		if (x.mode != .constant) || (x.a < 0) {
			lexer.mark("bad index")
		}
		
		if currentToken.symbol == .of {
			currentToken = lexer.getToken()
		}
		else { lexer.mark("Expected OF") }
		
		let tp = parseType()
		let type = TypeInfo()
		type.form = .array
		type.base = tp
		type.len = x.a
		type.size = type.len * tp!.size
		
		return type
	}
	
	// ---------------------------------------------------
	private func parseTypeAlias() -> TypeInfo
	{
		let symbolInfo = currentScope.hierarchy[currentToken.identifier]
		currentToken = lexer.getToken()
		
		if let symInfo = symbolInfo, symInfo.kind == .type {
			return symInfo.type!
		}
		else { lexer.mark("Expected a type") }
		
		return CodeGen.intType
	}

	// ---------------------------------------------------
	private func parseType() -> TypeInfo?
	{
		if (currentToken.symbol != .identifier) && (currentToken.symbol < .array)
		{
			lexer.mark("Expected a type")
			repeat {
				currentToken = lexer.getToken()
			} while currentToken.symbol != .identifier
				&& currentToken.symbol < .array
		}
		
		switch currentToken.symbol
		{
			case .identifier: return parseTypeAlias()
			case .array: return parseArrayDeclaration()
			case .record: return parseRecordTypeDeclaration()
			default: lexer.mark("Expected an identifier")
		}
		
		return CodeGen.intType // sync
	}
	
	// ---------------------------------------------------
	private func define(
		symbol name: String,
		kind: SymbolInfo.Kind,
		in scope: SymbolScope) -> SymbolInfo
	{
		let symbolInfo: SymbolInfo
		do
		{
			symbolInfo = try currentScope.defineSymbol(
				named: name,
				kind: kind
		)
		}
		catch SymbolScope.Error.duplicateSymbolDefinition(let existingInfo)
		{
			lexer.mark(
				"symbol, \(name), is already defined"
			)
			symbolInfo = existingInfo
		}
		catch { fatalError("Unexpected Error: \(error)") }
		
		return symbolInfo
	}
	
	// ---------------------------------------------------
	private func parseConstantDeclarations()
	{
		currentToken = lexer.getToken()
		while currentToken.symbol == .identifier
		{
			let symbolInfo = define(
				symbol: currentToken.identifier,
				kind: .constant,
				in: currentScope
			)

			currentToken = lexer.getToken()
			if currentToken.symbol == .isEqualTo {
				currentToken = lexer.getToken()
			}
			else { lexer.mark("=?") }
			
			var x = CodeGen.RISCOperand()
			x = parseExpression()
			if x.mode == .constant
			{
				symbolInfo.value = x.a
				symbolInfo.type = x.type
			}
			else { lexer.mark("expression not constant") }
			
			if currentToken.symbol == .semicolon {
				currentToken = lexer.getToken()
			}
			else { lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private func parseTypeDeclarations()
	{
		currentToken = lexer.getToken()
		while currentToken.symbol == .identifier
		{
			let symbolInfo = define(
				symbol: currentToken.identifier,
				kind: .type,
				in: currentScope
			)

			currentToken = lexer.getToken()
			if currentToken.symbol == .isEqualTo {
				currentToken = lexer.getToken()
			}
			else { lexer.mark("=?") }
			
			symbolInfo.type = parseType()
			
			if currentToken.symbol == .semicolon {
				currentToken = lexer.getToken()
			}
			else { lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private func parseVariableDeclarations(varsize: Int) -> Int
	{
		var varsize = varsize
		
		currentToken = lexer.getToken()
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
				currentToken = lexer.getToken()
			}
			else { lexer.mark("; ?") }
			
			currentScope.append(variables)
		}
		
		return varsize
	}

	// ---------------------------------------------------
	private func parseDeclarations(_ varsize: Int) -> Int
	{
		var varsize = varsize
		
		// sync
		if currentToken.symbol < .const && currentToken.symbol != .end
		{
			lexer.mark("declaration?")
			repeat {
				currentToken = lexer.getToken()
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
				lexer.mark("Expected declaration (ie. CONST, VAR or TYPE)")
			}
			else { break }
		}
		
		return varsize
	}

	// ---------------------------------------------------
	private func parseParameterList(for token: inout Token)
		-> [SymbolInfo]
	{
		if token.symbol == .var
		{
			token = lexer.getToken()
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
	private func FPSection(_ startingParameterBlockSize: Int) -> Int
	{
		// ---------------------------------------------------
		func getType(for token: inout Token) -> TypeInfo?
		{
			if token.symbol == .identifier
			{
				let identifierInfo = currentScope.hierarchy[token.identifier]
				token = lexer.getToken()
				
				if identifierInfo?.kind == .type {
					return identifierInfo!.type
				}
			}

			lexer.mark("Expected identifier")
			return CodeGen.intType
		}
		
		let parameters = parseParameterList(for: &currentToken)

		let tp = getType(for: &currentToken)
		
		let parsize: Int
		if parameters.first!.kind == .variable
		{
			parsize = tp!.size
			if tp!.form >= .array {
				lexer.mark("Struct parameters are not supported")
			}
		}
		else { parsize = Parser.WordSize }
		
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
	private func parseParameterList(
		_ startingParameterBlockSize: Int) -> Int
	{
		var parameterBlockSize = startingParameterBlockSize
		currentToken = lexer.getToken()
		if currentToken.symbol == .closeParen {
			currentToken = lexer.getToken()
		}
		else
		{
			parameterBlockSize = FPSection(parameterBlockSize)
			while currentToken.symbol == .semicolon
			{
				currentToken = lexer.getToken()
				parameterBlockSize = FPSection(parameterBlockSize)
			}
			
			if currentToken.symbol == .closeParen {
				currentToken = lexer.getToken()
			}
			else { lexer.mark(")?") }
		}
		
		return parameterBlockSize
	}
	
	// ---------------------------------------------------
	private func setLocalBlockSizeInSymbolTable(
		_ parameterBlockSize: Int)
	{
		var localBlockSize = parameterBlockSize
		
		currentScope.modifyEach
		{
			$0.level = codeGenerator.curlev
			if $0.kind == .parameter {
				localBlockSize -= Parser.WordSize
			}
			else {
				localBlockSize -= Int($0.type!.size)
			}
			$0.value = localBlockSize
		}
	}
	
	// ---------------------------------------------------
	private func parseNestedProcedures()
	{
		while currentToken.symbol == .procedure
		{
			parseProcedureDeclaration()
			if currentToken.symbol == .semicolon {
				currentToken = lexer.getToken()
			}
			else { lexer.mark(";?") }
		}
	}
	
	// ---------------------------------------------------
	private func parseProcedureBody(
		procedureInfo procInfo: SymbolInfo,
		localBlockSize: Int,
		parameterBlockSize: Int,
		markSize: Int) -> SymbolInfo
	{
		procInfo.value = Int(codeGenerator.pc)
		codeGenerator.enter(localBlockSize)
		
		if currentToken.symbol == .begin
		{
			currentToken = lexer.getToken()
			parseStatementSequence()
		}
		else
		{
			lexer.mark(
				"Expected BEGIN for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .end {
			currentToken = lexer.getToken()
		}
		else
		{
			lexer.mark(
				"Expected END for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .identifier
		{
			if procInfo.name != currentToken.identifier
			{
				lexer.mark(
					"Procedure end identifier, \(currentToken.identifier), "
					+ "doesn't match procedure name, \(procInfo.name)")
			}
			currentToken = lexer.getToken()
		}
		
		codeGenerator.procedureReturn(parameterBlockSize - markSize)
		
		return procInfo
	}
	
	// ---------------------------------------------------
	private func openProcedureDeclaration(_ markSize: Int)
		-> (proc: SymbolInfo, parameterBlockSize: Int)
	{
		let proc = define(
			symbol: currentToken.identifier,
			kind: .procedure,
			in: currentScope
		)
		currentToken = lexer.getToken()
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
			currentToken = lexer.getToken()
		}
		else { lexer.mark(";?") }
		
		return (proc, parameterBlockSize)
	}
	
	// ---------------------------------------------------
	private func closeProcedureDeclaration()
	{
		currentScope = currentScope.closeScope()
		codeGenerator.IncLevel(-1)
	}
	
	// ---------------------------------------------------
	private func parseProcedureDeclaration()
	{
		// ---------------------------------------------------
		// ProcedureDecl
		currentToken = lexer.getToken()
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
			lexer.mark("Expected procedure name.")
		}
	}

	// ---------------------------------------------------
	private func parseModule()
	{
		var moduleName = ""

		print(" compiling ", terminator: "", to: &standardOutput)
		if currentToken.symbol == .module
		{
			currentToken = lexer.getToken()
			codeGenerator.open()
			currentScope = currentScope.openScope()
			
			if currentToken.symbol == .identifier
			{
				moduleName = currentToken.identifier
				currentToken = lexer.getToken()
				print("\(moduleName)", to: &standardOutput)
			}
			else { lexer.mark("ident?") }
			
			if currentToken.symbol == .semicolon {
				currentToken = lexer.getToken()
			}
			else { lexer.mark(";?") }
			
			let varsize = parseDeclarations(0)
			while currentToken.symbol == .procedure
			{
				parseProcedureDeclaration()
				if currentToken.symbol == .semicolon {
					currentToken = lexer.getToken()
				}
				else { lexer.mark(";?") }
			}
			codeGenerator.header(varsize)
			
			if currentToken.symbol == .begin
			{
				currentToken = lexer.getToken()
				parseStatementSequence()
			}
			
			if currentToken.symbol == .end {
				currentToken = lexer.getToken()
			}
			else { lexer.mark("END?") }
			
			if currentToken.symbol == .identifier
			{
				if moduleName != currentToken.identifier {
					lexer.mark("no match")
				}
				currentToken = lexer.getToken()
			}
			else { lexer.mark("ident?") }
			
			if currentToken.symbol != .period {
				lexer.mark(". ?")
			}
			
			currentScope = currentScope.closeScope()
			if !lexer.error
			{
				codeGenerator.close()
				print(
					"code generated\(codeGenerator.pc, pad: 6)",
					to: &standardOutput
				)
			}
		}
		else { lexer.mark("MODULE?") }
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
	internal var program: [UInt32]
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
	public func compile(source: String)
	{
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		lexer = Lexer(sourceStream: sourceStream)
		currentToken = lexer.getToken()
		parseModule()
	}

	// ---------------------------------------------------
	/**
	Disassemble a compiled Oberon-0 program to a `String`
	*/
	public func disassemble() -> String
	{
		var result = ""
		codeGenerator.decode(to: &result)
		return result.isEmpty ? "!!!!! NO OUTPUT !!!!!" : result.description
	}
}


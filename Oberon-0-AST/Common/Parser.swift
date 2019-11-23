// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

// ---------------------------------------------------
public final class Parser
{
	typealias CodeGen = RISCCodeGenerator
	
	internal static let WordSize:Int = 4

	internal var currentToken: Token =
		Token.null(location: SourceLocation.none)
	
	internal var loaded: Bool = false
	
	internal static var globalScope = SymbolScope.makeGlobalScope()
	internal var currentScope = globalScope
	internal var standardOutput: OutputStream =
		FileHandle.standardOutput.textOutputStream!
	
	private let errorReporter: ErrorReporter
	
	private var codeGenerator = RISCCodeGenerator()
	
	internal var lexer: Lexer! = nil

	// ---------------------------------------------------
	private func emitErrorOnThrow(for block: () throws -> Void)
	{
		do { return try block() }
		catch {
			emitError(error.localizedDescription)
		}
	}

	// MARK:- Parser
	// ---------------------------------------------------
	private func arrayElementSelector(_ x: RISCOperand) -> RISCOperand
	{
		var x = x
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		let y = parseExpression()
		
		if x.type!.form == .array {
			emitErrorOnThrow { try x.index(at: y, for: &codeGenerator) }
		}
		else { emitError("Not an array") }
		
		if currentToken.symbol == .closeBracket {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected \"]\"") }
		
		return x
	}
	
	// ---------------------------------------------------
	private func recordFieldSelector(_ x: RISCOperand) -> RISCOperand
	{
		var x = x
		
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
					emitError(
						"Undefined record field, \(currentToken.identifier)"
					)
				}
				
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Not a record") }
		}
		else { emitError("Expected an identifier") }
		
		return x
	}
	
	fileprivate static let selectors: [TokenType] = [.openBracket, .period]
	// ---------------------------------------------------
	private func selector(_ x: RISCOperand) -> RISCOperand
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
	private func factor(_ x: RISCOperand) -> RISCOperand
	{
		var x = x
		// sync
		if currentToken.symbol < .openParen
		{
			emitError("Expected identifier in expression")
			repeat {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			} while !(currentToken.symbol >= .openParen)
		}
		
		switch currentToken.symbol
		{
			case .identifier:
				let identifierInfo =
					currentScope.hierarchy[currentToken.identifier]
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				emitErrorOnThrow {
					x = try codeGenerator.makeOperand(identifierInfo!)
				}
				x = selector(x)
			case .number:
				x = codeGenerator.makeConstItem(
					CodeGen.intType,
					currentToken.value
				)
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			case .openParen:
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				x = parseExpression()
				if currentToken.symbol == .closeParen {
					currentToken = (lexer.nextToken() ?? lexer.eofToken)
				}
				else { emitError("Expected \")\" in expression") }
			case .not:
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				x = factor(x)
				emitErrorOnThrow { try codeGenerator.emitUnaryExpression(.not, &x) }
			default:
				// TODO: Need a better error message
				emitError("factor?")
				x = codeGenerator.makeDefaultOperand()
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseTerminalSymbol(_ x: RISCOperand) -> RISCOperand
	{
		var x = factor(x)
		var op: TokenType;
		
		while (currentToken.symbol >= .times) && (currentToken.symbol <= .and)
		{
			op = currentToken.symbol
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			if op == TokenType.and
			{
				emitErrorOnThrow
				{
					try codeGenerator.emitLogicShortCircuit(
						for: op,
						operand: &x
					)
				}
			}
			
			var y = RISCOperand()
			y = factor(y)
			emitErrorOnThrow {
				try codeGenerator.emitBinaryExpression(op, &x, &y)
			}
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseSimpleExpression(_ x: RISCOperand) -> RISCOperand
	{
		var x = x
		var op: TokenType
		
		if currentToken.symbol == .plus
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			x = parseTerminalSymbol(x)
		}
		else if currentToken.symbol == .minus
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			x = parseTerminalSymbol(x)
			emitErrorOnThrow {
				try codeGenerator.emitUnaryExpression(.minus, &x)
			}
		}
		else {
			x = parseTerminalSymbol(x)
		}
		
		while (currentToken.symbol >= .plus) && (currentToken.symbol <= .or)
		{
			op = currentToken.symbol
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			if op == TokenType.or
			{
				emitErrorOnThrow
				{
					try codeGenerator.emitLogicShortCircuit(
						for: op,
						operand: &x
					)
				}
			}
			
			var y = parseTerminalSymbol(RISCOperand())
			emitErrorOnThrow {
				try codeGenerator.emitBinaryExpression(op, &x, &y)
			}
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseExpression() -> RISCOperand
	{
		var op: TokenType
		
		var x = parseSimpleExpression(RISCOperand())
		if (currentToken.symbol >= .isEqualTo)
			&& (currentToken.symbol <= .greaterThan)
		{
			op = currentToken.symbol
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			var y = parseSimpleExpression(RISCOperand())
			emitErrorOnThrow { try codeGenerator.emitComparison(op, &x, &y) }
		}
		
		return x
	}

	// ---------------------------------------------------
	private func parseParameter(_ fp: SymbolInfo) -> Bool
	{
		var x = parseExpression()
		
		if fp.isParameter
		{
			emitErrorOnThrow { try codeGenerator.parameter(&x, fp) }
			return true
		}

		emitError("Too many parameters")
		return false
	}

	// ---------------------------------------------------
	private func param() -> RISCOperand
	{
		if currentToken.symbol == .openParen {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected \"(\" to begin parameter list") }
		
		let x = parseExpression()
		if currentToken.symbol == .closeParen {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected \")\" to terminate parameter list") }
		
		return x
	}
	
	private static let nullToken = Token.null(location: SourceLocation.none)
	// ---------------------------------------------------
	private func advanceLexerToAtLeastIdentifier() -> Token
	{
		var token = Parser.nullToken
		repeat {
			token = (lexer.nextToken() ?? lexer.eofToken)
		} while token.symbol < .identifier
		
		return token
	}
	
	// ---------------------------------------------------
	private func parseAssignment(_ x: RISCOperand)
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		var y = parseExpression()
		var newX = x
		emitErrorOnThrow { try codeGenerator.emitAssignment(into: &newX, from: &y) }
	}
	
	// ---------------------------------------------------
	private func parseErroneousEquality()
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
		
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		if currentToken.symbol == .closeParen
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
						currentToken = (lexer.nextToken() ?? lexer.eofToken)
						
					case .closeParen:
						currentToken = (lexer.nextToken() ?? lexer.eofToken);
						return true
						
					default:
						if currentToken.symbol >= .semicolon { break loop }
						emitError("Expected \")\" or \",\" in parameter list")
				}
				
				if !isAParameter { break loop }
			}
			
			return !isAParameter
		}
	}
	
	// ---------------------------------------------------
	private func parseProcedureCall(
		procedureInfo procInfo: SymbolInfo,
		_ x: RISCOperand)
	{
		var allParametersParsed = true
		if currentToken.symbol == .openParen {
			allParametersParsed = parseActualParameters(for: procInfo)
		}
		
		if procInfo.value < 0
		{
			// TODO: Need a better error message here
			emitError("forward call")
		}
		else if allParametersParsed
		{
			var newX = x
			codeGenerator.call(&newX)
		}
		else { emitError("Too few parameters") }
	}
	
	// ---------------------------------------------------
	private func parseStandardProcedureCall(
		_ procInfo: SymbolInfo,
		_ x: RISCOperand)
	{
		var y = procInfo.value <= 3
			? param()
			: RISCOperand()
		var newX = x
		emitErrorOnThrow { try codeGenerator.ioCall(&newX, &y) }
	}

	// ---------------------------------------------------
	private func parseThen()
	{
		if currentToken.symbol == .then {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected THEN") }
		parseStatementSequence()
	}
	
	// ---------------------------------------------------
	private func parseIfStatement()
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		var x = parseExpression()
		emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
		parseThen()
		var L = 0
		
		while currentToken.symbol == .elsif
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			x = parseExpression()
			emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
			parseThen()
		}
		
		if currentToken.symbol == .else
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			codeGenerator.jumpForward(&L)
			codeGenerator.fixLink(x.a)
			parseStatementSequence()
		}
		else {
			codeGenerator.fixLink(x.a)
		}
		
		codeGenerator.fixLink(L)
		
		if currentToken.symbol == .end {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected END to terminate IF statement") }
	}
	
	// ---------------------------------------------------
	private func parseWhileStatement()
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		let L = Int(codeGenerator.pc)
		var x = parseExpression()
		emitErrorOnThrow { try codeGenerator.conditionalJump(&x) }
		
		if currentToken.symbol == .do {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected DO") }
		
		parseStatementSequence()
		codeGenerator.jumpBack(L)
		codeGenerator.fixLink(x.a)
		
		if currentToken.symbol == .end {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected END") }
	}
	
	// ---------------------------------------------------
	private func parseStatementSequence()
	{
		func makeItem(_ symbolInfo: SymbolInfo) -> RISCOperand
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
				emitError("Expected a statement.")
				currentToken = advanceLexerToAtLeastIdentifier()
			}
			
			switch currentToken.symbol
			{
				case .identifier:
					let identiferInfo =
						currentScope.hierarchy[currentToken.identifier]!
					currentToken = (lexer.nextToken() ?? lexer.eofToken)
					
					var x = makeItem(identiferInfo)
					x = selector(x)
					
					if currentToken.symbol == .becomes {
						parseAssignment(x)
					}
					else if currentToken.symbol == .isEqualTo
					{
						emitError("Expected assignment operator, \":=\"")
						parseErroneousEquality()
					}
					else if x.mode == .procedure {
						parseProcedureCall(procedureInfo: identiferInfo, x)
					}
					else if x.mode == .standardProcedure {
						parseStandardProcedureCall(identiferInfo, x)
					}
					else if identiferInfo.kind == .type {
						emitError("Illegal assignment.")
					}
					else { emitError("Expected a statement.") }
				
				case .if: parseIfStatement()
				case .while: parseWhileStatement()
				
				default: break
			}

			if currentToken.symbol == .semicolon {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else if (currentToken.symbol >= .semicolon)
				&& (currentToken.symbol < .if)
				|| (currentToken.symbol >= .array)
			{
				break
			}
			else { emitError("Expected \";\" to terminate statement") }
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

			token = (lexer.nextToken() ?? lexer.eofToken)
			while token.symbol == .comma
			{
				token = (lexer.nextToken() ?? lexer.eofToken)
				if token.symbol == .identifier
				{
					fields.append(
						SymbolInfo(name: token.identifier, kind: kind)
					)
					token = (lexer.nextToken() ?? lexer.eofToken)
				}
				else { emitError("Expected field identifier") }
			}
			if token.symbol == .colon {
				token = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \":\" in identifier list") }
		}
		
		return fields
	}
	
	// ---------------------------------------------------
	private func parseRecordTypeDeclaration() -> TypeInfo
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else if currentToken.symbol == .identifier {
				emitError("Expected \";\" to terminate type declaration")
			}
			else { break }
		}
		
		currentScope = currentScope.closeScope()

		if currentToken.symbol == .end {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected END for record declaration") }
		
		return type
	}
	
	// ---------------------------------------------------
	private func parseArrayDeclaration() -> TypeInfo
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		let x = parseExpression()
		
		if x.mode != .constant {
			emitError("Array delcaration requires a constant length")
		}
		else if x.a < 0
		{
			emitError(
				"Attempt to declare a negative length array: length = \(x.a)"
			)
		}
		
		if currentToken.symbol == .of {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else { emitError("Expected OF") }
		
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
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		
		if let symInfo = symbolInfo, symInfo.kind == .type {
			return symInfo.type!
		}
		else { emitError("Expected a type") }
		
		return CodeGen.intType
	}

	// ---------------------------------------------------
	private func parseType() -> TypeInfo?
	{
		if (currentToken.symbol != .identifier) && (currentToken.symbol < .array)
		{
			emitError("Expected a type")
			repeat {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			} while currentToken.symbol != .identifier
				&& currentToken.symbol < .array
		}
		
		switch currentToken.symbol
		{
			case .identifier: return parseTypeAlias()
			case .array: return parseArrayDeclaration()
			case .record: return parseRecordTypeDeclaration()
			default: emitError("Expected an identifier")
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
		do {
			symbolInfo = try currentScope.defineSymbol(named: name, kind: kind)
		}
		catch SymbolScope.Error.duplicateSymbolDefinition(let existingInfo)
		{
			emitError("symbol, \(name), is already defined")
			symbolInfo = existingInfo
		}
		catch
		{
			/*
			Something is really wrong if SymbolScope.defineSymbol(name:kind:)
			throws an error other than .duplicateSymbolDefinition, so just
			terminate with an error.
			*/
			fatalError(
				"SymbolTable.defineSymbol(named:kind:) threw an unexpected "
				+ "error: \(error)"
			)
		}
		
		return symbolInfo
	}
	
	// ---------------------------------------------------
	private func parseConstantDeclarations()
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		while currentToken.symbol == .identifier
		{
			let symbolInfo = define(
				symbol: currentToken.identifier,
				kind: .constant,
				in: currentScope
			)

			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			if currentToken.symbol == .isEqualTo {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \"=\" to assign value to constant") }
			
			let x = parseExpression()
			if x.mode == .constant
			{
				symbolInfo.value = x.a
				symbolInfo.type = x.type
			}
			else { emitError("Expression not constant") }
			
			if currentToken.symbol == .semicolon {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \";\" to terminate declaration") }
		}
	}
	
	// ---------------------------------------------------
	private func parseTypeDeclarations()
	{
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		while currentToken.symbol == .identifier
		{
			let symbolInfo = define(
				symbol: currentToken.identifier,
				kind: .type,
				in: currentScope
			)

			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			if currentToken.symbol == .isEqualTo {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \"=\" to assign type declaration") }
			
			symbolInfo.type = parseType()
			
			if currentToken.symbol == .semicolon {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \";\" to terminate type declaration") }
		}
	}
	
	// ---------------------------------------------------
	private func parseVariableDeclarations(varsize: Int) -> Int
	{
		var varsize = varsize
		
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else {
				emitError("Expected \";\" to terminate variable declaration")
			}
			
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
			emitError(
				"Expected a declaration (ie. CONST, VAR, TYPE or PROCEDURE)"
			)
			repeat {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
				emitError(
					"Expected a declaration (ie. CONST, VAR, TYPE or PROCEDURE)"
				)
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
			token = (lexer.nextToken() ?? lexer.eofToken)
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
				token = (lexer.nextToken() ?? lexer.eofToken)
				
				if identifierInfo?.kind == .type {
					return identifierInfo!.type
				}
			}

			emitError("Expected identifier")
			return CodeGen.intType
		}
		
		let parameters = parseParameterList(for: &currentToken)

		let tp = getType(for: &currentToken)
		
		let parsize: Int
		if parameters.first!.kind == .variable
		{
			parsize = tp!.size
			switch tp!.form
			{
				case .void, .integer, .boolean: break
				case .array, .record,  .procedure:
					emitError("\(tp!.form) parameters are not supported")
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
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
		if currentToken.symbol == .closeParen {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else
		{
			parameterBlockSize = FPSection(parameterBlockSize)
			while currentToken.symbol == .semicolon
			{
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				parameterBlockSize = FPSection(parameterBlockSize)
			}
			
			if currentToken.symbol == .closeParen {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else { emitError("Expected \")\" to terminate parameter list") }
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
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else {
				emitError(
					"Expected \";\" to terminate nested procedure declaration"
				)
			}
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
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			parseStatementSequence()
		}
		else
		{
			emitError(
				"Expected BEGIN for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .end {
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else
		{
			emitError(
				"Expected END for procedure, \(procInfo.name)"
			)
		}
		
		if currentToken.symbol == .identifier
		{
			if procInfo.name != currentToken.identifier
			{
				emitError(
					"Procedure end identifier, \(currentToken.identifier), "
					+ "doesn't match procedure name, \(procInfo.name)")
			}
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
		}
		else
		{
			emitError(
				"Expected \";\" to terminate delcaration of procedure, "
				+ "\(proc.name)"
			)
		}
		
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
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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
			emitError("Expected procedure name.")
		}
	}

	// ---------------------------------------------------
	private func parseModule()
	{
		var moduleName = ""

		print(" compiling ", terminator: "", to: &standardOutput)
		if currentToken.symbol == .module
		{
			currentToken = (lexer.nextToken() ?? lexer.eofToken)
			codeGenerator.open()
			currentScope = currentScope.openScope()
			
			if currentToken.symbol == .identifier
			{
				moduleName = currentToken.identifier
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				print("\(moduleName)", to: &standardOutput)
			}
			else { emitError("Expected an identifier for module name.") }
			
			if currentToken.symbol == .semicolon {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else {
				// TODO: Improve error message: Expected ; to termiante what?
				emitError("Expected a \";\"")
			}
			
			let varsize = parseDeclarations(0)
			while currentToken.symbol == .procedure
			{
				parseProcedureDeclaration()
				if currentToken.symbol == .semicolon {
					currentToken = (lexer.nextToken() ?? lexer.eofToken)
				}
				else
				{
					emitError(
						"Expected a \";\" to terminate procedure declaration"
					)
				}
			}
			codeGenerator.header(varsize)
			
			if currentToken.symbol == .begin
			{
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
				parseStatementSequence()
			}
			
			if currentToken.symbol == .end {
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else {
				emitError("Expected END to terminate MODULE \(moduleName)")
			}
			
			/*
			The next few lines are an example of language design silliness:
			We got an END, and it's terminating the MODULE.  We know that, so
			why exactly does Oberon-0 require the programmer to follow that
			with the module's name and a period?  That's just needless
			boilerplate.  If the module is prematurely terminated, then there
			will be code following the END, and errors should be generated for
			that code, noting that the module was already ended. The END
			closing the module's BEGIN should be sufficient to terminate the
			module by itself.  The rest just makes annoying busy work to
			satisfy some obviously unnecessary and artificially imposed
			symmetry.
			
			For that matter, why is the module terminated with a period when
			every other statement is terminated with a semicolon?  Just to
			emphasize that the module is special?  That needlessly overloads
			the meaning of a period, which is normally for record field access,
			and increases, if only just little, the complexity of the grammar
			with zero benefit.
			
			I can see allowing, but not requiring the module's name after END,
			and if it is provided, it should be checked that it matches the
			module name, because programmer may decide it increases
			readability, but this seems like a silly thing to require the
			programmer to do.
			
			Same goes for procedure definitions.
			
			The basic philosophy should be to help the programmer do what he
			wants, and only require him to do it in a particular way when it
			is either logically required to make the language work, or when
			there is some real benefit to be gained in code safety or
			performance.
			
			Anyway, this is Wirth's language we're compiling, not mine, so
			we'll follow his rules.
			*/
			if currentToken.symbol == .identifier
			{
				if moduleName != currentToken.identifier
				{
					emitError(
						"END \(currentToken.identifier) does not match module "
						+ "name, \(moduleName)"
					)
				}
				currentToken = (lexer.nextToken() ?? lexer.eofToken)
			}
			else {
				emitError(
					"Expected identifier, \(moduleName), to terminate module "
					+ "definition"
				)
			}
			
			if currentToken.symbol != .period {
				emitError("Expected a \".\" to terminate module \(moduleName)")
			}
			
			currentScope = currentScope.closeScope()
			if errorReporter.errorCount == 0 {
				codeGenerator.close()
			}
		}
		else { emitError("Expected module to begin with MODULE") }
	}
	
	// ---------------------------------------------------
	private func emitError(_ message: String) {
		errorReporter.mark(message)
	}

	// MARK:- Public Interface
	// ---------------------------------------------------
	public init(errorsTo reporter: ErrorReporter? = nil)
	{
		self.errorReporter = reporter
			?? ErrorReporter(FileHandle.standardError)!
	}
	
	// ---------------------------------------------------
	/// Program signature/marker
	static var magic: UInt32 {
		return UInt32(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	/**
	Returns the compiled program
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
	public func compile(source: String, sourceName: String)
	{
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		lexer = Lexer(
			sourceStream: sourceStream,
			sourceName: sourceName,
			errorsTo: errorReporter
		)
		currentToken = (lexer.nextToken() ?? lexer.eofToken)
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


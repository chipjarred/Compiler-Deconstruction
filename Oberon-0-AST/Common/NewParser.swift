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

// ----------------------------------
/**
Oberon-0 Parser that generates an Abstract Syntax Tree
*/
final class NewParser: CompilerPhase
{
	private typealias OperatorStack = Stack<Token>
	private typealias OperandStack = Stack<ASTNode>
		
	var lexer: Lexer
	
	// ----------------------------------
	init(
		source: String,
		sourceName: String = "<<NONE>>",
		errorsTo reporter: ErrorReporter? = nil)
	{
		let reporter = reporter ?? ErrorReporter(FileHandle.standardError)!
		
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		self.lexer = Lexer(
			sourceStream: sourceStream,
			sourceName: sourceName,
			errorsTo: reporter
		)
		
		super.init(errorsTo: reporter)
	}
	
	// ----------------------------------
	/**
	Parses an Oberon-0 program, which is just a source file containing at least one module declaration.
	
	- Parameter allowErrors: `Bool` for specifying whether an `ASTNode` should be returned if
		the parser encountered errors, but was able to generate a tree.
	
	- Returns: `ASTNode` containing the program, or `nil` if there were errors during parsing and
		`allowErrors` is `true`, or if no modules were successfully parsed.
	
	The `ASTNode` returned from this method is  the root of the abstract syntax tree representing a program.
	
	If `allowErrors` is `false`, the default, the returned `ASTNode` represents a program whose
	structure is syntaticly valid, but that does *not* mean the program is correct.  It is not type-checked, nor
	has there been any attempt to determine whether identifiers are even properly defined before they are
	used.
	
	If `allowErrors` is `true`, the returned `ASTNode` may not even represent a syntacticly valid
	program.  This can be useful in testing and debugging, as one can inspect the tree to compare it with
	expectations.  For actual compilation, setting `allowErrors` to `true` might result in a flood of
	subsequent errors that are directly the result of program source code that is not syntacticly correct.  For
	example, if the parser is unable to generate an `ASTNode` for a procedure, that procedure will be
	missing from the tree, but we may still be able to parse the code that calls that procedure.   In later phases,
	that code would then emit errors for the missing procedure, but those are not actually themselves real
	errors. The real error was that the syntax for the procedure declaration was wrong.  As a result it doesn't
	make sense to pass the AST resulting from parsing syntacticly incorrect code to later phases.  This is why
	the default is to set `allowErrors` to `false`
	
	- NOTE:Since normally complation would set `allowErrors` to `false`, an AST with syntax
	errors will not passed to later phases.  This means that syntax errors will have to be fixed in the source
	code before other kinds of errors can be found.
	*/
	public final func parse(allowErrors: Bool = false) -> AbstractSyntaxTree?
	{
		var modules = [ASTNode]()
		
		while let token = lexer.peekToken()
		{
			if token.symbol != .module
			{
				emitError(expectedOneOf: [.module], got: token)
				lexer.advance()
				continue
			}
			
			if let module = parseScopeDeclaration(asModule: true) {
				modules.append(module)
			}
		}
		
		if modules.isEmpty {
			emitError("Souce file must have at least one module!")
			return nil
		}
		
		if !allowErrors && errorCount > 0 { return nil }
		
		return AbstractSyntaxTree(root: ASTNode(programModules: modules))
	}
	
	// MARK:- Section parsing
	// ----------------------------------
	/**
	A "section" consists of the portion of code marked by CONST, TYPE, VAR, OR BEGIN, with each section
	being terminated by the start of the next section, or by END in the case of BEGIN.  Each section can only
	contain a specific kind of statement.  Sections are parts of PROCEDURE and MODULE definitions.
	
	- CONST may only contain constant declarations
	- TYPE may only contain type declarations
	- VAR may only contain variable declarations
	- BEGIN...END may only contain statements.
	*/

	// ----------------------------------
	/**
	Parse a section of the specified `sectionType`
	
	- Parameter sectionType: `TokenType` specifying the type of section to be parsed.
	- Returns: An `ASTNode` representing the section, or `nil` if the section could not be parsed.
	*/
	internal final func parseSection(_ sectionType: TokenType) -> ASTNode?
	{
		assert(TokenType.sectionTypes.contains(sectionType))
		
		guard let token = currentToken(
			ifAnyOf: TokenType.sectionTypes,
			consuming: true)
		else { return nil }
		
		assert(token.symbol == sectionType)
		
		switch token.symbol
		{
			case .const:
				return parseCONSTSection(startingWith: token)
			
			case .type:
				return parseTYPESection(startingWith: token)
			
			case .var:
				return parseVARSection(startingWith: token)
			
			case .begin:
				return parseCodeBlock(startingWith: token, terminatedBy: [.end])
			
			default:
				emitError(expectedOneOf: TokenType.sectionTypes, got: token)
		}
		
		return nil
	}
	
	// ----------------------------------
	private func parseVARSection(startingWith section: Token) -> ASTNode?
	{
		assert(section.symbol == .var)
		
		var declarations = parseVariableDeclarationList(
			terminatedBy: TokenType.sectionTypes + [.procedure],
			inRecordDeclaration: false
		)
		
		while let terminator = lexer.peekToken(), terminator.symbol == .var
		{
			emitError("Duplicate VAR section", for: terminator)
			lexer.advance()
			let moreDeclarations = parseVariableDeclarationList(
				terminatedBy: TokenType.sectionTypes,
				inRecordDeclaration: false)
			
			declarations.append(contentsOf: moreDeclarations)
		}
		
		if declarations.isEmpty { return nil }
		
		return ASTNode(section: section, contents: declarations)
	}
	
	// ----------------------------------
	private func parseCONSTSection(startingWith section: Token) -> ASTNode?
	{
		assert(section.symbol == .const)
		
		var declarations = parseConstantDeclarationList(
			terminatedBy: TokenType.sectionTypes
		)
		
		while let terminator = lexer.peekToken(), terminator.symbol == .const
		{
			emitError("Duplicate CONST section", for: terminator)
			lexer.advance()
			let moreDeclarations = parseConstantDeclarationList(
				terminatedBy: TokenType.sectionTypes + [.procedure]
			)
			
			declarations.append(contentsOf: moreDeclarations)
		}
		
		if declarations.isEmpty { return nil }
		
		return ASTNode(section: section, contents: declarations)
	}
	
	// ----------------------------------
	private func parseTYPESection(startingWith section: Token) -> ASTNode?
	{
		assert(section.symbol == .type)
		
		var declarations = parseTypeDeclarationList(
			terminatedBy: TokenType.sectionTypes + [.procedure]
		)
		
		while let terminator = lexer.peekToken(), terminator.symbol == .const
		{
			emitError("Duplicate TYPE section", for: terminator)
			lexer.advance()
			let moreDeclarations = parseTypeDeclarationList(
				terminatedBy: TokenType.sectionTypes
			)
			
			declarations.append(contentsOf: moreDeclarations)
		}
		
		if declarations.isEmpty { return nil }
		
		return ASTNode(section: section, contents: declarations)
	}

	// MARK:- Declaration parsing
	// ----------------------------------
	internal final func parseScopeDeclaration(asModule: Bool) -> ASTNode?
	{
		guard let scopeToken = currentToken(
			is: asModule ? .module : .procedure,
			consuming: true)
		else { return nil }
		
		let scopeName = lexer.nextToken()
		if scopeName?.symbol != .identifier
		{
			emitError(expected: "identifier", got: scopeName)
			lexer.advance(to: .end, consuming: true)
		}
		
		let paramMarker = currentToken(
			ifAnyOf: asModule ? [.semicolon] : [.semicolon, .openParen],
			consuming: true
		)
		
		let parameters:[ASTNode]
		if !asModule && paramMarker?.symbol == .openParen
		{
			parameters = parseFormalParameters()
			expect(.semicolon, consuming: true)
		}
		else { parameters = [] }
		
		var constSection: ASTNode? = nil
		var typeSection: ASTNode? = nil
		var varSection: ASTNode? = nil
		var procedures: [ASTNode] = []
		var body: ASTNode? = nil
		
		parseSections(
			forScope: scopeToken,
			named: scopeName!,
			constSection: &constSection,
			typeSection: &typeSection,
			varSection: &varSection,
			procedures: &procedures,
			body: &body
		)
		
		
		if asModule
		{
			return ASTNode(
				module: scopeToken,
				named: scopeName!,
				constSection: constSection!,
				typeSection: typeSection!,
				varSection: varSection!,
				procedures: procedures,
				body: body!
			)
		}
		
		return ASTNode(
			procedure: scopeToken,
			named: scopeName!,
			parameters: parameters,
			constSection: constSection!,
			typeSection: typeSection!,
			varSection: varSection!,
			procedures: procedures,
			body: body!
		)
	}
	
	// ----------------------------------
	private func emptySection(
		type: TokenType,
		at location: SourceLocation) -> ASTNode
	{
		return ASTNode(
			section: Token(type, location: location),
			contents: []
		)
	}
	
	// ----------------------------------
	private func emptyBody(at location: SourceLocation) -> ASTNode
	{
		return ASTNode(
			block: Token(.begin, location: location),
			statements: []
		)
	}
	
	// ----------------------------------
	private func parseSections(
		forScope scope: Token,
		named scopeName: Token,
		constSection: inout ASTNode?,
		typeSection: inout ASTNode?,
		varSection: inout ASTNode?,
		procedures: inout [ASTNode],
		body: inout ASTNode?)
	{
		assert(scope.symbol == .module || scope.symbol == .procedure)
		
		let asModule = scope.symbol == .module
		var scopeStr = asModule ? "module" : "procedure"

		// ----------------------------------
		func assign(
			section: inout ASTNode?,
			from token: Token,
			using parseSection: (_: Token) -> ASTNode?)
		{
			if let astNode = parseSection(token)
			{
				if section == nil { section = astNode }
				else
				{
					let sectionName = token.symbol == .begin
						? "\(token.srcString) section"
						: "body"
					emitError(
						"Ignoring duplicate \(sectionName) for "
						+ "\(scopeStr), \"\(scopeName.srcString)\".",
						for: token
					)
				}
			}
		}

		// ----------------------------------
		while let token = lexer.peekToken(),
			token.symbol != .identifier,
			token.identifier != scopeName.identifier
		{
			if token.symbol != .procedure { lexer.advance() }
			switch token.symbol
			{
				case .const:
					assign(section: &constSection, from: token) {
						parseCONSTSection(startingWith: $0)
					}
				
				case .type:
					assign(section: &typeSection, from: token) {
						parseTYPESection(startingWith: $0)
					}
				
				case .var:
					assign(section: &varSection, from: token) {
						parseVARSection(startingWith: $0)
					}

				case .procedure:
					if let proc = parseScopeDeclaration(asModule: false) {
						procedures.append(proc)
					}

				case .begin:
					assign(section: &body, from: token) {
						parseCodeBlock(startingWith: $0, terminatedBy: [.end])
					}
				
				default:
					emitError(
						expectedOneOf: TokenType.sectionsAndProcedure,
						got: token
					)
					lexer.advance(
						toOneOf: TokenType.sectionsAndProcedure,
						consuming: false
					)
			}
		}
		
		lexer.advance()
		expect(asModule ? .period : .semicolon, consuming: true)
		
		let endOfName = scopeName.sourceRange.upperBound
		
		if body == nil
		{
			emitError(
				"BEGIN...END block missing for \(scopeStr), "
				+ "\(scopeName.identifier)")
		}
		
		constSection = constSection ?? emptySection(type: .const, at: endOfName)
		typeSection = typeSection ?? emptySection(type: .type, at: endOfName)
		varSection = varSection ?? emptySection(type: .var, at: endOfName)
		body = body ?? emptyBody(at: endOfName)
	}
			
	
	private let paramTokenTypes: [TokenType] = [.identifier, .var]
	// ----------------------------------
	private func parseFormalParameters() -> [ASTNode]
	{
		assert(paramTokenTypes.contains(lexer.peekToken()?.symbol))
		
		var parameters: [ASTNode] = []
		while true
		{
			let paramToken = lexer.peekToken()
			if paramToken?.symbol == .closeParen
			{
				lexer.advance()
				break
			}
			
			guard paramTokenTypes.contains(paramToken?.symbol) else
			{
				emitError(expected: "identifier or \"VAR\"", got: paramToken)
				lexer.advance(to: .closeParen, consuming: true)
				break
			}
			
			parameters.append(
				contentsOf: parseAFormalParameter(
					startingWith: paramToken!
				)
			)
		}
		
		return parameters
	}
	
	// ----------------------------------
	private final func parseAFormalParameter(
		startingWith firstParamName: Token) -> [ASTNode]
	{
		assert(
			firstParamName.symbol == .var
			|| firstParamName.symbol == .identifier
		)
		
		let extractionType: ASTNode.Kind = firstParamName.symbol == .identifier
			? .valueParam
			: .referenceParam
		
		if extractionType == .referenceParam {
			lexer.advance() // consume VAR
		}
		
		return extractFormalParameters(
			from: parseVariableDeclaration(
				terminatedBy: [.semicolon, .closeParen]
			),
			as: extractionType
		)
	}
	
	// ----------------------------------
	private final func extractFormalParameters(
		from paramNode: ASTNode?,
		as newKind: ASTNode.Kind) -> [ASTNode]
	{
		assert(newKind == .valueParam || newKind == .referenceParam)
		guard let params = paramNode else { return [] }
		
		if params.kind == .nodeList
		{
			for param in params.children {
				param.kind = newKind
			}
			
			return params.children
		}
		
		params.kind = newKind
		return [params]
	}
	
	// ----------------------------------
	private final func parseTypeDeclarationList(
		terminatedBy terminators: [TokenType]) -> [ASTNode]
	{
		var declarations = [ASTNode]()
		
		while let token = lexer.peekToken(), !terminators.contains(token.symbol)
		{
			guard let declaration = parseTypeDeclaration(
				terminatedBy: terminators + [.semicolon])
			else { break }
			
			declarations.append(declaration)
		}
		
		return declarations
	}

	// ----------------------------------
	internal final func parseTypeDeclaration(
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		let typeName = lexer.peekToken()
		guard typeName?.symbol == .identifier else {
			emitError(expected: "identifier", got: typeName)
			return nil
		}
		
		lexer.advance()
		
		let equalToken = lexer.peekToken()
		guard equalToken?.symbol == .isEqualTo else {
			emitError(expected: .isEqualTo, got: equalToken)
			return nil
		}
		
		lexer.advance()
		
		return parseTypeDeclaration(
			named: typeName!,
			indicatedBy: equalToken!,
			terminatedBy: terminators)
	}
	
	// ----------------------------------
	private func parseTypeDeclaration(
		named typeName: Token,
		indicatedBy equalToken: Token,
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(equalToken.symbol == .isEqualTo)
		
		var result: ASTNode? = nil
		
		let value = lexer.peekToken()
		
		if let value = value
		{
			switch value.symbol
			{
				case .identifier, .array, .record:
					if let typeSpec = parseTypeSpecification()
					{
						result = ASTNode(
							typeNamed: ASTNode(typeName: typeName),
							equalsToken: equalToken,
							value: typeSpec
						)
						break
					}
					fallthrough
				
				default: break
			}
		}
		
		guard let typeDeclartion = result else
		{
			emitError(expected: "a constant literal", got: value)
			return nil
		}
		
		expect(anyOf: terminators, consuming: .semicolon)
				
		return typeDeclartion
	}

	// ----------------------------------
	private final func parseConstantDeclarationList(
		terminatedBy terminators: [TokenType]) -> [ASTNode]
	{
		var declarations = [ASTNode]()
		
		while let token = lexer.peekToken(), !terminators.contains(token.symbol)
		{
			guard let declaration = parseConstantDeclaration(
				terminatedBy: terminators + [.semicolon])
			else { break }
			
			declarations.append(declaration)
		}
		
		return declarations
	}

	// ----------------------------------
	internal final func parseConstantDeclaration(
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		let constantName = lexer.peekToken()
		guard constantName?.symbol == .identifier else {
			emitError(expected: "identifier", got: constantName)
			return nil
		}
		
		lexer.advance()
		
		let equalToken = lexer.peekToken()
		guard equalToken?.symbol == .isEqualTo else {
			emitError(expected: .isEqualTo, got: equalToken)
			return nil
		}
		
		lexer.advance()
		
		return parseConstantDeclaration(
			named: constantName!,
			indicatedBy: equalToken!,
			terminatedBy: terminators)
	}
	
	// ----------------------------------
	private func parseConstantDeclaration(
		named constant: Token,
		indicatedBy equalToken: Token,
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(equalToken.symbol == .isEqualTo)
		
		var result: ASTNode? = nil
		
		if let valueNode = parseExpression(
			terminatedBy: [.var, .type, .procedure, .begin, .semicolon])
		{
			result = ASTNode(
				constantNamed: ASTNode(token: constant),
				equalsToken: equalToken,
				value: valueNode
			)
		}
		
		expect(anyOf: terminators, consuming: .semicolon)
				
		return result
	}
	
	// ----------------------------------
	internal final func parseVariableDeclaration(
		terminatedBy terminators: [TokenType] = [.semicolon],
		inRecordDeclaration: Bool = false) -> ASTNode?
	{
		assert(terminators.contains(TokenType.semicolon))
		
		let variableName = lexer.peekToken()
		guard variableName?.symbol == .identifier else
		{
			let varKind = inRecordDeclaration
				? "record field"
				: "variable"
			emitError(expected: "a \(varKind) name", got: variableName)
			return nil
		}
		
		lexer.advance()
		
		let declIndicator = lexer.peekToken()
		guard variableDeclIndicators.contains(declIndicator?.symbol) else
		{
			emitError(
				expectedOneOf: variableDeclIndicators,
				got: declIndicator
			)
			
			return nil
		}
		
		switch declIndicator!.symbol
		{
			case .colon:
				lexer.advance()
				return parseSingleVariableDeclaration(
					startingWith: variableName!,
					terminatedBy: terminators
				)
			case .comma:
				lexer.advance()
				let declarations = parseMultipleVariableDeclarations(
					startingWith: variableName!,
					terminatedBy: terminators
				)
				if declarations.isEmpty { return nil }
				return ASTNode(listOf: declarations)
			
			default: break
		}
		
		return nil
	}
	
	let variableDeclTerminators: [TokenType] =
		[.semicolon, .const, .type, .begin, .procedure]
	
	// ----------------------------------
	/**
	Parse single variable declaration of the form:
	
		x: typename;
	*/
	private func parseSingleVariableDeclaration(
		startingWith variable: Token,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(lexer.peekToken()?.symbol != .colon)
		
		// colon has been consumed already, so we're at the type
		guard let typeSpec = parseTypeSpecification() else { return nil }
		
		expect(anyOf: terminators, consuming: .semicolon)
		
		return ASTNode(variable: variable, ofType: typeSpec)
	}
	
	private let variableDeclIndicators: [TokenType] = [.comma, .colon]
	
	// ----------------------------------
	/**
	Parse multiple variable declarations of the same type of the form:
	
		x, y: typename;
	*/
	private func parseMultipleVariableDeclarations(
		startingWith variable: Token,
		terminatedBy terminators: [TokenType]) -> [ASTNode]
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(lexer.peekToken()?.symbol != .comma)
		
		var variables = parseCommaDelimitedVariables(startingWith: variable)
		
		assert(variables.count > 0)
		assert(lexer.peekToken()?.symbol == .colon)
		
		lexer.advance()
		guard let lastVarDeclaration = parseSingleVariableDeclaration(
			startingWith: variables.removeLast(),
			terminatedBy: terminators)
		else
		{
			emitError("Unable to parse type specifier")
			return []
		}
		
		// Assign the same type as the last declaration to all the rest, and
		// accumulate the ASTNodes
		var declarations = [ASTNode](capacity: variables.count + 1)
		
		for variable in variables {
			declarations.append(
				ASTNode(variable: variable, sameTypeAs: lastVarDeclaration)
			)
		}
		
		declarations.append(lastVarDeclaration)

		return declarations
	}
	
	// ----------------------------------
	private func parseCommaDelimitedVariables(
		startingWith variable: Token) -> [Token]
	{
		var errorEmitted = false
		var variables = [variable]
		
		// first comma has been consumed already
		while let nextToken = lexer.peekToken()
		{
			if nextToken.symbol == .identifier
			{
				variables.append(nextToken)
				lexer.advance()
			}
			else
			{
				if !errorEmitted
				{
					emitError(expected: "an identifier", got: nextToken)
					errorEmitted = true
				}
				
				if nextToken.symbol == .colon
				{
					lexer.advance()
					break
				}
				else if nextToken.symbol == .comma {
					lexer.advance()
					continue
				}
				
				return []
			}
			
			guard let delimiter = lexer.peekToken() else
			{
				if !errorEmitted {
					emitError(expectedOneOf: variableDeclIndicators)
				}
				return []
			}
			
			if delimiter.symbol == .colon { break }
			else if delimiter.symbol != .comma
			{
				if !errorEmitted
				{
					emitError(
						expectedOneOf: variableDeclIndicators,
						got: delimiter
					)
					errorEmitted = true
				}
			}
			else { lexer.advance() }
		}
		
		return variables
	}
	
	// ----------------------------------
	private func parseVariableDeclarationList(
		terminatedBy terminators: [TokenType],
		inRecordDeclaration: Bool) -> [ASTNode]
	{
		var declarations: [ASTNode] = []
		while let token = lexer.peekToken(), !terminators.contains(token.symbol)
		{
			guard let declaration = parseVariableDeclaration(
				terminatedBy: terminators + [.semicolon],
				inRecordDeclaration: inRecordDeclaration)
			else { break }
			
			if declaration.kind == .nodeList {
				declarations.append(contentsOf: declaration.children)
			}
			else {
				declarations.append(declaration)
			}
		}
		
		expect(anyOf: terminators)
		
		return declarations
	}
	
	// MARK:- Parsing type specifiers
	// ----------------------------------
	private let typeSpecifierStarts: [TokenType] =
		[.identifier, .array, .record]
	
	// ----------------------------------
	private func parseTypeSpecification() -> ASTNode?
	{
		// assumption: lexer.peekToken() returns the first token in the
		// type specification.
		
		guard let typeToken = currentToken(ifAnyOf: typeSpecifierStarts) else {
			return nil
		}
		
		lexer.advance()
		
		switch typeToken.symbol
		{
			case .array:
				return parseArrayTypeSpecifier(arrayToken: typeToken)
			
			case .record:
				return parseRecordTypeSpecifier(recordToken: typeToken)
			
			case .identifier:
				return ASTNode(type: typeToken)
			
			default:
				emitError(expected: "type specifier", got: typeToken)
		}
		
		return nil
	}
	
	// ----------------------------------
	private func parseArrayTypeSpecifier(arrayToken: Token) -> ASTNode?
	{
		assert(arrayToken.symbol == .array)
		
		guard let arraySizeNode = parseExpression(terminatedBy: [.of]) else {
			emitError(expected: "array size expression")
			return nil
		}
		
		expect(anyOf: [.of], consuming: .of)
		
		guard let elementTypeNode = parseTypeSpecification() else {
			emitError(expected: "type specifier")
			return nil
		}
		
		return ASTNode(
			array: arrayToken,
			size: arraySizeNode,
			ofElementType: elementTypeNode
		)
	}
	
	// ----------------------------------
	private func parseRecordTypeSpecifier(recordToken: Token) -> ASTNode?
	{
		assert(recordToken.symbol == .record)
		
		let fieldDeclarations = parseVariableDeclarationList(
			terminatedBy: [.end],
			inRecordDeclaration: true
		)
		
		assert(lexer.peekToken()?.symbol == .end)
		lexer.advance()

		return ASTNode(record: recordToken, fields: fieldDeclarations)
	}
	
	// MARK:- Code block parsing
	// ----------------------------------
	/**
	A code block is a sequence of statements enclosed by keywords.  Examples would be BEGIN...END or
	THEN...ELSE
	*/
	
	// ----------------------------------
	/**
	Parse a sequence block that begins with `startSymbol` and is terminated by `.end` to form an
	`ASTNode`.
	
	- Parameters:
		- startSymbol: `TokenType` expected to begin the code block.
		- terminators: An `Array` of `TokenType` that specify what symbols may end the code
			block.
	
	- Returns: An `ASTNode` representing the code block, or `nil`, if the code block could not be
		parsed.
	
	- Note: this method primarily exists as a means for unit testing parsing a code block in
		isolation from its owning context.
	*/
	internal final func parseCodeBlock(
		startingWith startSymbol: TokenType,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		let beginToken = lexer.peekToken()
		guard beginToken?.symbol == startSymbol else
		{
			emitError(expected: startSymbol, got: beginToken)
			return nil
		}
		
		lexer.advance()
		
		return parseCodeBlock(
			startingWith: beginToken!,
			terminatedBy: terminators
		)
	}
	
	// ----------------------------------
	/**
	Parse sequence an sequence of statments that form a code block `ASTNode`
	
	This method is called to parse keyword-delimited statement sequences like BEGIN...END, or
	THEN...ELSE.  In some cases, it is necessary to specify `consumingTerminator` to be `false`.
	For example, when parsing an IF...THEN statement, the THEN portion opens a code block, but it could be
	terminated by "END", "ELSE" or "ELSIF".  In that case, the parser would need to check which one,
	because if it's ELSE, or ELSIF, it not only closes the block that was just parsed, but also opens another
	one.
	
	- Parameters:
		- begin: `Token` that marked the beginning of the code block.  This could be
			`.begin`, but could be others, like `.then`, or `.while`, etc...
		- terminators: An `Array` of `TokenType` that specify what symbols may end the code
			block.
		- consumingTerminator: Specifies whether or not the terminating token should be
			consumed.  Defaults to `true` (consumes the terminating token).

	- Returns: An `ASTNode` representing the code block.
	*/
	private func parseCodeBlock(
		startingWith begin: Token,
		terminatedBy terminators: [TokenType],
		consumingTerminator: Bool = true) -> ASTNode
	{
		let statements = parseStatementSequence(terminatedBy: terminators)
		if consumingTerminator, terminators.contains(lexer.peekToken()?.symbol)
		{
			lexer.advance()
		}
		
		return ASTNode(block: begin, statements: statements)
	}
	
		
	// ----------------------------------
	/**
	Parse a statement sequence terminated by any of the specified `TokenType`s
	
	- Parameter termintors: `Array` of `TokenType` that marks the end of the statement
		sequence
	
	- Returns: an `Array` of `ASTNode`s where each element in the `Array` corresponds to a
		program statement in the source code.
	*/
	private func parseStatementSequence(
		terminatedBy terminators: [TokenType] = []) -> [ASTNode]
	{
		var statements = [ASTNode]()
		
		let statementTerminators = terminators + [.semicolon]
		while let statement = parseStatement(terminatedBy: statementTerminators)
		{
			if statement.kind != .empty {
				statements.append(statement)
			}
		}
		
		return statements
	}
	
	// MARK:- Statement parsing
	// ----------------------------------
	private let statementTerminators: [TokenType] =
		[.semicolon, .end, .else, .elsif]
	
	private let statementStartKeywords: [TokenType] = [.if, .while]

	// ----------------------------------
	internal final func parseStatement(
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		assert(terminators.contains(.semicolon))
		guard let token = lexer.peekToken() else
		{
			emitError(
				expected: "an identifier",
				orOneOf: terminators + statementStartKeywords
			)
			return nil
		}
		
		if terminators.contains(token.symbol) {
			return nil
		}
		
		lexer.advance()
		if let statement = parseStatement(
			startingWith: token,
			terminatedBy: terminators)
		{
			return statement
		}
		
		return ASTNode.empty
	}
		
	let statementIndicators: [TokenType] = [.semicolon, .openParen, .becomes]
	
	// ----------------------------------
	private func parseStatement(
		startingWith identifier: Token,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		assert(terminators.contains(.semicolon))
		assert(
			identifier.symbol == .identifier
			|| statementStartKeywords.contains(identifier.symbol)
		)
		
		return parseControlFlowStatement(startingWith: identifier)
			?? 	parseIdentifierStatement(
					startingWith: identifier,
					terminatedBy: terminators
				)
	}
	
	// ----------------------------------
	private func parseIdentifierStatement(
		startingWith identifier: Token,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		assert(identifier.symbol == .identifier)
		
		guard let nextToken = lexer.peekToken() else
		{
			emitError(
				expected: "assignment or procedure call.  Missing semicolon?"
			)
			return ASTNode(token: identifier)
		}
		
		let ast: ASTNode? =
			parseProcedureCallStatement(startingWith: identifier)
			??	parseAssignment(
					startingWith: identifier,
					terminatedBy: terminators
				)
		
		if ast == nil {
			emitError(expectedOneOf: statementIndicators, got: nextToken)
		}
		
		expect(anyOf: terminators, consuming: .semicolon)
		
		return ast
	}
	
	// ----------------------------------
	private func parseControlFlowStatement(
		startingWith controlToken: Token) -> ASTNode?
	{
		switch controlToken.symbol
		{
			case .if: return parseIfStatement(startingWith: controlToken)
			case .while: return parseWhileStatement(startingWith: controlToken)
			case .identifier: break
			
			default:
				emitError(
					expected: "identifier",
					orOneOf: statementStartKeywords,
					got: controlToken
				)
		}
		return nil
	}
	
	// ----------------------------------
	private func parseWhileStatement(startingWith whileToken: Token) -> ASTNode?
	{
		assert(whileToken.symbol == .while)
		
		return ASTNode(
			while: whileToken,
			condition: parseControlFlowCondition(terminatedBy: [.do]),
			do: parseDoBlock()
		)
	}
	
	// ----------------------------------
	private func parseDoBlock() -> ASTNode
	{
		guard let doToken = currentToken(is: .do, consuming: true) else
		{
			lexer.advance(to: .end, consuming: true)
			return ASTNode(block: Token.doToken, statements: [])
		}
		
		return parseCodeBlock(
			startingWith: doToken,
			terminatedBy: [.end],
			consumingTerminator: false
		)
	}
	
	// ----------------------------------
	private func parseIfStatement(startingWith ifToken: Token) -> ASTNode?
	{
		assert(ifToken.symbol == .if || ifToken.symbol == .elsif)
		
		let condition = parseControlFlowCondition(terminatedBy: [.then])
		let thenBlock = parseThenBlock()
		let elseBlock = parseElseBlock()
		
		expect(anyOf: [.end, .semicolon], consuming: .semicolon)
		
		return ASTNode(
			if: ifToken,
			condition: condition,
			thenBlock: thenBlock,
			elseBlock: elseBlock
		)
	}
	
	// ----------------------------------
	/**
	Parses the condition portion of a control flow statement like IF...THEN or WHILE...DO
	
	- Parameter terminators: `Array` of `TokenType`s that terminate the condition portion of
		the control flow statement.
	*/
	private func parseControlFlowCondition(
		terminatedBy terminators: [TokenType]) -> ASTNode
	{
		assert(terminators == [.then] || terminators == [.do])
		
		guard let condition = parseExpression(terminatedBy: terminators) else
		{
			emitError("Expected expression")
			lexer.advance(to: .then, consuming: false)
			
			return ASTNode(token: Token.trueToken)
		}
		
		return condition
	}
	
	// ----------------------------------
	private func parseThenBlock() -> ASTNode
	{
		guard let thenToken = currentToken(is: .then, consuming: true) else
		{
			lexer.advance(toOneOf: [.else, .elsif, .end], consuming: false)
			return ASTNode(block: Token.thenToken, statements: [])
		}
		
		return parseCodeBlock(
			startingWith: thenToken,
			terminatedBy: [.else, .elsif, .end],
			consumingTerminator: false
		)
	}
	
	// ----------------------------------
	private func parseElseBlock() -> ASTNode
	{
		guard let elseToken =
			currentToken(ifAnyOf: [.else, .elsif, .end], consuming: false)
		else
		{
			lexer.advance(to: .semicolon, consuming: false)
			return ASTNode(block: Token.elseToken, statements: [])
		}
		
		lexer.advance()
		switch elseToken.symbol
		{
			case .else:
				return parseCodeBlock(
					startingWith: elseToken,
					terminatedBy: [.end],
					consumingTerminator: true
				)
			
			case .elsif:
				if let elseBlock = parseIfStatement(startingWith: elseToken) {
					return elseBlock
				}
				fallthrough

			case .end:
				return ASTNode(block: Token.elseToken, statements: [])
			
			default: fatalError("Shouldn't get here")
		}
	}
	
	// ----------------------------------
	private func parseAssignment(
		startingWith identifier: Token,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		let rValue = parseIdentifierExpression(identifier)!
		
		guard let assignOp = currentToken(is: .becomes, consuming: true) else {
			return rValue
		}
		
		assert(rValue.isAssignable)
		assert(assignOp.symbol == .becomes)
		
		let startOfExpression = lexer.peekToken()
		if let rvalue = parseExpression(
			terminatedBy: terminators + [.semicolon])
		{
			return ASTNode(
				assignment: assignOp,
				lvalue: rValue,
				rvalue: rvalue
			)
		}
		else { emitError(expected: "expression", got: startOfExpression)}
		
		return nil
	}
	
	// ----------------------------------
	private func parseProcedureCallStatement(
		startingWith procedureName: Token) -> ASTNode?
	{
		assert(procedureName.symbol == .identifier)
		
		guard let indicator = lexer.peekToken(),
			indicator.symbol == .openParen || indicator.symbol == .semicolon
		else { return nil }
		
		assert(
			indicator.symbol == .openParen
			|| indicator.symbol == .semicolon
		)
		
		var procCallAST: ASTNode? = nil
		if indicator.symbol == .semicolon
		{
			lexer.advance()
			procCallAST = ASTNode(function: procedureName, parameters: [])
		}
		else if indicator.symbol == .openParen
		{
			lexer.advance()
			let params =
				commaSeparatedExpressionList(terminatedBy: .closeParen)
			procCallAST =
				ASTNode(function: procedureName, parameters: params)
		}
		
		return procCallAST
	}
	
	private let paramListTerminators: [TokenType] = [.comma, .closeParen]
	
	// ----------------------------------
	/**
	Generate an `Array` of `ASTNode`s for a comma separated list of expressions, for example for the
	actual parameters in a function call.
	
	- Parameter terminator: `TokenType` to use as terminating symbol for the expression list.
	
	- Returns:`Array` of `ASTNode`s where each element of the `Array` corresponds to an
		expression in the list, in the same order.
	*/
	private func commaSeparatedExpressionList(
		terminatedBy terminator: TokenType) -> [ASTNode]
	{
		var params = [ASTNode]()
		
		var errorEmitted = false
		
		while let token = lexer.peekToken(), token.symbol != terminator
		{
			if let param = parseExpression(terminatedBy: paramListTerminators)
			{
				params.append(param)
				if lexer.peekToken()?.symbol == .comma { lexer.advance() }
			}
			else if lexer.peekToken()?.symbol == .comma
			{
				if !errorEmitted
				{
					emitError(
						expected: "an expression",
						got: lexer.peekToken()
					)
					errorEmitted = true
				}
				lexer.advance()
			}
		}
		lexer.advance()

		return params
	}
	
	// ----------------------------------
	private func parseIdentifierExpression(_ token: Token) -> ASTNode?
	{
		guard token.symbol == .identifier else { return nil }
		
		var variable = ASTNode(token: token)
		
		switch lexer.peekToken()?.symbol
		{
			case .openBracket: variable = parseArrayElement(for: variable)
			case .period: variable = parseRecordField(for: variable)
			default: break
		}
		
		return parseArrayOrRecordExpression(for: variable)
	}
	
	// ----------------------------------
	private func parseArrayOrRecordExpression(for variable: ASTNode) -> ASTNode
	{
		let arrayOrRecord: ASTNode
		switch lexer.peekToken()?.symbol
		{
			case .openBracket: arrayOrRecord = parseArrayElement(for: variable)
			case .period: arrayOrRecord = parseRecordField(for: variable)
			default: return variable
		}
		
		return parseArrayOrRecordExpression(for: arrayOrRecord)
	}
	
	// ----------------------------------
	private func parseArrayElement(for array: ASTNode) -> ASTNode
	{
		assert(array.isArrayIndexable)
		
		let bracket = lexer.peekToken()
		assert(bracket?.symbol == .openBracket)
		lexer.advance()
		
		let index = parseExpression(terminatedBy: [.closeBracket])
			?? ASTNode(token: Token.zero)
		
		expect(.closeBracket, consuming: true)
		
		return ASTNode(array: array, bracket: bracket!, index: index)
	}
	
	// ----------------------------------
	private func parseRecordField(for record: ASTNode) -> ASTNode
	{
		assert(record.isFieldSelectable)
		
		let dot = lexer.peekToken()
		assert(dot?.symbol == .period)
		lexer.advance()
		
		guard let field = lexer.peekToken(), field.symbol == .identifier else {
			return ASTNode(token: Token.zero)
		}
		
		lexer.advance()

		return ASTNode(
			record: record,
			dot: dot!,
			field: ASTNode(token: field)
		)
	}
	
	// MARK:- Expression parsing: Shunting Yard algorithm
	// ----------------------------------
	/**
	Covenience method for testing whether operator1 should be processed before `operator2` for the
	Shunting Yard algorithm in `processExpression()`
	
	This method uses the two operators' precedences, breaking the tie by preferring `operator1` if it is
	left-associative, otherwise it prefers `operator2`.
	
	- Parameters:
		- operator1: `Token` containing a binary or postfix unary operator
		- operator2: `Token` containing a binray or postfix unary operator
	
	- Returns: `true` if `operator1` should be processed before `operator2`,  or `false`
		otherwise.
	*/
	private func process(
		_ operator1: Token,
		before operator2: Token) -> Bool
	{
		let op1Prec = operator1.precedence
		let op2Prec = operator2.precedence
		let binaryOrPostfix = operator1.operatorGroup == .binary
			|| operator1.operatorGroup == .postfixUnary
		
		return binaryOrPostfix &&
			(op1Prec > op2Prec
				|| (op1Prec == op2Prec && operator1.associativity == .left)
			)
	}
	
	// ----------------------------------
	/**
	Convenience function for making an `ASTNode` from the operatator at the top of the `operators` stack
	and its corresponding operands from the `operands` stack for use with the Shunting Yard algorithm in
	`processExpression()` and related methods.
	
	- Parameters:
		- operators:  operator stack
		- operands: operand stack
	
	- Returns: The `ASTNode` resuling from combining the top operator on the `operators` stack
		with its corresponding operands from `operands.`
	
	- Note: Both `operators` and `operands` are modified by this method.
	*/
	func makeASTNode(
		from operators: inout Stack<Token>,
		and operands: inout Stack<ASTNode>) -> ASTNode
	{
		assert(operators.count > 0)
		
		let operatorToken = operators.pop()!
		let operatorNode = ASTNode(token: operatorToken)
		
		switch operatorToken.operatorGroup
		{
			case .binary:
				assert(operands.count > 1)
				let op2 = operands.pop()! // preserve original order
				operatorNode.addChild(operands.pop()!)
				operatorNode.addChild(op2)
			
			case .prefixUnary:
				assert(operands.count > 0)
				operatorNode.addChild(operands.pop()!)
			
			default: assertionFailure("shouldn't get here!")
		}
		
		return operatorNode
	}
	
	// ----------------------------------
	/**
	Parses an identifier that begins a function call for the Shunting Yard algorithm in
	`parseExpression(terminatedBy:)`
	
	This is method deviates from the usual method in the Shunting Yard algorithm in that classically, the
	parentheses that are part of the function call are parsed along with the rest of the parentheses, but we
	parse the function call completely independently, then push the resulting `ASTNode` onto the
	`operands` stack.
	
	- Parameters:
		- functionName: The token containing the identifier that could be a function name.
		- operators:  operator stack
		- operands: operand stack
	
	- Returns: `true` if `functionName` is parsed as a function call, and `false` otherwise.

	- Note: Both `operators` and `operands` are modified by this method.
	*/
	private func parseFunctionCall(
		_ functionName: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack) -> Bool
	{
		if lexer.peekToken()?.symbol == .openParen
		{
			lexer.advance()
			let parameters =
				commaSeparatedExpressionList(terminatedBy: .closeParen)
			
			operands.push(
				applyPossiblePrefixUnary(
					atTopOf: &operators,
					to: ASTNode(
						function: functionName,
						parameters: parameters)
				)
			)
			return true
		}
		return false
	}
	
	// ----------------------------------
	/**
	Parses a identifier that is variable or constant as part of the Shunting Yard algorithm in
	`processExpression()`.
	
	If the top of the `operators` stack contains a prefix unary operator, it is combined with the token and
	with the resulting ASTNode pushed back onto the `operands` stack. Otherwise the variable or constant
	forms an `ASTNode` by itself and is pushed onto the `operands` stack.
	
	- Parameters:
		- token: The token containing the identifier or constant.
		- operators:  operator stack
		- operands: operand stack
		
	- Note: Both `operators` and `operands` are modified by this method.
	*/
	private func parseVariableOrConstExpression(
		_ token: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		operands.push(
			applyPossiblePrefixUnary(
				atTopOf: &operators,
				to: parseIdentifierExpression(token) ?? ASTNode(token: token)
			)
		)
	}
	
	// ----------------------------------
	/**
	Processes both binary and postfix unary operators as part of the Shunting Yard algorithm in
	`parseExpression(terminatedBy:)`
	
	Ultimately it pushes the current operator onto the `operators` stack, but first it loops through
	constructing the `operators` stack combining them with their corresponding operands from the
	`operands` stack to form `ASTNode`s to form new operands that are pushed back onto the
	`operands` stack so long as the operator at the top of the stack has higher precendence as the
	current token (or the same precedent but is left-associative).
	
	- Parameters:
		- token: The token containing the binary (infix) or postfix unary operator.
		- operators:  operator stack
		- operands: operand stack
		
	- Note: Both `operators` and `operands` are modified by this method.
	*/
	private func parseInfixPostfixExpr(
		_ token: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		while let stackTop = operators.top, stackTop.symbol != .openParen
		{
			guard process(stackTop, before: token) else {
				break
			}
			
			let node = makeASTNode(from: &operators, and: &operands)
			operands.push(node)
		}
		
		operators.push(token)
	}
	
	// ----------------------------------
	/**
	Apply prefix unary operator at the top of the stack, if there is one there, to `operand`
	
	- Parameters:
		- operators: operator stack.
		- operand: `ASTNode` representing an expression to which a possible prefix unary operator
			can be applied.
	
	- Returns: An `ASTNode` which will contain the result of applying the prefix unary operator at the
		top of the stack to `operand`, or if there is no prefix unary operator at the top of the stack,
		`operand` itself is returned.
	*/
	private func applyPossiblePrefixUnary(
		atTopOf operators: inout OperatorStack,
		to operand: ASTNode) -> ASTNode
	{
		if operators.top?.operatorGroup == .prefixUnary {
			return ASTNode(token: operators.pop()!, child: operand)
		}
		
		return operand
	}
	
	// ----------------------------------
	/**
	Final processing of operands remaining on the operand stack for the Shunting Yard algorithm in
	`parseExpression(terminatedBy:)`.
	
	This method loops through popping off operators from the `operators` stack, along with their
	corresponding operands from the `operands` stack, combining them to form AST subexpressions and
	pushing them on to the `operands` stack.
	
	- Parameters:
		- operators:  operator stack
		- operands: operand stack
	
	- Note: Both `operators` and `operands` are modified by this method.
	*/
	private func processRemainingStackedOperators(
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		var errorEmitted = false
		while let stackTop = operators.top
		{
			if stackTop.symbol == .openParen
			{
				if !errorEmitted
				{
					emitError(expected: .closeParen, got: stackTop)
					errorEmitted = true
				}
				continue
			}
			
			let node = makeASTNode(from: &operators, and: &operands)
			operands.push(node)
		}
	}
	
	// ----------------------------------
	/**
	Parse an infix expression using Edgar Djikstra's Shunting Yard algorithm.
	
	The name, Shunting Yard, refers to a shunting yard or switch yard for trains, because you can imagine it
	operates the way a shunting yard would use a T-junction (or maybe it's more approciate to call it a
	Y-junction) on a railroad to move cars around to put them in the order they want.  One leg of the T
	represents the output, one leg represents an operator stack, and the remaining leg represents an
	operand stack.
	
	This algorithm can be a little hard to follow, and without extracting subparts into separate functions, it can
	be kind of long.  I hope doing that has made it easier to follow, but it's arguable that seeing all the moving
	parts together would be clearer despite the length.  It's hard to say which is better in this particular case.
	
	The "big picture" of the algorithm is that it basically combines operators along with their operands into
	single operands until it gets down to just one operand left, which is itself the abstract syntax tree for the
	whole expression.  If it helps, you can imagine instead of a parser generating an AST, the algorithm is
	being used by a calculator to generate a number.  Everytime an operator is combined with its operands,
	a calculator would get a number that replaces that operator and operands, and that number becomes an
	operand for another operator until there are no more left, and then you have the final answer.   The main
	difference in our case is that instead of numbers we have identifiers that stand in for values, so when we
	combine an operator with its operands, instead of number, we get a little bundle  called an abstract syntax
	tree that encapsulates the operator and operands as a single thing, which becomes a new operand.  So
	in our parser the AST is a symbolic version of the numbers a calculator generates.  The tricky bit to
	understand has to do with how the algorithm  handles operator precedence so that it does that
	combination for higher precedence operators before lower precedence ones.
	
	To understand it you need to understand two concepts about the ordering of operators in infix expressions.
	
	The first is the idea of precedence.  This just the idea that some operations should be performed on their
	adjacent operands before others even though they come later in the expression (reading left to right).   If
	for example, you are parsing `1 + 2 * 3`, multiplication has higher precedence than addition, so it's
	parsed as `1 + (2 * 3)`.  If you remember "My Dear Aunt Sally" from grade school, you've basically
	got the concept, though there are more precedence levels than just those for multiplication/division and
	addition/subtraction.
	
	The second idea is that of left- vs right-associativity.  This serves as a kind of tie-breaker when operators
	have the same precedence.  Most operators can be taken as  left associative, which means given
	the same precedence level, they will be grouped from left to right.  For example `1 + 2 + 3` is parsed
	as `(1 + 2) + 3`.  Technically addition and multiplication can have either right- or left-associativity,
	while subtraction and division have strictly left-associativity.
	
	Assignment operators tend to have right-associativity, though they do not make proper r-values  in
	Oberon0, because an assignment itself does not have a value (though it assigns a value to a variable).
	To use an example from C, which can use assignment as an r-value,`a = b = c` is parsed as
	`a = (b = c)`, which assigns `c` to `b` and then the expression `(b = c)` takes on the new
	value of `b` which is `c` and that value is assigned to `a`.
	
	If you got lost in the previous paragraph at the mention of "r-value", don't worry.  It's a simple enough
	concept. An r-value (or right hand value) is an expression that can act as the *source* for an assignment.
	It is an expression that can occur on the right side of an assignment operator, hence the name, though
	they can occur in other contexts as well.  But in order for it to be the source of an assignment, it must
	evaluate to a value, typically at runtime, otherwise there would be nothing to assign.  In contrast, an
	l-value (left hand value) is an expression that can act as the *destination* for an assignment.  That is to
	say, it can occur on the left side of an assignment operator.  In most languages, including Oberon,
	l-values only evaluate to a location, the place the value is to be stored, not a value, and the assignment
	operation itself is neither an l-value, nor r-value.  It is just a statement that involves a l-value and an
	r-value.  C and C++ are unusual in allowing the assignment operator to be used as an l-value.  This little
	aside isn't important to the Shunting Yard algorithm, except that C's assignment operator makes a really
	good example of right-associativity.

	The basic idea of the Shunting Yard algorithm is that by cleverly maintaining separate stacks of operators
	and operands, you can parse infix expressions including operators of different precedence levels, into a
	properly constructed abstract syntax tree for the expression, or into a the equivalent postfix notion string
	output, though it's the former case that we're interested in.
	
	In the classic version (without supporting unary operators), one reads tokens until there are no more, or
	until a terminating symbol is reached for expressions in the token language.
	
	If that token is a constant or identifier, it is pushed on to an operand stack.
	
	If it is an operator, one loops through the operator stack.  As long as the operator at the top of the stack
	has higher precendece than the one represented by the current token, or if it's the same precedence but
	is a left-associative operator, then one pops that operator off of the operator stack, and  pops its
	arguments off of the operand stack, constructs an `ASTNode` out of them, and pushes that node  onto
	the operand stack.  The termination of the loop seems like a long list of conditions, but actually they're
	pretty simple.  The loop terminates if
	
	1) the operator stack has been exhausted, or
	2) if an open parenthesis is found at the top of operator stack, since this indicates we're processing an
		parenthetical expression, and now we've processed it backwards all the way to the start of the
		expresison, or
	3) if the operator represented by the current token has higher precedence than the one at the top of
		the stack, which includes if they are the same precendence level, but the one at the top of the
		stack is not left-associative.
	
	Then after this inner loop terminates, the token is pushed on the operator stack.  The idea here is to
	process higher precedence operators that have already been encountered (ie, on the operator stack)
	before pushing the current operator onto the stack.
	
	If it is an open parenthesis, it is pushed onto the stack (ie. start a new parenthetical expression)
	
	If it is a close parenthesis, the operators are popped off of the operator stack, and their operands off of
	the operand stack, combined to form an `ASTNode`, which is then pushed back on to the operand stack,
	until an open parenthesis is reached, which is popped off and discarded as the entire parenthetical
	expression has now been parsed and it's AST is at the top of the operand stack.
	
	The token reading loop continues until all tokens have been read (or a terminating symbol is reached
	like "`;`: marking the end of a statement).
	
	Once all tokens have been read, whatever operators remain on the operator stack are popped off, along
	with their parameters from the operand stack, combined to form a new `ASTNode`, which is pushed back
	onto the operand stack. Wash. Rinse. Repeat until all operators have been popped from the operator
	stack, leaving only one `ASTNode` on the operand stack, and that node is your AST for the expression.
	
	- Note:
	This implementation makes a few adjustments to the classic algorithm to support function calls and unary
	operators, and to handle parenthetical expression differenty.
	
	Parenthetical expressions are handled by a recursive call to `parseExpression(terminatedBy:)`,
	specifying a close parenthesis as the terminator.
	
	Function calls are supported by parsing them separately, generating an AST for the call, and then pushing
	that AST onto the operand stack.
	
	Unary operators come in two flavors: prefix and postfix.   Oberon, like most programming
	languages, only supports prefix unary operators, but it turns out that postfix unary operators are trivial to
	support, because they work exactly the same as binary operators, except that they only have a single
	operand, provided they have higher precendence than binary operators, which generally is the only way
	they would make sense in an otherwise infix notation.
	
	Prefix unary operators require a special handling.  The handling of the operator itself is easy enough, as
	it's just pushed onto the operator stack, but whenever an identifier, constant, function, or close parenthesis
	is processed, the top of the operator stack is checked to see if it is a prefix unary operator.  If so, it is
	popped off of the operator stack, combined with the token to form an `ASTNode`, which is then pushed
	onto the operand stack.
	
	- Parameter terminator: `Array` of `TokenType`s that can terminates the expression.
		Passing `[]`, the default, uses only the end of input as the terminator
	
	- Returns: An `ASTNode` representing the parsed expression, or `nil` if the parse reached end of
	input without obtaining tokens to generate the node.
	*/
	internal final func parseExpression(
		terminatedBy terminators: [TokenType] = []) -> ASTNode?
	{
		var operators = OperatorStack()
		var operands = OperandStack()
		
		let tokenTypesAfterOperand =
			TokenType.binaryOperatorSymbols + TokenType.postfixUnaryOperators
		
		var expectedTokenTypes = TokenType.expressionStartSymbols
		
		var lastTokenWasOperand = false
		var terminatorFound = false
		tokenLoop: while var token = lexer.peekToken()
		{
			if terminators.contains(token.symbol)
			{
				terminatorFound = true
				break
			}
			
			if !expectedTokenTypes.contains(token.symbol)
			{
				if expectedTokenTypes == TokenType.expressionStartSymbols
				{
					emitError(
						expected: "an identifier or number",
						orOneOf: expectedTokenTypes - [.identifier, .number],
						got: token
					)
				}
				else if expectedTokenTypes == tokenTypesAfterOperand {
					emitError(expected:"binary operator", got: token)
				}
				else {
					emitError(expectedOneOf: expectedTokenTypes, got: token)
				}
				
				break
			}
			
			lexer.advance()
			
			if !lastTokenWasOperand
			{
				if token.symbol == .plus {
					token.symbol = .unaryPlus
				}
				else if token.symbol == .minus {
					token.symbol = .unaryMinus
				}
			}
			
			var tokenIsOperand = true
			
			switch token.symbol
			{
				case .identifier:
					if parseFunctionCall(token, &operators, &operands)
					{
						expectedTokenTypes = tokenTypesAfterOperand
						break
					}
					fallthrough // variable or named constant

				case .number: // constant
					parseVariableOrConstExpression(token, &operators, &operands)
					expectedTokenTypes = tokenTypesAfterOperand

				case .openParen:
					if let expr = parseExpression(terminatedBy: [.closeParen])
					{
						operands.push(
							applyPossiblePrefixUnary(
								atTopOf: &operators,
								to: expr
							)
						)
					}
					expectedTokenTypes = tokenTypesAfterOperand
					lexer.advance()
				
				case .closeParen:
					assertionFailure(
						"Unexpected \")\".  Should be handled by nested call "
						+ "to parseExpression()"
					)
					continue
				
				default: switch token.operatorGroup
				{
					case .prefixUnary:
						operators.push(token)
						expectedTokenTypes = TokenType.expressionStartSymbols
					
					case .binary, .postfixUnary:
						parseInfixPostfixExpr(token, &operators, &operands)
						expectedTokenTypes = TokenType.expressionStartSymbols
						tokenIsOperand = false
					
					default:
						emitError(
							"Unexpected symbol, \(token.srcString), in "
							+ "expression",
							for: token
						)
						break tokenLoop
				}
			}
			
			lastTokenWasOperand = tokenIsOperand
		}
		
		if !terminatorFound && terminators.count > 0 {
			emitError(expectedOneOf: terminators)
		}
		
		processRemainingStackedOperators(&operators, &operands)
		
		if operands.isEmpty {
			emitError("Empty expression")
		}
		else if operands.count > 1 {
			emitError("Unprocessed operands while parsing expression")
		}

		return operands.top
	}
	
	// MARK:- Error handling helper methods
	// ----------------------------------
	/**
	Check that one of the expected symbols matches the `token`, and emit an error if not.
	
	Advances the lexer beyond  the current token if it matches `consumableToken`.
	
	- Parameters:
		- token:
		- expectedSymbols: `Array` of `TokenType`, any one of which is expected to match the
			current token.
		- consumableSymbol: a `TokenType`, which if it matches the type of the current token, the
			lexer is advanced to the next token.  `consumableSymbol` must be one of
			`expectedSymbols`.
	
	- Returns: `token` if the current token is of any of the token types listed in `expectedSymbols`,
		or`nil` otherwise
	*/
	@discardableResult
	private func expect(
		_ token: Token?,
		isOneOf expectedSymbols: [TokenType],
		consuming consumableSymbol: TokenType? = nil) -> Token?
	{
		assert(expectedSymbols.count > 0)
		assert(
			consumableSymbol == nil
			|| expectedSymbols.contains(consumableSymbol)
		)
		
		let actualSymbol = lexer.peekToken()
		if !expectedSymbols.contains(actualSymbol?.symbol)
		{
			emitError(expectedOneOf: expectedSymbols, got: actualSymbol)
			return nil
		}
		else if actualSymbol?.symbol == consumableSymbol {
			lexer.advance()
		}
		
		return token
	}
	
	// ----------------------------------
	/**
	Check that one of the expected symbols matches the `token`, and emit an error if not.
	
	Advances the lexer beyond  the current token if it matches `consumableToken`.
	
	- Parameters:
		- token:
		- expectedSymbols: `Array` of `TokenType`, any one of which is expected to match the
			current token.
		- consumableSymbol: a `TokenType`, which if it matches the type of the current token, the
			lexer is advanced to the next token.  `consumableSymbol` must be one of
			`expectedSymbols`.
	
	- Returns: `true` if the current token is of any of the token types listed in `expectedSymbols`,
		or`false` otherwise
	*/
	@discardableResult
	private func expect(
		_ token: Token?,
		isOneOf expectedSymbols: [TokenType],
		consuming consumableSymbol: TokenType? = nil) -> Bool
	{
		return expect(
			token,
			isOneOf: expectedSymbols,
			consuming: consumableSymbol) != nil
	}
	
	// ----------------------------------
	/**
	Check that one of the expected symbols matches the current token, and emit an error if not.
	
	Advances the lexer beyond  the current token if it matches `consumableToken`.
	
	- Parameters:
		- expectedSymbols: `Array` of `TokenType`, any one of which is expected to match the
			current token.
		- consumableSymbol: a `TokenType`, which if it matches the type of the current token, the
			lexer is advanced to the next token.  `consumableSymbol` must be one of
			`expectedSymbols`.
	
	- Returns: `true` if the current token is of any of the token types listed in `expectedSymbols`,
		or`false` otherwise
	*/
	@discardableResult
	private func expect(
		anyOf expectedSymbols: [TokenType],
		consuming consumableSymbol: TokenType? = nil) -> Bool
	{
		return expect(
			lexer.peekToken(),
			isOneOf: expectedSymbols,
			consuming: consumableSymbol) != nil
	}
	
	// ----------------------------------
	/**
	Check that expected symbols matches the current token, and emit an error if not.
	
	Advances the lexer beyond  the current token `consuming` is `true`.
	
	- Parameters:
		- expectedSymbol: `TokenType`, which is expected to match the current token.
		- consuming: `Bool` specifying whether the current token should be consumed, if it matches
			the `expectedSymbol`.  If `true` the token is consumed.  If `false`, the current token
			is left in the stream ot be read.
	
	- Returns: `true` if the current token matches `expectedSymbol`, or`false` otherwise
	*/
	@discardableResult
	private func expect(
		_ expectedSymbol: TokenType,
		consuming: Bool) -> Bool
	{
		return expect(
			lexer.peekToken(),
			isOneOf: [expectedSymbol],
			consuming: consuming ? expectedSymbol : nil) != nil
	}

	// ----------------------------------
	/**
	Get the current token if it matches any of  the expected symbols, and emit an error if not.
	
	Advances the lexer beyond  the current token if it matches `consumableToken`.
	
	- Parameters:
		- expectedSymbols: `Array` of `TokenType`, any one of which is expected to match the
			current token.
		- consumableSymbol: a `TokenType`, which if it matches the type of the current token, the
			lexer is advanced to the next token.  `consumableSymbol` must be one of
			`expectedSymbols`.
	
	- Returns: the current `Token` if it is of any of the token types listed in `expectedSymbols`,
		or`nil` otherwise
	*/
	private func currentToken(
		ifAnyOf expectedSymbols: [TokenType],
		consuming consumableSymbol: TokenType? = nil) -> Token?
	{
		return expect(
			lexer.peekToken(),
			isOneOf: expectedSymbols,
			consuming: consumableSymbol)
	}
	
	// ----------------------------------
	/**
	Get the current token if it matches any of  the expected symbols, and emit an error if not.
	
	Advances the lexer beyond  the current token if `consumes` is `true`.
	
	- Parameters:
		- expectedSymbols: `Array` of `TokenType`, any one of which is expected to match the
			current token.
		- consumableSymbol: a `Bool` specifying whether the expected token should be consumed
			from the token stream.
	
	- Returns: the current `Token` if it is of any of the token types listed in `expectedSymbols`,
		or`nil` otherwise
	*/
	private func currentToken(
		ifAnyOf expectedSymbols: [TokenType],
		consuming: Bool) -> Token?
	{
		let token = currentToken(ifAnyOf: expectedSymbols, consuming: nil)
		
		if consuming { lexer.advance() }
		return token
	}
	
	// ----------------------------------
	/**
	Get the current token if it matches the expected symbol, and emit an error if not.
	
	Advances the lexer beyond  the current token if `consumes` is `true`.
	
	- Parameters:
		- expectedSymbol: `TokenType`which is expected to match the current token.
		- consumableSymbol: a `Bool` specifying whether the expected token should be consumed
			from the token stream.
	
	- Returns: the current `Token` if it is of the token type `expectedSymbol`, or`nil` otherwise
	*/
	private func currentToken(
		is expectedSymbol: TokenType,
		consuming: Bool) -> Token?
	{
		return currentToken(ifAnyOf: [expectedSymbol], consuming: true)
	}
	
	// MARK:- Error Reporting
	// ---------------------------------------------------
	public final func emitError(_ msg: String, for token: Token?)
	{
		if let token = token {
			emitError(msg, at: token.sourceRange.lowerBound)
		}
		else { emitError(msg) }
	}
	
	// ---------------------------------------------------
	public final func emitError(
		expected prefix: String,
		orOneOf tokenTypes: [TokenType] = [],
		got actual: Token? = nil)
	{
		var message = "Expected \(prefix)"
		
		assert(prefix != "" || tokenTypes.count > 0)
		
		if tokenTypes.count > 0
		{
			message += prefix == "" ? "" : " or "
			message += tokenTypes.count > 1
				? "one of \(list: tokenTypes, .or)"
				: "\(list: tokenTypes)"
		}
		
		if let actualToken = actual {
			message += ", but got \"\(actualToken.srcString)\"."
		}
		else { message += "." }
		
		emitError(message, for: actual)
	}
	
	// ---------------------------------------------------
	public final func emitError(
		expectedOneOf tokenTypes: [TokenType],
		got actual: Token? = nil)
	{
		emitError(expected: "", orOneOf: tokenTypes, got: actual)
	}
	
	// ---------------------------------------------------
	public final func emitError(expected: TokenType, got actual: Token? = nil) {
		emitError(expected: "", orOneOf: [expected], got: actual)
	}
}

// ---------------------------------------------------
extension Array where Element == TokenType
{
	// ---------------------------------------------------
	static func - (left: Array, right: Array) -> Array
	{
		var result = Array()
		result.reserveCapacity(left.count)
		
		for element in left {
			if !right.contains(element) { result.append(element) }
		}
		
		return result
	}
}

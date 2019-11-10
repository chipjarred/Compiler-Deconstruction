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
final class NewParser
{
	private typealias OperatorStack = Stack<Token>
	private typealias OperandStack = Stack<ASTNode>
		
	var lexer: Lexer
	
	// ----------------------------------
	init(source: String, sourceName: String = "<<NONE>>")
	{
		let sourceStream = InputStream(contentsOf: source)
		sourceStream.open()
		self.lexer = Lexer(sourceStream: sourceStream, sourceName: sourceName)
	}
	
	// ----------------------------------
	public final func parse() -> ASTNode?
	{
		// FIXME: This needs to do actual parsing
		return nil
	}
		
	// MARK:- Declaration parsing
	// ----------------------------------
	internal final func parseTypeDeclaration(
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		let typeName = lexer.peekToken()
		guard typeName?.symbol == .identifier else {
			lexer.mark(expected: "identifier", got: typeName)
			return nil
		}
		
		lexer.advance()
		
		let equalToken = lexer.peekToken()
		guard equalToken?.symbol == .isEqualTo else {
			lexer.mark(expected: .isEqualTo, got: equalToken)
			return nil
		}
		
		lexer.advance()
		
		return parseTypeDeclaration(
			named: typeName!,
			indicatedBy: equalToken!,
			terminatedBy: terminators)
	}
	
	// ----------------------------------
	internal final func parseTypeDeclaration(
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
			lexer.mark(expected: "a constant literal", got: value)
			return nil
		}
		
		expect(anyOf: terminators, consuming: .semicolon)
				
		return typeDeclartion
	}
	

	
	// ----------------------------------
	internal final func parseConstantDeclaration(
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		let constantName = lexer.peekToken()
		guard constantName?.symbol == .identifier else {
			lexer.mark(expected: "identifier", got: constantName)
			return nil
		}
		
		lexer.advance()
		
		let equalToken = lexer.peekToken()
		guard equalToken?.symbol == .isEqualTo else {
			lexer.mark(expected: .isEqualTo, got: equalToken)
			return nil
		}
		
		lexer.advance()
		
		return parseConstantDeclaration(
			named: constantName!,
			indicatedBy: equalToken!,
			terminatedBy: terminators)
	}
	
	// ----------------------------------
	internal final func parseConstantDeclaration(
		named constant: Token,
		indicatedBy equalToken: Token,
		terminatedBy terminators: [TokenType] = [.semicolon]) -> ASTNode?
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(equalToken.symbol == .isEqualTo)
		
		var result: ASTNode? = nil
		
		let value = lexer.peekToken()
		if value?.symbol == .number
		{
			lexer.advance()
			result = ASTNode(
				constantNamed: ASTNode(token: constant),
				equalsToken: equalToken,
				value: ASTNode(token: value!)
			)
		}
		else
		{
			lexer.mark(expected: "a constant literal", got: value)
			return nil
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
			lexer.mark(expected: "a \(varKind) name", got: variableName)
			return nil
		}
		
		lexer.advance()
		
		let declIndicator = lexer.peekToken()
		guard variableDeclIndicators.contains(declIndicator?.symbol) else
		{
			lexer.mark(
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
	
		x, y: typeanme;
	*/
	private func parseMultipleVariableDeclarations(
		startingWith variable: Token,
		terminatedBy terminators: [TokenType]) -> [ASTNode]
	{
		assert(terminators.contains(TokenType.semicolon))
		assert(lexer.peekToken()?.symbol != .comma)
		
		var variables = Stack<Token>()
		variables.push(variable)
		
		var errorEmitted = false
		var lastVarDeclaration: ASTNode! = nil
		
		// first comma has been consumed already, so we're at the type
		while let nextToken = lexer.peekToken()
		{
			if nextToken.symbol == .identifier
			{
				variables.push(nextToken)
				lexer.advance()
			}
			else
			{
				if !errorEmitted
				{
					lexer.mark(expected: "an identifier", got: nextToken)
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
					lexer.mark(expectedOneOf: variableDeclIndicators)
				}
				return []
			}
			
			if delimiter.symbol == .colon
			{
				lexer.advance()
				lastVarDeclaration = parseSingleVariableDeclaration(
					startingWith: variables.pop()!,
					terminatedBy: terminators
				)
				break
			}
			else if delimiter.symbol != .comma
			{
				if !errorEmitted
				{
					lexer.mark(
						expectedOneOf: variableDeclIndicators,
						got: delimiter
					)
					errorEmitted = true
				}
			}
			else { lexer.advance() }
		}
		
		guard lastVarDeclaration != nil else
		{
			if !errorEmitted { lexer.mark("Unable to parse type specifier") }
			return []
		}
		
		var declarations = [ASTNode](capacity: variables.count + 1)
		declarations.append(lastVarDeclaration)
		
		while let variable = variables.pop()
		{
			declarations.append(
				ASTNode(variable: variable, sameTypeAs: lastVarDeclaration)
			)
		}
		
		return declarations.reversed()
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
				return ASTNode(typeName: typeToken)
			
			default:
				lexer.mark(expected: "type specifier", got: typeToken)
		}
		
		return nil
	}
	
	// ----------------------------------
	private func parseArrayTypeSpecifier(arrayToken: Token) -> ASTNode?
	{
		assert(arrayToken.symbol == .array)
		
		guard let arraySizeNode = parseExpression(terminatedBy: [.of]) else {
			lexer.mark(expected: "array size expression")
			return nil
		}
		
		expect(anyOf: [.of], consuming: .of)
		
		guard let elementTypeNode = parseTypeSpecification() else {
			lexer.mark(expected: "type specifier")
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
			lexer.mark(expected: startSymbol, got: beginToken)
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
			lexer.mark(
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
			startingWithIdentifier: token,
			terminatedBy: terminators)
		{
			return statement
		}
		
		return ASTNode.empty
	}
		
	let statementIndicators: [TokenType] = [.semicolon, .openParen, .becomes]
	
	// ----------------------------------
	internal final func parseStatement(
		startingWithIdentifier identifier: Token,
		terminatedBy terminators: [TokenType]) -> ASTNode?
	{
		assert(terminators.contains(.semicolon))
		assert(identifier.symbol == .identifier)
		
		guard let nextToken = lexer.peekToken() else
		{
			lexer.mark(
				expected: "assignment or procedure call.  Missing semicolon?"
			)
			return ASTNode(token: identifier)
		}
		
		var ast: ASTNode? = nil
		switch nextToken.symbol
		{
			case .semicolon, .openParen:
				ast = parseProcedureCallStatement(
					startingWith: identifier,
					callIndicator: nextToken
				)
			
			case .becomes:
				ast = parseAssignment(
					startingWith: identifier,
					assignmentIndicator: nextToken,
					terminatedBy: terminators
				)
			
			default:
				lexer.mark(expectedOneOf: statementIndicators, got: nextToken)
		}
		
		expect(anyOf: terminators, consuming: .semicolon)
		
		return ast
	}
	
	// ----------------------------------
	internal final func parseAssignment(
		startingWith variableName: Token,
		assignmentIndicator: Token,
		terminatedBy terminators: [TokenType] = []) -> ASTNode?
	{
		assert(variableName.symbol == .identifier)
		assert(assignmentIndicator.symbol == .becomes)
		
		let startOfExpression = lexer.nextToken()
		if let rvalue = parseExpression(
			terminatedBy: terminators + [.semicolon])
		{
			return ASTNode(
				assignment: assignmentIndicator,
				lvalue: ASTNode(token: variableName),
				rvalue: rvalue
			)
		}
		else { lexer.mark(expected: "expression", got: startOfExpression)}
		
		return nil
	}
	
	// ----------------------------------
	internal final func parseProcedureCallStatement(
		startingWith procedureName: Token,
		callIndicator: Token) -> ASTNode?
	{
		assert(procedureName.symbol == .identifier)
		assert(
			callIndicator.symbol == .openParen
			|| callIndicator.symbol == .semicolon
		)
		
		var procCallAST: ASTNode? = nil
		if callIndicator.symbol == .semicolon
		{
			lexer.advance()
			procCallAST = ASTNode(function: procedureName, parameters: [])
		}
		else if callIndicator.symbol == .openParen
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
					lexer.mark(
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
	func makeNode(
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
				to: ASTNode(token: token)
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
			
			let node = makeNode(from: &operators, and: &operands)
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
					lexer.mark(expected: .closeParen, got: stackTop)
					errorEmitted = true
				}
				continue
			}
			
			let node = makeNode(from: &operators, and: &operands)
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
	being used by a calculated to generate a number.  Everytime an operator is combined with its operands,
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
	the same precedence level, they will be groupled from left to right.  For example `1 + 2 + 3` is parsed
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
	r-value.  C and C++ are unusual in allowing making the assignment operator also an l-value.  This little
	aside isn't important to the Shunting Yard algorithm, except that C's assignment operator makes a really
	good example of right-associativity, and it isn't possible in most other languages.

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
	2) if an open parenthesis is found at the top of operator stack, or
	3) if the operator represented by the current token has higher precedence than the one at the top of
		the stack, which includes if they are the same precendence level, but the one at the top of the
		stack is not left-associative.
	
	Then after this inner loop terminates, the token is pushed on the operator stack.
	
	If it is an open parenthesis, it is pushed onto the stack
	
	If it is a close parenthesis, the operators are popped off of the operator stack, and their operands off of
	the operand stack, combined to form an `ASTNode`, which is then pushed back on to the operand stack,
	until an open parenthesis is reached, which is popped off and discarded as the entire parenthetical
	expression has now been parsed and it's AST is at the top of the operand stack.
	
	The token reading loop continues until all tokens have been read (or a terminating symbol is reached
	like "`;`: marking the end of a statement.
	
	Once all tokens have been read, whatever operators remain on the operator stack are popped off, along
	with their parameters from the operand stack, combined to form a new `ASTNode`, which is pushed back
	onto the operand stack. Wash. Rinse. Repeat until all operators have been popped from the operator
	stack, leaving only one `ASTNode` on the operand stack, and that node is your AST for the expression.
	
	- Note:
	This implementation makes a few adjustments to the classic algorithm to support function calls and unary
	operators, and to handle parenthetical expression differenty.
	
	Parenthetical expressions are handled by a recursive call to `parseExpression(terminatedBy:)`,
	specifying a close parenthesis as the terminator.
	
	Function calls are suppored by parsing them separately, generating an AST for the call, and then pushing
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
		
		var terminatorFound = false
		while let token = lexer.peekToken()
		{
			if terminators.contains(token.symbol)
			{
				terminatorFound = true
				break
			}
			lexer.advance()
			
			switch token.symbol
			{
				case .identifier:
					if parseFunctionCall(token, &operators, &operands) {
						break
					}
					fallthrough // variable
					
				case .number: // constant
					parseVariableOrConstExpression(token, &operators, &operands)
										
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
					lexer.advance()
				case .closeParen:
					assertionFailure(
						"Unexpected \")\".  Should be handled by nested call "
						+ "to parseExpression()"
					)
					continue
				
				default: switch token.operatorGroup
				{
					case .prefixUnary: operators.push(token)
					case .binary, .postfixUnary:
						parseInfixPostfixExpr(token, &operators, &operands)
					default: break
				}
			}
		}
		
		if !terminatorFound && terminators.count > 0 {
			lexer.mark(expectedOneOf: terminators)
		}
		
		processRemainingStackedOperators(&operators, &operands)
		
		assert(operands.count == 1)
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
			lexer.mark(expected: .semicolon, got: actualSymbol)
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
}

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
class NewParser
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
	private func precedence(
		of operator1: Token,
		isHigherThan operator2: Token) -> Bool
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
	- Returns: `true` if `functionName` is parsed as a function call, and `false` otherwise.
	*/
	private func parseFunctionCall(
		_ functionName: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack) -> Bool
	{
		if lexer.peekToken()?.symbol == .openParen {
			lexer.mark("Parsing function call is not yet supported")
		}
		return false
	}
	
	// ----------------------------------
	private func parseVariableOrConstExpression(
		_ token: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		if operators.top?.operatorGroup == .prefixUnary
		{
			operands.push(
				ASTNode(
					token: operators.pop()!,
					child: ASTNode(token: token)
				)
			)
		}
		else { operands.push(ASTNode(token: token)) }
	}
	
	// ----------------------------------
	private func parseInfixPostfixExpr(
		_ token: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		while let stackTop = operators.top, stackTop.symbol != .openParen
		{
			guard precedence(of: stackTop, isHigherThan: token) else {
				break
			}
			
			let node = makeNode(from: &operators, and: &operands)
			operands.push(node)
		}
		
		operators.push(token)
	}
	
	// ----------------------------------
	private func parseCloseParenExpression(
		_ token: Token,
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		while let stackTop = operators.top, stackTop.symbol != .openParen
		{
			let node = makeNode(from: &operators, and: &operands)
			operands.push(node)
		}
		
		if let stackTop = operators.top, stackTop.symbol == .openParen {
			let _ = operators.pop()
		}
		else { lexer.mark("Too many close parentheses") }
		
		if operators.top?.operatorGroup == .prefixUnary
		{
			assert(operands.count > 0)
			let opNode = ASTNode(
				token: operators.pop()!,
				child: operands.pop()!
			)
			operands.push(opNode)
		}
	}
	
	// ----------------------------------
	private func processRemainingStackedOperators(
		_ operators: inout OperatorStack,
		_ operands: inout OperandStack)
	{
		while let stackTop = operators.top
		{
			if stackTop.symbol == .openParen {
				lexer.mark("Missing close parenthesis")
			}
			
			let node = makeNode(from: &operators, and: &operands)
			operands.push(node)
		}
	}
	
	// ----------------------------------
	/**
	Parse an infix expression using Edgar Djikstra's Shunting Yard algorithm.
	
	The name, Shunting Yard, refers to a shunting yard or rail yard for trains, because you can imagine it
	operates the way a rail yard would use a T-junction (or maybe it's more approciate to call it a Y-junction) in
	a railroad to move cars around to put them in the order they want.  One leg of the T represents the output,
	one leg represents an operator stack, and the remaining leg represents an operand stack.
	
	This algorithm can be a little hard to follow, and without extracting subparts into separate functions, it can
	be kind of long.  I hope doing that has made it easier to follow, but it's arguable that seeing all the moving
	parts together would be clearer despite the length.  It's hard to say which is better in this particular case.
	
	To understand it you need to understand two concepts about the ordering of operators in infix expressions.
	
	The first is the idea of precedence.  This just the idea that some operations should be performed on their
	adjacent operands before others even though they come later in the expression (reading left to right).   If
	for example, you are parsing `1 + 2 * 3`, multiplication has higher precedence than addition, so it's
	parsed as `1 + (2 * 3)`.  If you remember "My Dear Aunt Sally" from grade school, you've basically
	got the concept.
	
	The second idea is that of left- vs right-associativity.  This serves as a kind of tie-breaker when operators
	have the same precedence.  Most operators can be taken as being  left associative, which means given
	the same precedence level, they will be groupled from left to right.  For example `1 + 2 + 3` is parsed
	as `(1 + 2) + 3`.  Technically addition and multiplication can have either right- or left-associativity,
	while subtraction and division have strictly left-associativity.  Assignment operators tend to have
	right-associativity, though they do not make proper r-values in Oberon0, because an assignment
	itself does not have a value (though it assigns a value to a variable).   To use an example from C, which
	can use assignment as an r-value,`a = b = c` is parsed as `a = (b = c)`, which assigns `c` to
	`b` and then the expression `(b = c)` takes on the new value of `b` which is `c` and that value is
	assigned to `a`.
	
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
	arguments off of the operand stack, constructs an AST node out of them, and pushes that node  onto the
	operand stack.  The termination of the loop seems like a long list of conditions, but actually they're pretty
	simple.  The loop terminates if
	
	1) the operator stack has been exhausted, or
	2) if an open parenthesis is found at the top of operator stack, or
	3) if the operator represented by the current token has higher precedence than the one at the top of
		the stack, which includes if they are the same precendence level, but the one at the top of the
		stack is not left-associative.
	
	Then after this inner loop terminates, the token is pushed on the operator stack.
	
	If it is an open parenthesis, it is pushed onto the stack
	
	If it is a close parenthesis, the operators are popped off of the operator stack, and their operands off of
	the operand stack, combined to form an AST ASTNode, which is then pushed back on to the operand stack,
	until an open parenthesis is reached, which is popped off and discarded as the entire parenthetical
	expression has now been parsed and it's AST is at the top of the operand stack.
	
	The token reading loop continues until all tokens have been read (or a terminating symbol is reached
	like "`;`: marking the end of a statement.
	
	Once all tokens have been read, whatever operators remainon the operator stack are popped off, along
	with their parameters from the operand stack, combined to form a new AST node, which is pushed back
	onto the operand stack. Wash. Rinse. Repeat until all operators have been popped from the operator
	stack, leaving only on AST ASTNode on the operand stack, and that node is your AST for the expression.
	
	- Note:
	This implementation makes a few adjustments to the classic algorithm to support function calls and unary
	operators.
	
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
	popped off of the operator stack, combined with the token to form an AST node, which is then pushed
	onto the operand stack.
	
	- Returns: An Abstract Syntax Tree ASTNode representing the parsed expression, or `nil` if the parse
		reached end of input without obtaining tokens to generate the node.
	*/
	func parseExpression() -> ASTNode?
	{
		var operators = OperatorStack()
		var operands = OperandStack()
		
		while let token = lexer.nextToken(), token.symbol != .semicolon
		{
			switch token.symbol
			{
				case .identifier:
					if parseFunctionCall(token, &operators, &operands) {
						break
					}
					fallthrough // variable
					
				case .number: // constant
					parseVariableOrConstExpression(token, &operators, &operands)
										
				case .openParen: operators.push(token)
				case .closeParen:
					parseCloseParenExpression(token, &operators, &operands)
				
				default: switch token.operatorGroup
				{
					case .prefixUnary: operators.push(token)
					case .binary, .postfixUnary:
						parseInfixPostfixExpr(token, &operators, &operands)
					default: break
				}
			}
		}
		
		processRemainingStackedOperators(&operators, &operands)
		
		assert(operands.count == 1)
		return operands.top
	}
}

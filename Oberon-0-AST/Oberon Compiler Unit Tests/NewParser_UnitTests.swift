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

import XCTest

// ----------------------------------
class NewParser_UnitTests: XCTestCase 
{
	// ----------------------------------
    override func setUp() 
    {
        super.setUp()
 		continueAfterFailure = false
    }
    
	// ----------------------------------
    override func tearDown() {
        super.tearDown()
    }
	
	// ----------------------------------
	func parse(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parse() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
			
	// ----------------------------------
	func parseExpression(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseExpression() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseStatement(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseStatement() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseVariableDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseVariableDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseConstantDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseConstantDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseTypeDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseTypeDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseBeginEndBlock(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression)
			.parseCodeBlock(startingWith: .begin, terminatedBy: [.end])
		{
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}

			
	// ----------------------------------
    func test_basic_expression_gives_correct_kind_value_and_child_count_for_nodes()
	{
		guard let sumNode = parseExpression("2 * 3 + a DIV b") else { return }
		
		XCTAssertEqual(sumNode.kind, .binaryOperator)
		XCTAssertEqual(sumNode.srcStr, "+")
		XCTAssertEqual(sumNode.children.count, 2)
		
		let multiplyNode = sumNode.children[0]
		XCTAssertEqual(multiplyNode.kind, .binaryOperator)
		XCTAssertEqual(multiplyNode.srcStr, "*")
		XCTAssertEqual(multiplyNode.children.count, 2)
		
		let twoNode = multiplyNode.children[0]
		XCTAssertEqual(twoNode.kind, .constant)
		XCTAssertEqual(twoNode.srcStr, "2")
		XCTAssertEqual(twoNode.children.count, 0)
		
		let threeNode = multiplyNode.children[1]
		XCTAssertEqual(threeNode.kind, .constant)
		XCTAssertEqual(threeNode.srcStr, "3")
		XCTAssertEqual(threeNode.children.count, 0)
		
		let divisionNode = sumNode.children[1]
		XCTAssertEqual(divisionNode.kind, .binaryOperator)
		XCTAssertEqual(divisionNode.srcStr, "DIV")
		XCTAssertEqual(divisionNode.children.count, 2)
		
		let aNode = divisionNode.children[0]
		XCTAssertEqual(aNode.kind, .variable)
		XCTAssertEqual(aNode.srcStr, "a")
		XCTAssertEqual(aNode.children.count, 0)
		
		let bNode = divisionNode.children[1]
		XCTAssertEqual(bNode.kind, .variable)
		XCTAssertEqual(bNode.srcStr, "b")
		XCTAssertEqual(bNode.children.count, 0)
    }
	
	// ----------------------------------
    func test_print_of_basic_expression()
	{
		guard let sumNode = parseExpression("2 * 3 + a DIV b") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((2 * 3) + (a DIV b))")
    }
	
	// ----------------------------------
    func test_print_multiplication_in_middle_has_higher_precedence_than_addition()
	{
		guard let sumNode = parseExpression("2 + 3 * a - b") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((2 + (3 * a)) - b)")
    }
	
	// ----------------------------------
    func test_print_uniary_not_has_higher_precedence_than_AND()
	{
		guard let sumNode = parseExpression("2 OR ~3 & a OR b") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((2 OR (~(3) & a)) OR b)")
    }
	
	// ----------------------------------
    func test_print_uniary_not_can_be_applied_to_parenthetical_expression()
	{
		guard let sumNode = parseExpression("2 OR ~(3 & a) OR b") else {
			return
		}
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((2 OR ~((3 & a))) OR b)")
    }
	
	// ----------------------------------
	func test_parses_function_call_with_no_parameters()
	{
		guard let ast = parseExpression("foo()") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo()")
	}
	
	// ----------------------------------
	func test_parses_function_call_with_one_parameters()
	{
		guard let ast = parseExpression("foo(bar)") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo(bar)")
	}
	
	// ----------------------------------
	func test_parses_function_call_with_two_parameters()
	{
		guard let ast = parseExpression("foo(bar, baz)") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo(bar, baz)")
	}
	
	// ----------------------------------
	func test_parses_function_call_with_complex_expression_parameter()
	{
		guard let ast = parseExpression("foo(2 OR ~(3 & a) OR b)") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo(((2 OR ~((3 & a))) OR b))")
	}
	
	// ----------------------------------
	func test_parses_expression_involving_function_call_as_operands()
	{
		guard let ast = parseExpression("2 OR ~(foo(bar) & a) OR b") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "((2 OR ~((foo(bar) & a))) OR b)")
	}
	
	// ----------------------------------
	func test_parses_simple_assignment_statement()
	{
		guard let ast = parseStatement("a := b;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "a = b")
	}
	
	// ----------------------------------
	func test_parses_assignment_from_expression()
	{
		guard let ast = parseStatement("a := b * c;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "a = (b * c)")
	}
	
	// ----------------------------------
	func test_parses_assignment_from_function_call()
	{
		guard let ast = parseStatement("a := foo(bar);") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "a = foo(bar)")
	}
	
	// ----------------------------------
	func test_parses_single_variable_declaration()
	{
		guard let ast = parseVariableDeclaration("x: INTEGER;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: INTEGER")
	}
	
	// ----------------------------------
	func test_parses_multiple_variable_declaration_of_one_type()
	{
		guard let ast = parseVariableDeclaration("x, y: INTEGER;") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: INTEGER, y: INTEGER")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_array_of_literal_size_and_simple_element_type()
	{
		guard let ast = parseVariableDeclaration("x: ARRAY 5 OF INTEGER;") else
		{
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: ARRAY 5 OF INTEGER")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_array_of_expression_size_and_simple_element_type()
	{
		guard let ast = parseVariableDeclaration("x: ARRAY 5 * a OF INTEGER;") else
		{
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: ARRAY (5 * a) OF INTEGER")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_array_of_array()
	{
		guard let ast = parseVariableDeclaration(
			"x: ARRAY 5 OF ARRAY 20 OF INTEGER;")
		else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: ARRAY 5 OF ARRAY 20 OF INTEGER")
	}

	// ----------------------------------
	func test_parses_constant_declaration()
	{
		guard let ast = parseConstantDeclaration("x = 5;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x is 5")
	}

	// ----------------------------------
	func test_parses_simple_type_declaration()
	{
		guard let ast = parseTypeDeclaration("x = SomeType;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x is SomeType")
	}

	// ----------------------------------
	func test_parses_empty_code_block()
	{
		guard let ast = parseBeginEndBlock("BEGIN END") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "{}")
	}
	
	// ----------------------------------
	func test_parses_code_block_with_statements_without_final_semicolon_before_END()
	{
		let code =
		"""
		BEGIN
			doSomething;
			doSomethingElse(withParameters);
			a := foo(bar) * b;
			c := a - 2
		END
		"""
		guard let ast = parseBeginEndBlock(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "{doSomething(), doSomethingElse(withParameters), a = (foo(bar) * b), c = (a - 2)}")
	}
	
	// ----------------------------------
	func test_parses_code_block_with_statements_with_final_semicolon_before_END()
	{
		let code =
		"""
		BEGIN
			doSomething;
			doSomethingElse(withParameters);
			a := foo(bar) * b;
			c := a - 2;
		END
		"""
		guard let ast = parseBeginEndBlock(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "{doSomething(), doSomethingElse(withParameters), a = (foo(bar) * b), c = (a - 2)}")
	}
}

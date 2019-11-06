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
		guard let sumNode = parseExpression("2 OR ~(3 & a) OR b") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((2 OR ~((3 & a))) OR b)")
    }
}

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
class ParsingExpression_UnitTests: XCTestCase 
{
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
	func test_print_of_constant_expression()
	{
		guard let sumNode = parseExpression("5") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "5")
	}
	
	// ----------------------------------
	func test_print_of_variable_expression()
	{
		guard let sumNode = parseExpression("a") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "a")
	}
	
	// ----------------------------------
	func test_print_of_record_field_expression()
	{
		guard let sumNode = parseExpression("a.b") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a.b)")
	}
	
	// ----------------------------------
	func test_print_of_array_element_expression_with_constant_index()
	{
		guard let sumNode = parseExpression("a[5]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a[5])")
	}
	
	// ----------------------------------
	func test_print_of_array_element_expression_with_variable_index()
	{
		guard let sumNode = parseExpression("a[i]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a[i])")
	}
	
	// ----------------------------------
	func test_print_of_array_element_expression_with_record_field_index()
	{
		guard let sumNode = parseExpression("a[b.c]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a[(b.c)])")
	}
	
	// ----------------------------------
	func test_print_of_array_element_expression_with_array_element_index()
	{
		guard let sumNode = parseExpression("a[b[c]]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a[(b[c])])")
	}
	
	// ----------------------------------
	func test_print_of_record_field_array_expression()
	{
		guard let sumNode = parseExpression("a.b[i]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((a.b)[i])")
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
	func test_parses_func_call_with_array_element_parameter()
	{
		guard let ast = parseExpression("foo(a[i])") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo((a[i]))")
	}
	
	// ----------------------------------
	func test_parses_func_call_with_record_field_parameter()
	{
		guard let ast = parseExpression("foo(a.b)") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo((a.b))")
	}

	// ----------------------------------
	func test_parses_nested_function_calls()
	{
		guard let ast = parseExpression("foo(bar(b))") else {
			return
		}
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "foo(bar(b))")
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
	func test_print_of_array_element_expression_with_expression_index()
	{
		guard let sumNode = parseExpression("a[i * stride]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "(a[(i * stride)])")
	}
	
	// ----------------------------------
	func test_print_of_expression_with_array_element_operands()
	{
		guard let sumNode = parseExpression("a[i] * b[j]") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((a[i]) * (b[j]))")
	}
	
	// ----------------------------------
	func test_print_of_expression_with_record_field_operands()
	{
		guard let sumNode = parseExpression("a.x * b.y") else { return }
		
		let result = "\(sumNode)"
		
		XCTAssertEqual(result, "((a.x) * (b.y))")
	}
}

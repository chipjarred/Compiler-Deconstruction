// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
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
class ParsingAssignment_UnitTests: XCTestCase 
{
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
	func test_parses_assignment_to_array_element()
	{
		guard let ast = parseStatement("a[5] := 2;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "(a[5]) = 2")
	}
	
	// ----------------------------------
	func test_parses_assignment_to_record_field()
	{
		guard let ast = parseStatement("a.b := 2;") else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "(a.b) = 2")
	}
}

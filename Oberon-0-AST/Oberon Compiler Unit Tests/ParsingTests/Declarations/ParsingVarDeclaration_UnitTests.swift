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
class ParsingVarDeclaration_UnitTests: XCTestCase 
{
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
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: ARRAY 5 OF ARRAY 20 OF INTEGER")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_empty_record()
	{
		guard let ast = parseVariableDeclaration(
			"x: RECORD END;")
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: RECORD{}")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_record_with_one_field()
	{
		guard let ast = parseVariableDeclaration(
			"x: RECORD field1: INTEGER END;")
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: RECORD{field1: INTEGER}")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_record_with_two_fields_of_same_type()
	{
		guard let ast = parseVariableDeclaration(
			"x: RECORD field1, field2: INTEGER END;")
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: RECORD{field1: INTEGER, field2: INTEGER}")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_record_with_two_fields_of_different_types()
	{
		guard let ast = parseVariableDeclaration(
			"x: RECORD field1: INTEGER; field2: ARRAY 5 OF INTEGER END;")
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: RECORD{field1: INTEGER, field2: ARRAY 5 OF INTEGER}")
	}
	
	// ----------------------------------
	func test_parses_variable_declaration_for_nested_records()
	{
		guard let ast = parseVariableDeclaration(
			"x: RECORD field1: INTEGER; field2: RECORD a,b: INTEGER END END;")
		else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "x: RECORD{field1: INTEGER, field2: RECORD{a: INTEGER, b: INTEGER}}")
	}
}

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
class TypeCheck_Statements_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_integer_assignment_from_literal()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
		BEGIN
			x := 42
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.value, 42)
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_integer_assignment_from_variable()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: INTEGER
		BEGIN
			y := 42;
			x := y
		END TEST.
		"""
		
		guard let node = assignment(skip: 1, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.name, "y")
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_integer_assignment_from_expression()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: INTEGER
		BEGIN
			y := 42;
			x := y + 5
		END TEST.
		"""
		
		guard let node = assignment(skip: 1, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.symbol, .plus)
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_integer_assignment_from_array_element()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: ARRAY 5 OF INTEGER
		BEGIN
			x := y[0]
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.kind, .arrayElement)
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}

	// ----------------------------------
	func test_integer_assignment_to_array_element()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: ARRAY 5 OF INTEGER
		BEGIN
			y[0] := x
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.kind, .arrayElement)
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.name, "x")
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}

	// ----------------------------------
	func test_integer_assignment_from_record_field()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: RECORD a, b: INTEGER END
		BEGIN
			x := y.a
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.kind, .recordField)
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}

	// ----------------------------------
	func test_integer_assignment_to_record_field()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: RECORD a, b: INTEGER END
		BEGIN
			y.a := x
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.kind, .recordField)
		XCTAssertEqual(left.typeInfo, TypeInfo.integer)
		XCTAssertEqual(right.name, "x")
		XCTAssertEqual(right.typeInfo, TypeInfo.integer)
	}

	// ----------------------------------
	func test_boolean_assignment_from_literal()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: BOOLEAN;
		BEGIN
			x := TRUE
		END TEST.
		"""
		
		guard let node = assignment(skip: 0, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(right.value, 1)
		XCTAssertEqual(right.typeInfo, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_boolean_assignment_from_variable()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: BOOLEAN;
			y: BOOLEAN
		BEGIN
			y := TRUE;
			x := y
		END TEST.
		"""
		
		guard let node = assignment(skip: 1, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(right.name, "y")
		XCTAssertEqual(right.typeInfo, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_boolean_assignment_from_expression()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: BOOLEAN;
			y: INTEGER;
			z: INTEGER;
		BEGIN
			y := 5;
			z := 10;
			x := (y = (z DIV 2))
		END TEST.
		"""
		
		guard let node = assignment(skip: 2, in: code)
		else { return }
		
		let left = node.children[0]
		let right = node.children[1]
		
		XCTAssertEqual(node.typeInfo, TypeInfo.void)
		
		XCTAssertEqual(left.name, "x")
		XCTAssertEqual(left.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(right.symbol, .isEqualTo)
		XCTAssertEqual(right.typeInfo, TypeInfo.boolean)
	}

}

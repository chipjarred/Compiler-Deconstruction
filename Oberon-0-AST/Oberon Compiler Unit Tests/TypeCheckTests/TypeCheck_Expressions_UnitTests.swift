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

// ---------------------------------------------------
class TypeCheck_Expressions_UnitTests: XCTestCase
{
	// ---------------------------------------------------
	func test_constant_plus_operator()
	{
		let code =
		"""
		MODULE Test;
		CONST value = 5 + 7;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "value", in: code) else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 12)
	}
	
	// ---------------------------------------------------
	func test_constant_minus_operator()
	{
		let code =
		"""
		MODULE Test;
		CONST value = 5 - 7;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "value", in: code) else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, -2)
	}
	
	// ---------------------------------------------------
	func test_constant_multiplication_operator()
	{
		let code =
		"""
		MODULE Test;
		CONST value = 5 * 7;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "value", in: code) else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 35)
	}
	
	// ---------------------------------------------------
	func test_constant_division_operator()
	{
		let code =
		"""
		MODULE Test;
		CONST value = 35 DIV 7;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "value", in: code) else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 5)
	}
	
	// ---------------------------------------------------
	func test_constant_modulus_operator()
	{
		let code =
		"""
		MODULE Test;
		CONST value = 135 MOD 100;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "value", in: code) else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 35)
	}
	
	// ---------------------------------------------------
	func test_precedence_in_mixed_operations()
	{
		let code =
		"""
		MODULE Test;
		CONST
			x1 = 2 + 3 * 8;
			x2 = (2 + 3) * 8;
			x3 = 8 * 3 + 2;
			x4 = 8 * (2 + 3);
		BEGIN
		END TEST.
		"""
		
		if let node = constant(named: "x1", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.integer)
			XCTAssertEqual(node.value, 26)
		}
		
		if let node = constant(named: "x2", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.integer)
			XCTAssertEqual(node.value, 40)
		}
		
		if let node = constant(named: "x3", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.integer)
			XCTAssertEqual(node.value, 26)
		}
		
		if let node = constant(named: "x4", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.integer)
			XCTAssertEqual(node.value, 40)
		}
	}
	
	// ---------------------------------------------------
	func test_constant_not_expression()
	{
		let code =
		"""
		MODULE Test;
		CONST
			x1 = ~FALSE;
			x2 = ~TRUE;
		BEGIN
		END TEST.
		"""
		
		if let node = constant(named: "x1", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 1)
		}
		
		if let node = constant(named: "x2", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 0)
		}
	}
	
	// ---------------------------------------------------
	func test_constant_and_expression()
	{
		let code =
		"""
		MODULE Test;
		CONST
			x1 = FALSE & FALSE;
			x2 = FALSE & TRUE;
			x3 = TRUE & FALSE;
			x4 = TRUE & TRUE;
		BEGIN
		END TEST.
		"""
		
		if let node = constant(named: "x1", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 0)
		}
		
		if let node = constant(named: "x2", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 0)
		}
		
		if let node = constant(named: "x3", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 0)
		}
		
		if let node = constant(named: "x4", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 1)
		}
	}
	
	// ---------------------------------------------------
	func test_constant_or_expression()
	{
		let code =
		"""
		MODULE Test;
		CONST
			x1 = FALSE OR FALSE;
			x2 = FALSE OR TRUE;
			x3 = TRUE OR FALSE;
			x4 = TRUE OR TRUE;
		BEGIN
		END TEST.
		"""
		
		if let node = constant(named: "x1", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 0)
		}
		
		if let node = constant(named: "x2", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 1)
		}
		
		if let node = constant(named: "x3", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 1)
		}
		
		if let node = constant(named: "x4", in: code)
		{
			XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
			XCTAssertEqual(node.value, 1)
		}
	}
}

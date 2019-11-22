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
class ParsingModule_UnitTests: XCTestCase 
{
	// ----------------------------------
	func test_parses_trivial_module_declaration()
	{
		let code =
		"""
		MODULE foo;
		BEGIN
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{}, VAR{}, [], {}}")
	}
	
	// ----------------------------------
	func test_parses_module_declaration_with_const_section()
	{
		let code =
		"""
		MODULE foo;
		CONST x = 5; y = 10
		BEGIN
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{x is 5, y is 10}, TYPE{}, VAR{}, [], {}}")
	}
	
	// ----------------------------------
	func test_parses_module_declaration_with_type_section()
	{
		let code =
		"""
		MODULE foo;
		TYPE myArray = ARRAY 5 OF INTEGER; myRecord = RECORD x, y: INTEGER END
		BEGIN
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{myArray is ARRAY 5 OF INTEGER, myRecord is RECORD{x: INTEGER, y: INTEGER}}, VAR{}, [], {}}")
	}
	
	
	// ----------------------------------
	func test_parses_module_declaration_with_var_section()
	{
		let code =
		"""
		MODULE foo;
		VAR x,y: INTEGER
		BEGIN
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{}, VAR{x: INTEGER, y: INTEGER}, [], {}}")
	}
	
	// ----------------------------------
	func test_parses_module_declaration_with_procedure()
	{
		let code =
		"""
		MODULE foo;
			PROCEDURE swap(VAR x, y: INTEGER);
			VAR temp: INTEGER
			BEGIN
				temp := x;
				x := y;
				y := temp;
			END swap;
		BEGIN
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{}, VAR{}, [PROCEDURE{swap, CONST{}, TYPE{}, VAR{temp: INTEGER}, [], {temp = x, x = y, y = temp}, [ref(x: INTEGER), ref(y: INTEGER)]}], {}}")
	}
	
	// ----------------------------------
	func test_parses_module_declaration_with_body()
	{
		let code =
		"""
		MODULE foo;
		VAR temp: INTEGER
		BEGIN
			temp := x;
			x := y;
			y := temp;
		END foo;
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{}, VAR{temp: INTEGER}, [], {temp = x, x = y, y = temp}}")
	}
	
	// ----------------------------------
	func test_parses_module_declaration_with_var_and_procedure()
	{
		let code =
		"""
		MODULE foo;
		VAR temp: INTEGER
			PROCEDURE P;
			BEGIN
				temp := x;
				x := y;
				y := temp;
			END P;
		BEGIN
		END foo.
		"""
		guard let ast = parseModuleDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "MODULE{foo, CONST{}, TYPE{}, VAR{temp: INTEGER}, [PROCEDURE{P, CONST{}, TYPE{}, VAR{}, [], {temp = x, x = y, y = temp}, []}], {}}")
	}

}

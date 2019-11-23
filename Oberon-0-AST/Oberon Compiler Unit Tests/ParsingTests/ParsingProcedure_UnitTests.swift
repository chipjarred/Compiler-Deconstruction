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
class ParsingProcedure_UnitTests: XCTestCase 
{
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_no_parameters()
	{
		let code =
		"""
		PROCEDURE foo;
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, []}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_one_value_parameter()
	{
		let code =
		"""
		PROCEDURE foo(x:INTEGER);
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, [val(x: INTEGER)]}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_two_value_parameters_of_the_same_kind()
	{
		let code =
		"""
		PROCEDURE foo(x,y:INTEGER);
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, [val(x: INTEGER), val(y: INTEGER)]}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_one_reference_parameter()
	{
		let code =
		"""
		PROCEDURE foo(VAR x:INTEGER);
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, [ref(x: INTEGER)]}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_two_reference_parameters_of_the_same_kind()
	{
		let code =
		"""
		PROCEDURE foo(VAR x,y:INTEGER);
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, [ref(x: INTEGER), ref(y: INTEGER)]}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_mixed_parameters()
	{
		let code =
		"""
		PROCEDURE foo(x,y:INTEGER; VAR z: ARRAY 5 OF INTEGER);
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {}, [val(x: INTEGER), val(y: INTEGER), ref(z: ARRAY 5 OF INTEGER)]}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_no_parameters_but_local_variables()
	{
		let code =
		"""
		PROCEDURE foo;
		VAR x,y: INTEGER
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{x: INTEGER, y: INTEGER}, [], {}, []}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_no_parameters_but_has_const_section()
	{
		let code =
		"""
		PROCEDURE foo;
		CONST x = 5; y = 10
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{x is 5, y is 10}, TYPE{}, VAR{}, [], {}, []}")
	}
	
	// ----------------------------------
	func test_parses_simple_procedure_declaration_with_no_parameters_but_has_type_section()
	{
		let code =
		"""
		PROCEDURE foo;
		TYPE myArray = ARRAY 5 OF INTEGER; myRecord = RECORD x, y: INTEGER END
		BEGIN
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{myArray is ARRAY 5 OF INTEGER, myRecord is RECORD{x: INTEGER, y: INTEGER}}, VAR{}, [], {}, []}")
	}
	
	// ----------------------------------
	func test_parses_procedure_declaration_with_body_statements()
	{
		let code =
		"""
		PROCEDURE foo;
		BEGIN
			x := 5;
			swap(x, y)
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{}, [], {x = 5, swap(x, y)}, []}")
	}
	
	
	// ----------------------------------
	func test_parses_procedure_declaration_with_nested_procedure()
	{
		let code =
		"""
		PROCEDURE foo;
		VAR x, y: INTEGER;
			PROCEDURE swap(VAR x, y: INTEGER);
			VAR temp: INTEGER;
			BEGIN
				temp := x;
				x := y;
				y := temp
			END swap;
		BEGIN
			x := 5;
			swap(x, y)
		END foo;
		"""
		guard let ast = parseProcedureDeclaration(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "PROCEDURE{foo, CONST{}, TYPE{}, VAR{x: INTEGER, y: INTEGER}, [PROCEDURE{swap, CONST{}, TYPE{}, VAR{temp: INTEGER}, [], {temp = x, x = y, y = temp}, [ref(x: INTEGER), ref(y: INTEGER)]}], {x = 5, swap(x, y)}, []}")
	}
}

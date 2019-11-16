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
class TypeCheck_Declarations_UnitTests: XCTestCase
{	
	// ----------------------------------
	func test_integer_constant_declaration_with_plain_integer_value()
	{
		let code =
		"""
		MODULE Test;
		CONST ultimateAnswer = 42;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "ultimateAnswer", in: code)
		else { return }
		
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 42)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_negative_value()
	{
		let code =
		"""
		MODULE Test;
		CONST x = -99;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, -99)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_unary_positive_value()
	{
		let code =
		"""
		MODULE Test;
		CONST numberOfProblems = +99;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "numberOfProblems", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 99)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_binary_expression_value()
	{
		let code =
		"""
		MODULE Test;
		CONST minutesPerDay = 24*60;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "minutesPerDay", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 1440)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_binary_expression_that_refers_to_another_constant()
	{
		let code =
		"""
		MODULE Test;
		CONST
			minutesPerDay = 24*60;
			secondsPerDay = minutesPerDay * 60;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "secondsPerDay", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.value, 86400)
	}
	
	// ----------------------------------
	func test_true_boolean_constant_declaration()
	{
		let code =
		"""
		MODULE Test;
		CONST squirrelsLikeNuts = TRUE;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "squirrelsLikeNuts", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.value, 1)
	}
	
	// ----------------------------------
	func test_false_boolean_constant_declaration()
	{
		let code =
		"""
		MODULE Test;
		CONST tigersLikeNuts = FALSE;
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "tigersLikeNuts", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.value, 0)
	}
	
	// ----------------------------------
	func test_boolean_expression_constant_declaration()
	{
		let code =
		"""
		MODULE Test;
		CONST
			highlanderWasAGoodMovie = TRUE;
			highlander2WasAGoodMovie = FALSE;
			allHighlanderMoviesWereGood = highlanderWasAGoodMovie & highlander2WasAGoodMovie
		BEGIN
		END TEST.
		"""
		
		guard let node = constant(named: "allHighlanderMoviesWereGood", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.value, 0)
	}
	
	// ----------------------------------
	func test_type_declaration_of_boolean()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = BOOLEAN;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_type_declaration_of_integer()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = INTEGER;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_type_declaration_of_array_of_integer()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = ARRAY 5 OF INTEGER;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .array)
		XCTAssertEqual(node.typeInfo.len, 5)
		XCTAssertEqual(node.typeInfo.size, 20)
		XCTAssertNotNil(node.typeInfo.base)
		XCTAssertEqual(node.typeInfo.base, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_type_declaration_of_array_of_array_of_integer()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = ARRAY 5 OF ARRAY 10 OF INTEGER;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .array)
		XCTAssertEqual(node.typeInfo.len, 5)
		XCTAssertEqual(node.typeInfo.size, 200)
		XCTAssertNotNil(node.typeInfo.base)
		XCTAssertEqual(node.typeInfo.base!.form, .array)
		XCTAssertEqual(node.typeInfo.base!.len, 10)
		XCTAssertEqual(node.typeInfo.base!.size, 40)
		XCTAssertNotNil(node.typeInfo.base!.base)
		XCTAssertEqual(node.typeInfo.base!.base, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_type_declaration_of_record_of_integer_and_bool()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = RECORD x: INTEGER; y: BOOLEAN END;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .record)
		XCTAssertEqual(node.typeInfo.size, 8)
		XCTAssertEqual(node.typeInfo.fields[0].name, "x")
		XCTAssertEqual(node.typeInfo.fields[0].type, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.fields[1].name, "y")
		XCTAssertEqual(node.typeInfo.fields[1].type, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_type_declaration_of_record_of_another_record()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			BaseRec = RECORD a: INTEGER; b: BOOLEAN END;
			MyType = RECORD x, y: BaseRec END;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .record)
		XCTAssertEqual(node.typeInfo.size, 16)
		XCTAssertEqual(node.typeInfo.fields[0].name, "x")
		XCTAssertEqual(node.typeInfo.fields[0].type!.form, .record)
		XCTAssertEqual(node.typeInfo.fields[0].type!.fields[0].name, "a")
		XCTAssertEqual(node.typeInfo.fields[0].type!.fields[0].type, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.fields[0].type!.fields[1].name, "b")
		XCTAssertEqual(node.typeInfo.fields[0].type!.fields[1].type, TypeInfo.boolean)
		XCTAssertEqual(node.typeInfo.fields[1].name, "y")
		XCTAssertEqual(node.typeInfo.fields[1].type!.form, .record)
		XCTAssertEqual(node.typeInfo.fields[1].type!.fields[0].name, "a")
		XCTAssertEqual(node.typeInfo.fields[1].type!.fields[0].type, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.fields[1].type!.fields[1].name, "b")
		XCTAssertEqual(node.typeInfo.fields[1].type!.fields[1].type, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_type_declaration_of_array_of_records()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			Base = RECORD x: INTEGER; y: BOOLEAN END;
			MyType = ARRAY 5 OF Base;
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .array)
		XCTAssertEqual(node.typeInfo.len, 5)
		XCTAssertEqual(node.typeInfo.size, 40)
		XCTAssertNotNil(node.typeInfo.base)
		XCTAssertEqual(node.typeInfo.base!.form, .record)
		XCTAssertEqual(node.typeInfo.base!.size, 8)
		XCTAssertEqual(node.typeInfo.base!.fields[0].name, "x")
		XCTAssertEqual(node.typeInfo.base!.fields[0].type, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.base!.fields[1].name, "y")
		XCTAssertEqual(node.typeInfo.base!.fields[1].type, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_type_declaration_of_record_with_array_field()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			Base = ARRAY 5 OF INTEGER;
			MyType = RECORD a: BOOLEAN; b: Base; c: BOOLEAN END
		BEGIN
		END TEST.
		"""
		
		guard let node = type(named: "MyType", in: code)
		else { return }

		XCTAssertEqual(node.typeInfo.form, .record)
		XCTAssertEqual(node.typeInfo.size, 28)
		XCTAssertEqual(node.typeInfo.fields[0].name, "a")
		XCTAssertEqual(node.typeInfo.fields[0].type, TypeInfo.boolean)
		XCTAssertEqual(node.typeInfo.fields[1].name, "b")
		XCTAssertEqual(node.typeInfo.fields[1].type!.form, .array)
		XCTAssertEqual(node.typeInfo.fields[1].type!.len, 5)
		XCTAssertEqual(node.typeInfo.fields[1].type!.size, 20)
		XCTAssertEqual(node.typeInfo.fields[1].type!.base, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.fields[2].name, "c")
		XCTAssertEqual(node.typeInfo.fields[2].type, TypeInfo.boolean)
	}
	
	// ----------------------------------
	func test_integer_variable_declaration()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: INTEGER;
		BEGIN
		END TEST.
		"""
		
		guard let node = variable(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.name, "x")
		XCTAssertEqual(node.typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.typeInfo.size, 4)
	}
	
	// ----------------------------------
	func test_boolean_variable_declaration()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: BOOLEAN;
		BEGIN
		END TEST.
		"""
		
		guard let node = variable(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.name, "x")
		XCTAssertEqual(node.typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.typeInfo.size, 4)
	}
	
	// ----------------------------------
	func test_array_variable_declaration()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: ARRAY 5 OF INTEGER;
		BEGIN
		END TEST.
		"""
		
		guard let node = variable(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.name, "x")
		XCTAssertEqual(node.typeInfo.form, .array)
		XCTAssertEqual(node.typeInfo.len, 5)
		XCTAssertEqual(node.typeInfo.size, 20)
		XCTAssertEqual(node.typeInfo.base, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_record_variable_declaration()
	{
		let code =
		"""
		MODULE Test;
		VAR
			x: RECORD a: BOOLEAN; b: INTEGER END;
		BEGIN
		END TEST.
		"""
		
		guard let node = variable(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.name, "x")
		XCTAssertEqual(node.typeInfo.form, .record)
		XCTAssertEqual(node.typeInfo.size, 8)
		XCTAssertEqual(node.typeInfo.fields[0].name, "a")
		XCTAssertEqual(node.typeInfo.fields[0].type, TypeInfo.boolean)
		XCTAssertEqual(node.typeInfo.fields[1].name, "b")
		XCTAssertEqual(node.typeInfo.fields[1].type, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_record_variable_declaration_of_previously_defined_user_type()
	{
		let code =
		"""
		MODULE Test;
		TYPE
			MyType = RECORD a: BOOLEAN; b: INTEGER END
		VAR
			x: MyType
		BEGIN
		END TEST.
		"""
		
		guard let node = variable(named: "x", in: code)
		else { return }

		XCTAssertEqual(node.name, "x")
		XCTAssertEqual(node.typeInfo.form, .record)
		XCTAssertEqual(node.typeInfo.size, 8)
		XCTAssertEqual(node.typeInfo.fields[0].name, "a")
		XCTAssertEqual(node.typeInfo.fields[0].type, TypeInfo.boolean)
		XCTAssertEqual(node.typeInfo.fields[1].name, "b")
		XCTAssertEqual(node.typeInfo.fields[1].type, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_procedure_declaration_with_no_parameters()
	{
		let code =
		"""
		MODULE Test;
			PROCEDURE doSomething;
			BEGIN
			END doSomething;
		BEGIN
		END TEST.
		"""
		
		guard let node = procedure(named: "doSomething", in: code)
		else { return }

		XCTAssertEqual(node.name, "doSomething")
		XCTAssertEqual(node.symbolInfo.kind, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.form, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.base, TypeInfo.void)
		XCTAssertEqual(node.symbolInfo.type!.fields.count, 0)
	}
	
	// ----------------------------------
	func test_procedure_declaration_with_one_value_parameter()
	{
		let code =
		"""
		MODULE Test;
			PROCEDURE foo(bar: INTEGER);
			BEGIN
			END foo;
		BEGIN
		END TEST.
		"""
		
		guard let node = procedure(named: "foo", in: code)
		else { return }

		XCTAssertEqual(node.name, "foo")
		XCTAssertEqual(node.symbolInfo.kind, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.form, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.base, TypeInfo.void)
		XCTAssertEqual(node.symbolInfo.type!.fields.count, 1)
		XCTAssertEqual(node.symbolInfo.type!.fields[0].name, "bar")
		XCTAssertEqual(node.symbolInfo.type!.fields[0].kind, .variable)
		XCTAssertEqual(node.symbolInfo.type!.fields[0].type, TypeInfo.integer)
	}
	
	// ----------------------------------
	func test_procedure_declaration_with_one_reference_parameter()
	{
		let code =
		"""
		MODULE Test;
			PROCEDURE foo(VAR bar: INTEGER);
			BEGIN
			END foo;
		BEGIN
		END TEST.
		"""
		
		guard let node = procedure(named: "foo", in: code)
		else { return }

		XCTAssertEqual(node.name, "foo")
		XCTAssertEqual(node.symbolInfo.kind, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.form, .procedure)
		XCTAssertEqual(node.symbolInfo.type!.base, TypeInfo.void)
		XCTAssertEqual(node.symbolInfo.type!.fields.count, 1)
		XCTAssertEqual(node.symbolInfo.type!.fields[0].name, "bar")
		XCTAssertEqual(node.symbolInfo.type!.fields[0].kind, .parameter)
		XCTAssertEqual(node.symbolInfo.type!.fields[0].type, TypeInfo.integer)
	}
}

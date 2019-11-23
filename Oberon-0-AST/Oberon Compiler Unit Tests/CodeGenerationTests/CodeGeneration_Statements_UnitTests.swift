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
class CodeGeneration_Statements_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_from_literal()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: INTEGER;
		BEGIN
			x := 5
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4092
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	POP 	 14, 13,    4
		 20	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_from_named_constant()
	{
		let source =
		###"""
		MODULE Test;
		CONST
			five = 5;
		VAR
			x: INTEGER;
		BEGIN
			x := five
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4092
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	POP 	 14, 13,    4
		 20	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_from_constant_expression()
	{
		let source =
		###"""
		MODULE Test;
		CONST
			five = 5;
		VAR
			x: INTEGER;
		BEGIN
			x := five * 2
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4092
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,   10
		 12	STW 	  0, 15,  -16
		 16	POP 	 14, 13,    4
		 20	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_from_variable()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x, y: INTEGER;
		BEGIN
			x := 5;
			y := x
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	LDW 	  0, 15,  -20
		 20	STW 	  0, 15,  -28
		 24	POP 	 14, 13,    4
		 28	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_to_array_element()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: ARRAY 5 OF INTEGER;
		BEGIN
			x[1] := 5;
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4076
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -28
		 16	POP 	 14, 13,    4
		 20	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_body_with_assignment_to_record_field()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: RECORD a, b: INTEGER END;
		BEGIN
			x.a := 5;
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -20
		 16	POP 	 14, 13,    4
		 20	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_local_variable_from_literal()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x: INTEGER;
			BEGIN
				x := 5
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   40
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOV 	 13,  0,   12
		 28	POP 	 12, 13,    4
		 32	POP 	 14, 13,    4
		 36	RET    14
		 40	MOVI	 13,  0, 4096
		 44	PSH 	 14, 13,    4
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_local_variable_from_local_variable()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x, y: INTEGER;
			BEGIN
				x := 5;
				y := x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   48
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    8
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	STW 	  0, 12,   -8
		 32	MOV 	 13,  0,   12
		 36	POP 	 12, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14
		 48	MOVI	 13,  0, 4096
		 52	PSH 	 14, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_local_variable_from_global_variable()
	{
		let source =
		###"""
		MODULE Test;
			VAR
				x: INTEGER;
			PROCEDURE P;
			VAR
				y: INTEGER;
			BEGIN
				x := 5;
				y := x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   48
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 15,  -24
		 24	LDW 	  0, 15,  -28
		 28	STW 	  0, 12,   -4
		 32	MOV 	 13,  0,   12
		 36	POP 	 12, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14
		 48	MOVI	 13,  0, 4092
		 52	PSH 	 14, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_global_variable_from_local_variable()
	{
		let source =
		###"""
		MODULE Test;
			VAR
				x: INTEGER;
			PROCEDURE P;
			VAR
				y: INTEGER;
			BEGIN
				y := 5;
				x := y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   48
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	STW 	  0, 15,  -32
		 32	MOV 	 13,  0,   12
		 36	POP 	 12, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14
		 48	MOVI	 13,  0, 4092
		 52	PSH 	 14, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_local_array_element()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x: ARRAY 5 OF INTEGER;
			BEGIN
				x[1] := 5;
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   40
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   20
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,  -16
		 24	MOV 	 13,  0,   12
		 28	POP 	 12, 13,    4
		 32	POP 	 14, 13,    4
		 36	RET    14
		 40	MOVI	 13,  0, 4096
		 44	PSH 	 14, 13,    4
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_local_record_field()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x: RECORD a, b: INTEGER END;
			BEGIN
				x.b := 5;
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   40
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    8
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOV 	 13,  0,   12
		 28	POP 	 12, 13,    4
		 32	POP 	 14, 13,    4
		 36	RET    14
		 40	MOVI	 13,  0, 4096
		 44	PSH 	 14, 13,    4
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_global_array_element()
	{
		let source =
		###"""
		MODULE Test;
			VAR
				x: ARRAY 5 OF INTEGER;
			PROCEDURE P;
			BEGIN
				x[1] := 5;
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   40
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 15,  -36
		 24	MOV 	 13,  0,   12
		 28	POP 	 12, 13,    4
		 32	POP 	 14, 13,    4
		 36	RET    14
		 40	MOVI	 13,  0, 4076
		 44	PSH 	 14, 13,    4
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_with_assignment_to_global_record_field()
	{
		let source =
		###"""
		MODULE Test;
			VAR
				x: RECORD a, b: INTEGER END;
			PROCEDURE P;
			BEGIN
				x.b := 5;
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   40
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 15,  -24
		 24	MOV 	 13,  0,   12
		 28	POP 	 12, 13,    4
		 32	POP 	 14, 13,    4
		 36	RET    14
		 40	MOVI	 13,  0, 4088
		 44	PSH 	 14, 13,    4
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
}

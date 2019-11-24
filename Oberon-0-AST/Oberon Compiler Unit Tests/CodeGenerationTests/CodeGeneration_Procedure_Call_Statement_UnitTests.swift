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
class CodeGeneration_Procedure_Call_Statement_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_no_parameters()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE foo;
			BEGIN
			END foo;
		BEGIN
			foo;
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    4
		 28	RET    14
		 32	MOVI	 13,  0, 4096
		 36	PSH 	 14, 13,    4
		 40	BSR   -10
		 44	POP 	 14, 13,    4
		 48	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_one_constant_parameter()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE foo(bar: INTEGER);
			BEGIN
			END foo;
		BEGIN
			foo(1);
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    8
		 28	RET    14
		 32	MOVI	 13,  0, 4096
		 36	PSH 	 14, 13,    4
		 40	MOVI	  0,  0,    1
		 44	PSH 	  0, 13,    4
		 48	BSR   -12
		 52	POP 	 14, 13,    4
		 56	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_variable_value_parameter()
	{
		let source =
		###"""
		MODULE Test;
		VAR x: INTEGER;
			PROCEDURE foo(bar: INTEGER);
			BEGIN
			END foo;
		BEGIN
			x := 5;
			foo(x);
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    8
		 28	RET    14
		 32	MOVI	 13,  0, 4092
		 36	PSH 	 14, 13,    4
		 40	MOVI	  0,  0,    5
		 44	STW 	  0, 15,  -48
		 48	LDW 	  0, 15,  -52
		 52	PSH 	  0, 13,    4
		 56	BSR   -14
		 60	POP 	 14, 13,    4
		 64	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_variable_reference_parameter()
	{
		let source =
		###"""
		MODULE Test;
		VAR x: INTEGER;
			PROCEDURE foo(VAR bar: INTEGER);
			BEGIN
			END foo;
		BEGIN
			x := 5;
			foo(x);
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    8
		 28	RET    14
		 32	MOVI	 13,  0, 4092
		 36	PSH 	 14, 13,    4
		 40	MOVI	  0,  0,    5
		 44	STW 	  0, 15,  -48
		 48	ADDI	  0, 15,   -4
		 52	PSH 	  0, 13,    4
		 56	BSR   -14
		 60	POP 	 14, 13,    4
		 64	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_two_value_parameters()
	{
		let source =
		###"""
		MODULE Test;
		VAR x: INTEGER;
			PROCEDURE foo(x, y: INTEGER);
			BEGIN
			END foo;
		BEGIN
			x := 5;
			foo(x, -10);
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,   12
		 28	RET    14
		 32	MOVI	 13,  0, 4092
		 36	PSH 	 14, 13,    4
		 40	MOVI	  0,  0,    5
		 44	STW 	  0, 15,  -48
		 48	LDW 	  0, 15,  -52
		 52	PSH 	  0, 13,    4
		 56	MOVI	  0,  0,  -10
		 60	PSH 	  0, 13,    4
		 64	BSR   -16
		 68	POP 	 14, 13,    4
		 72	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_procedure_call_with_two_reference_parameters()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
			PROCEDURE foo(VAR x, y: INTEGER);
			BEGIN
			END foo;
		BEGIN
			x := 5;
			y := -10;
			foo(x, y);
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,   12
		 28	RET    14
		 32	MOVI	 13,  0, 4088
		 36	PSH 	 14, 13,    4
		 40	MOVI	  0,  0,    5
		 44	STW 	  0, 15,  -48
		 48	MOVI	  0,  0,  -10
		 52	STW 	  0, 15,  -60
		 56	ADDI	  0, 15,   -4
		 60	PSH 	  0, 13,    4
		 64	ADDI	  0, 15,   -8
		 68	PSH 	  0, 13,    4
		 72	BSR   -18
		 76	POP 	 14, 13,    4
		 80	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
}

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
class CodeGeneration_Variable_Storage_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_code_generation_for_empty_module()
	{
		let source =
		###"""
		MODULE Test;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4096
		  4	PSH 	 14, 13,    4
		  8	POP 	 14, 13,    4
		 12	RET    14


		"""###
		
		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_defining_one_simple_global_variable()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: INTEGER;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4092
		  4	PSH 	 14, 13,    4
		  8	POP 	 14, 13,    4
		 12	RET    14


		"""###
		
		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_defining_two_simple_global_variables()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: INTEGER;
			y: BOOLEAN;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	POP 	 14, 13,    4
		 12	RET    14


		"""###
		
		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_defining_one_array_variable()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: ARRAY 5 OF INTEGER;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4076
		  4	PSH 	 14, 13,    4
		  8	POP 	 14, 13,    4
		 12	RET    14


		"""###
		
		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_module_defining_one_record_variable()
	{
		let source =
		###"""
		MODULE Test;
		VAR
			x: RECORD a: INTEGER; b: BOOLEAN END;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	POP 	 14, 13,    4
		 12	RET    14


		"""###
		
		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_empty_procedure_with_no_parameters()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			BEGIN END P;
		BEGIN END Test.
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
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_empty_procedure_with_one_value_parameter()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P(x: INTEGER);
			BEGIN END P;
		BEGIN END Test.
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
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_empty_procedure_with_one_reference_parameter()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P(VAR x: INTEGER);
			BEGIN END P;
		BEGIN END Test.
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
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
}

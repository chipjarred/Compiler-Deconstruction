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
	
}

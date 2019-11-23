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
class CodeGeneration_If_Statement_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_code_generation_for_simple_if_then_statement()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
		BEGIN
			x := 5;
			y := 2;
			IF x = 5 THEN
				x := x * 2;
			END;
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	MOVI	  0,  0,    2
		 20	STW 	  0, 15,  -28
		 24	LDW 	  0, 15,  -28
		 28	CMPI	  0,  0,    5
		 32	BNE     4
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_if_then_else_statement()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
		BEGIN
			x := 5;
			y := 2;
			IF x = 5 THEN
				x := x * 2
			ELSE
				y := y + 10
			END
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	MOVI	  0,  0,    2
		 20	STW 	  0, 15,  -28
		 24	LDW 	  0, 15,  -28
		 28	CMPI	  0,  0,    5
		 32	BNE     5
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	BR     4
		 52	LDW 	  0, 15,  -60
		 56	ADDI	  0,  0,   10
		 60	STW 	  0, 15,  -68
		 64	POP 	 14, 13,    4
		 68	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_if_then_elsif_statement()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
		BEGIN
			x := 5;
			y := 2;
			IF x = 5 THEN
				x := x * 2
			ELSIF y < 10 THEN
				y := y + 10
			END
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	MOVI	  0,  0,    2
		 20	STW 	  0, 15,  -28
		 24	LDW 	  0, 15,  -28
		 28	CMPI	  0,  0,    5
		 32	BNE     5
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	BR     7
		 52	LDW 	  0, 15,  -60
		 56	CMPI	  0,  0,   10
		 60	BGE     4
		 64	LDW 	  0, 15,  -72
		 68	ADDI	  0,  0,   10
		 72	STW 	  0, 15,  -80
		 76	POP 	 14, 13,    4
		 80	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_if_then_elsif_elsif_statement()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
		BEGIN
			x := 5;
			y := 2;
			IF x = 5 THEN
				x := x * 2
			ELSIF y < 10 THEN
				y := y + 10
			ELSIF x = 2 * y THEN
				y := x DIV 2
			END
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	MOVI	  0,  0,    2
		 20	STW 	  0, 15,  -28
		 24	LDW 	  0, 15,  -28
		 28	CMPI	  0,  0,    5
		 32	BNE     5
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	BR    17
		 52	LDW 	  0, 15,  -60
		 56	CMPI	  0,  0,   10
		 60	BGE     5
		 64	LDW 	  0, 15,  -72
		 68	ADDI	  0,  0,   10
		 72	STW 	  0, 15,  -80
		 76	BR    10
		 80	MOVI	  0,  0,    2
		 84	LDW 	  1, 15,  -92
		 88	MUL 	  0,  0,    1
		 92	LDW 	  1, 15,  -96
		 96	CMP 	  1,  1,    0
		100	BNE     4
		104	LDW 	  0, 15, -108
		108	DIVI	  0,  0,    2
		112	STW 	  0, 15, -120
		116	POP 	 14, 13,    4
		120	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
	
	// ----------------------------------
	func test_code_generation_for_if_then_elsif_else_statement()
	{
		let source =
		###"""
		MODULE Test;
		VAR x, y: INTEGER;
		BEGIN
			x := 5;
			y := 2;
			IF x = 5 THEN
				x := x * 2
			ELSIF y < 10 THEN
				y := y + 10
			ELSE
				y := x DIV 2
			END
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4088
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    5
		 12	STW 	  0, 15,  -16
		 16	MOVI	  0,  0,    2
		 20	STW 	  0, 15,  -28
		 24	LDW 	  0, 15,  -28
		 28	CMPI	  0,  0,    5
		 32	BNE     5
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	BR    11
		 52	LDW 	  0, 15,  -60
		 56	CMPI	  0,  0,   10
		 60	BGE     5
		 64	LDW 	  0, 15,  -72
		 68	ADDI	  0,  0,   10
		 72	STW 	  0, 15,  -80
		 76	BR     4
		 80	LDW 	  0, 15,  -84
		 84	DIVI	  0,  0,    2
		 88	STW 	  0, 15,  -96
		 92	POP 	 14, 13,    4
		 96	RET    14


		"""###

		guard let disassembly = generateDisassembly(from: source)
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
}

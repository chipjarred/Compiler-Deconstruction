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

// ---------------------------------------------------
class ParserTests: XCTestCase
{
	let example =
	###"""
	(* Example program from the Compiler Construction book by Niklaus Wirth *)

	MODULE Sample;
		PROCEDURE Multiply;
			VAR x, y, z: INTEGER;
		BEGIN Read(x); Read(y); z := 0;
			WHILE x > 0 DO
				IF x MOD 2 = 1 THEN z := z + y END ;
				y := 2*y; x := x DIV 2
			END ;
			Write(x); Write(y); Write(z); WriteLn
		END Multiply;

		PROCEDURE Divide;
			VAR x, y, r, q, w: INTEGER;
		BEGIN Read(x); Read(y); r := x; q := 0; w := y;
			WHILE w <= r DO w := 2*w END ;
			WHILE w > y DO
				q := 2*q; w := w DIV 2;
				IF w <= r THEN r := r - w; q := q + 1 END
			END ;
			Write(x); Write(y); Write(q); Write(r); WriteLn
		END Divide;

		PROCEDURE BinSearch;
			VAR i, j, k, n, x: INTEGER;
			a: ARRAY 32 OF INTEGER;
		BEGIN Read(n); k := 0;
			WHILE k < n DO Read(a[k]); k := k + 1 END ;
			Read(x); i := 0; j := n;
			WHILE i < j DO
				k := (i+j) DIV 2;
				IF x < a[k] THEN j := k ELSE i := k+1 END
			END ;
			Write(i); Write(j); Write(a[j]); WriteLn
		END BinSearch;
	END Sample.
	"""###

	// ---------------------------------------------------
	private func dump(_ code: String) {
		print("GENERATED CODE:---\n\(code)---- END CODE")
	}

	// ---------------------------------------------------
	func testExample()
	{
		let expectedCode =
		###"""
		entry  664
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	READ	  0,  0,    0
		 20	STW 	  0, 12,   -4
		 24	READ	  0,  0,    0
		 28	STW 	  0, 12,   -8
		 32	MOVI	  0,  0,    0
		 36	STW 	  0, 12,  -12
		 40	LDW 	  0, 12,   -4
		 44	CMPI	  0,  0,    0
		 48	BLE    17
		 52	LDW 	  0, 12,   -4
		 56	MODI	  0,  0,    2
		 60	CMPI	  0,  0,    1
		 64	BNE     5
		 68	LDW 	  0, 12,  -12
		 72	LDW 	  1, 12,   -8
		 76	ADD 	  0,  0,    1
		 80	STW 	  0, 12,  -12
		 84	MOVI	  0,  0,    2
		 88	LDW 	  1, 12,   -8
		 92	MUL 	  0,  0,    1
		 96	STW 	  0, 12,   -8
		100	LDW 	  0, 12,   -4
		104	DIVI	  0,  0,    2
		108	STW 	  0, 12,   -4
		112	BR   -18
		116	LDW 	  0, 12,   -4
		120	WRD 	  0,  0,    0
		124	LDW 	  0, 12,   -8
		128	WRD 	  0,  0,    0
		132	LDW 	  0, 12,  -12
		136	WRD 	  0,  0,    0
		140	WRL 	  0,  0,    0
		144	MOV 	 13,  0,   12
		148	POP 	 12, 13,    4
		152	POP 	 14, 13,    4
		156	RET    14
		160	PSH 	 14, 13,    4
		164	PSH 	 12, 13,    4
		168	MOV 	 12,  0,   13
		172	SUBI	 13, 13,   20
		176	READ	  0,  0,    0
		180	STW 	  0, 12,   -4
		184	READ	  0,  0,    0
		188	STW 	  0, 12,   -8
		192	LDW 	  0, 12,   -4
		196	STW 	  0, 12,  -12
		200	MOVI	  0,  0,    0
		204	STW 	  0, 12,  -16
		208	LDW 	  0, 12,   -8
		212	STW 	  0, 12,  -20
		216	LDW 	  0, 12,  -20
		220	LDW 	  1, 12,  -12
		224	CMP 	  0,  0,    1
		228	BGT     6
		232	MOVI	  0,  0,    2
		236	LDW 	  1, 12,  -20
		240	MUL 	  0,  0,    1
		244	STW 	  0, 12,  -20
		248	BR    -8
		252	LDW 	  0, 12,  -20
		256	LDW 	  1, 12,   -8
		260	CMP 	  0,  0,    1
		264	BLE    20
		268	MOVI	  0,  0,    2
		272	LDW 	  1, 12,  -16
		276	MUL 	  0,  0,    1
		280	STW 	  0, 12,  -16
		284	LDW 	  0, 12,  -20
		288	DIVI	  0,  0,    2
		292	STW 	  0, 12,  -20
		296	LDW 	  0, 12,  -20
		300	LDW 	  1, 12,  -12
		304	CMP 	  0,  0,    1
		308	BGT     8
		312	LDW 	  0, 12,  -12
		316	LDW 	  1, 12,  -20
		320	SUB 	  0,  0,    1
		324	STW 	  0, 12,  -12
		328	LDW 	  0, 12,  -16
		332	ADDI	  0,  0,    1
		336	STW 	  0, 12,  -16
		340	BR   -22
		344	LDW 	  0, 12,   -4
		348	WRD 	  0,  0,    0
		352	LDW 	  0, 12,   -8
		356	WRD 	  0,  0,    0
		360	LDW 	  0, 12,  -16
		364	WRD 	  0,  0,    0
		368	LDW 	  0, 12,  -12
		372	WRD 	  0,  0,    0
		376	WRL 	  0,  0,    0
		380	MOV 	 13,  0,   12
		384	POP 	 12, 13,    4
		388	POP 	 14, 13,    4
		392	RET    14
		396	PSH 	 14, 13,    4
		400	PSH 	 12, 13,    4
		404	MOV 	 12,  0,   13
		408	SUBI	 13, 13,  148
		412	READ	  0,  0,    0
		416	STW 	  0, 12,  -16
		420	MOVI	  0,  0,    0
		424	STW 	  0, 12,  -12
		428	LDW 	  0, 12,  -12
		432	LDW 	  1, 12,  -16
		436	CMP 	  0,  0,    1
		440	BGE    11
		444	LDW 	  0, 12,  -12
		448	CHKI	  0,  0,   32
		452	MULI	  0,  0,    4
		456	ADD 	  0, 12,    0
		460	READ	  1,  0,    0
		464	STW 	  1,  0, -148
		468	LDW 	  0, 12,  -12
		472	ADDI	  0,  0,    1
		476	STW 	  0, 12,  -12
		480	BR   -13
		484	READ	  0,  0,    0
		488	STW 	  0, 12,  -20
		492	MOVI	  0,  0,    0
		496	STW 	  0, 12,   -4
		500	LDW 	  0, 12,  -16
		504	STW 	  0, 12,   -8
		508	LDW 	  0, 12,   -4
		512	LDW 	  1, 12,   -8
		516	CMP 	  0,  0,    1
		520	BGE    21
		524	LDW 	  0, 12,   -4
		528	LDW 	  1, 12,   -8
		532	ADD 	  0,  0,    1
		536	DIVI	  0,  0,    2
		540	STW 	  0, 12,  -12
		544	LDW 	  0, 12,  -12
		548	CHKI	  0,  0,   32
		552	MULI	  0,  0,    4
		556	ADD 	  0, 12,    0
		560	LDW 	  1, 12,  -20
		564	LDW 	  2,  0, -148
		568	CMP 	  1,  1,    2
		572	BGE     4
		576	LDW 	  0, 12,  -12
		580	STW 	  0, 12,   -8
		584	BR     4
		588	LDW 	  0, 12,  -12
		592	ADDI	  0,  0,    1
		596	STW 	  0, 12,   -4
		600	BR   -23
		604	LDW 	  0, 12,   -4
		608	WRD 	  0,  0,    0
		612	LDW 	  0, 12,   -8
		616	WRD 	  0,  0,    0
		620	LDW 	  0, 12,   -8
		624	CHKI	  0,  0,   32
		628	MULI	  0,  0,    4
		632	ADD 	  0, 12,    0
		636	LDW 	  1,  0, -148
		640	WRD 	  0,  0,    1
		644	WRL 	  0,  0,    0
		648	MOV 	 13,  0,   12
		652	POP 	 12, 13,    4
		656	POP 	 14, 13,    4
		660	RET    14
		664	MOVI	 13,  0, 4096
		668	PSH 	 14, 13,    4
		672	POP 	 14, 13,    4
		676	RET    14


		"""###
		
		let parser = Parser()
		parser.compile(source: example, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
    }
	
	// ---------------------------------------------------
	func test_emitted_code_for_empty_module()
	{
		let source =
		###"""
		MODULE Test;
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
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_defining_one_global_variable()
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
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}

	// ---------------------------------------------------
	func test_emitted_code_for_empty_procedure_declaration_with_no_parameters()
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
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_declaration_with_one_value_parameter()
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
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_declaration_with_one_reference_parameter()
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
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_declaration_no_parameters_and_one_local_variable()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR x: INTEGER;
			BEGIN END P;
		BEGIN END Test.
		"""###
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    4
		 28	RET    14
		 32	MOVI	 13,  0, 4096
		 36	PSH 	 14, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_declaration_one_value_parameters_and_one_local_variable()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P(a: INTEGER);
			VAR x: INTEGER;
			BEGIN END P;
		BEGIN END Test.
		"""###
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    8
		 28	RET    14
		 32	MOVI	 13,  0, 4096
		 36	PSH 	 14, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_declaration_one_reference_parameters_and_one_local_variable()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P(VAR a: INTEGER);
			VAR x: INTEGER;
			BEGIN END P;
		BEGIN END Test.
		"""###
		let expectedCode =
		###"""
		entry   32
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOV 	 13,  0,   12
		 20	POP 	 12, 13,    4
		 24	POP 	 14, 13,    8
		 28	RET    14
		 32	MOVI	 13,  0, 4096
		 36	PSH 	 14, 13,    4
		 40	POP 	 14, 13,    4
		 44	RET    14

		
		"""###
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_standard_procedures()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE TestFunc;
				VAR x: INTEGER;
			BEGIN
				Read(x); Write(x); WriteLn
			END TestFunc;
		BEGIN
		END Test.
		"""###

		let expectedCode =
		###"""
		entry   52
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	READ	  0,  0,    0
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	WRD 	  0,  0,    0
		 32	WRL 	  0,  0,    0
		 36	MOV 	 13,  0,   12
		 40	POP 	 12, 13,    4
		 44	POP 	 14, 13,    4
		 48	RET    14
		 52	MOVI	 13,  0, 4096
		 56	PSH 	 14, 13,    4
		 60	POP 	 14, 13,    4
		 64	RET    14


		"""###
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_with_globals()
	{
		let source =
		###"""
		MODULE Test;
		    VAR x, y: INTEGER;
			PROCEDURE P(VAR x: INTEGER; y: INTEGER);
			BEGIN
			    x := y * y;
			END P;
		BEGIN
		    x := 0;
		    P(y, x);
		END Test.
		"""###

		let expectedCode =
		###"""
		entry   52
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    0
		 16	LDW 	  0, 12,   12
		 20	LDW 	  1, 12,    8
		 24	LDW 	  2, 12,    8
		 28	MUL 	  1,  1,    2
		 32	STW 	  1,  0,    0
		 36	MOV 	 13,  0,   12
		 40	POP 	 12, 13,    4
		 44	POP 	 14, 13,   12
		 48	RET    14
		 52	MOVI	 13,  0, 4088
		 56	PSH 	 14, 13,    4
		 60	MOVI	  0,  0,    0
		 64	STW 	  0, 15,  -68
		 68	ADDI	  0, 15,   -8
		 72	PSH 	  0, 13,    4
		 76	LDW 	  0, 15,  -80
		 80	PSH 	  0, 13,    4
		 84	BSR   -21
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###
		
		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_body_with_assignment_from_literal()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_body_with_assignment_from_variable()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_body_with_assignment_to_array_element()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_module_body_with_assignment_to_record_field()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}

	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_literal()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_local_variable()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_global_variable()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_global_variable_from_local_variable()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_array_element()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_record_field()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_global_array_element()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_global_record_field()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_variable_unary_minus()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x, y: INTEGER;
			BEGIN
				x := 5;
				y := -x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   52
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    8
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	MVN 	  0,  0,    0
		 32	STW 	  0, 12,   -8
		 36	MOV 	 13,  0,   12
		 40	POP 	 12, 13,    4
		 44	POP 	 14, 13,    4
		 48	RET    14
		 52	MOVI	 13,  0, 4096
		 56	PSH 	 14, 13,    4
		 60	POP 	 14, 13,    4
		 64	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_global_variable_from_variable_unary_minus()
	{
		let source =
		###"""
		MODULE Test;
			VAR y: INTEGER;
			PROCEDURE P;
				VAR
					x: INTEGER;
			BEGIN
				x := 5;
				y := -x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   52
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	MVN 	  0,  0,    0
		 32	STW 	  0, 15,  -36
		 36	MOV 	 13,  0,   12
		 40	POP 	 12, 13,    4
		 44	POP 	 14, 13,    4
		 48	RET    14
		 52	MOVI	 13,  0, 4092
		 56	PSH 	 14, 13,    4
		 60	POP 	 14, 13,    4
		 64	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_variable_unary_not()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
			VAR
				x, y: BOOLEAN;
			BEGIN
				x := TRUE;
				y := ~x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   64
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    8
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	BNE     3
		 32	MOVI	  0,  0,    1
		 36	BR     2
		 40	MOVI	  0,  0,    0
		 44	STW 	  0, 12,   -8
		 48	MOV 	 13,  0,   12
		 52	POP 	 12, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14
		 64	MOVI	 13,  0, 4096
		 68	PSH 	 14, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_global_variable_from_variable_unary_not()
	{
		let source =
		###"""
		MODULE Test;
			VAR y: BOOLEAN;
			PROCEDURE P;
				VAR
					x: BOOLEAN;
			BEGIN
				x := TRUE;
				y := ~x
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   64
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 12,   -4
		 24	LDW 	  0, 12,   -4
		 28	BNE     3
		 32	MOVI	  0,  0,    1
		 36	BR     2
		 40	MOVI	  0,  0,    0
		 44	STW 	  0, 15,  -48
		 48	MOV 	 13,  0,   12
		 52	POP 	 12, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14
		 64	MOVI	 13,  0, 4092
		 68	PSH 	 14, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_global_variable_unary_not()
	{
		let source =
		###"""
		MODULE Test;
			VAR y: BOOLEAN;
			PROCEDURE P;
				VAR
					x: BOOLEAN;
			BEGIN
				y := TRUE;
				x := ~y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   64
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,    4
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 15,  -24
		 24	LDW 	  0, 15,  -28
		 28	BNE     3
		 32	MOVI	  0,  0,    1
		 36	BR     2
		 40	MOVI	  0,  0,    0
		 44	STW 	  0, 12,   -4
		 48	MOV 	 13,  0,   12
		 52	POP 	 12, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14
		 64	MOVI	 13,  0, 4092
		 68	PSH 	 14, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_logical_OR_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y, z: BOOLEAN;
			BEGIN
				x := TRUE;
				y := FALSE;
				z := x OR y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   80
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    0
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	BNE     3
		 40	LDW 	  0, 12,   -8
		 44	BEQ     3
		 48	MOVI	  0,  0,    1
		 52	BR     2
		 56	MOVI	  0,  0,    0
		 60	STW 	  0, 12,  -12
		 64	MOV 	 13,  0,   12
		 68	POP 	 12, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14
		 80	MOVI	 13,  0, 4096
		 84	PSH 	 14, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_logical_AND_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y, z: BOOLEAN;
			BEGIN
				x := TRUE;
				y := FALSE;
				z := x & y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   80
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    0
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	BEQ     5
		 40	LDW 	  0, 12,   -8
		 44	BEQ     3
		 48	MOVI	  0,  0,    1
		 52	BR     2
		 56	MOVI	  0,  0,    0
		 60	STW 	  0, 12,  -12
		 64	MOV 	 13,  0,   12
		 68	POP 	 12, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14
		 80	MOVI	 13,  0, 4096
		 84	PSH 	 14, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_compound_logical_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y, z: BOOLEAN;
			BEGIN
				x := TRUE;
				y := FALSE;
				z := (x & y) OR (~x & ~y)
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   96
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    1
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    0
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	BEQ     3
		 40	LDW 	  0, 12,   -8
		 44	BNE     5
		 48	LDW 	  0, 12,   -4
		 52	BNE     5
		 56	LDW 	  0, 12,   -8
		 60	BNE     3
		 64	MOVI	  0,  0,    1
		 68	BR     2
		 72	MOVI	  0,  0,    0
		 76	STW 	  0, 12,  -12
		 80	MOV 	 13,  0,   12
		 84	POP 	 12, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14
		 96	MOVI	 13,  0, 4096
		100	PSH 	 14, 13,    4
		104	POP 	 14, 13,    4
		108	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_arithmetic_plus_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y, z: INTEGER;
			BEGIN
				x := 5;
				y := 2;
				z := x + y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   64
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    2
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	LDW 	  1, 12,   -8
		 40	ADD 	  0,  0,    1
		 44	STW 	  0, 12,  -12
		 48	MOV 	 13,  0,   12
		 52	POP 	 12, 13,    4
		 56	POP 	 14, 13,    4
		 60	RET    14
		 64	MOVI	 13,  0, 4096
		 68	PSH 	 14, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_equality_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y: INTEGER;
					z: BOOLEAN;
			BEGIN
				x := 5;
				y := 2;
				z := x = y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   80
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    2
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	LDW 	  1, 12,   -8
		 40	CMP 	  0,  0,    1
		 44	BNE     3
		 48	MOVI	  0,  0,    1
		 52	BR     2
		 56	MOVI	  0,  0,    0
		 60	STW 	  0, 12,  -12
		 64	MOV 	 13,  0,   12
		 68	POP 	 12, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14
		 80	MOVI	 13,  0, 4096
		 84	PSH 	 14, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_not_equals_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y: INTEGER;
					z: BOOLEAN;
			BEGIN
				x := 5;
				y := 2;
				z := x # y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   80
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    2
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	LDW 	  1, 12,   -8
		 40	CMP 	  0,  0,    1
		 44	BEQ     3
		 48	MOVI	  0,  0,    1
		 52	BR     2
		 56	MOVI	  0,  0,    0
		 60	STW 	  0, 12,  -12
		 64	MOV 	 13,  0,   12
		 68	POP 	 12, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14
		 80	MOVI	 13,  0, 4096
		 84	PSH 	 14, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_procedure_with_assignment_to_local_variable_from_less_than_expression()
	{
		let source =
		###"""
		MODULE Test;
			PROCEDURE P;
				VAR
					x, y: INTEGER;
					z: BOOLEAN;
			BEGIN
				x := 5;
				y := 2;
				z := x < y
			END P;
		BEGIN
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry   80
		  0	PSH 	 14, 13,    4
		  4	PSH 	 12, 13,    4
		  8	MOV 	 12,  0,   13
		 12	SUBI	 13, 13,   12
		 16	MOVI	  0,  0,    5
		 20	STW 	  0, 12,   -4
		 24	MOVI	  0,  0,    2
		 28	STW 	  0, 12,   -8
		 32	LDW 	  0, 12,   -4
		 36	LDW 	  1, 12,   -8
		 40	CMP 	  0,  0,    1
		 44	BGE     3
		 48	MOVI	  0,  0,    1
		 52	BR     2
		 56	MOVI	  0,  0,    0
		 60	STW 	  0, 12,  -12
		 64	MOV 	 13,  0,   12
		 68	POP 	 12, 13,    4
		 72	POP 	 14, 13,    4
		 76	RET    14
		 80	MOVI	 13,  0, 4096
		 84	PSH 	 14, 13,    4
		 88	POP 	 14, 13,    4
		 92	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_simple_if_then_statement()
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
		 32	BNE     4
		 36	LDW 	  0, 15,  -40
		 40	MULI	  0,  0,    2
		 44	STW 	  0, 15,  -48
		 48	POP 	 14, 13,    4
		 52	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_if_then_else_statement()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_if_then_elsif_statement()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_if_then_elsif_elsif_statement()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_if_then_elsif_else_statement()
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

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_while_loop()
	{
		let source =
		###"""
		MODULE Test;
		VAR x: INTEGER;
		BEGIN
			x := 0;
			WHILE x < 5 DO
				x := x + 1;
			END
		END Test.
		"""###
		
		let expectedCode =
		###"""
		entry    0
		  0	MOVI	 13,  0, 4092
		  4	PSH 	 14, 13,    4
		  8	MOVI	  0,  0,    0
		 12	STW 	  0, 15,  -16
		 16	LDW 	  0, 15,  -20
		 20	CMPI	  0,  0,    5
		 24	BGE     5
		 28	LDW 	  0, 15,  -32
		 32	ADDI	  0,  0,    1
		 36	STW 	  0, 15,  -40
		 40	BR    -6
		 44	POP 	 14, 13,    4
		 48	RET    14


		"""###

		let parser = Parser()
		parser.compile(source: source, sourceName: #function)
		let generatedCode:String = parser.disassemble()
		XCTAssertEqual(generatedCode, expectedCode)
	}
}

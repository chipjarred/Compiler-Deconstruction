//
//  OSPTests.swift
//  OSPTests
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import XCTest
@testable import OSP

// ---------------------------------------------------
class OSPTests: XCTestCase
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
		 20	STW 	  0, 11,   -4
		 24	READ	  0,  0,    0
		 28	STW 	  0, 11,   -8
		 32	MOVI	  0,  0,    0
		 36	STW 	  0, 11,  -12
		 40	LDW 	  0, 11,   -4
		 44	CMPI	  0,  0,    0
		 48	BLE    17
		 52	LDW 	  0, 11,   -4
		 56	MODI	  0,  0,    2
		 60	CMPI	  0,  0,    1
		 64	BNE     5
		 68	LDW 	  0, 11,  -12
		 72	LDW 	  1, 11,   -8
		 76	ADD 	  0,  0,    1
		 80	STW 	  0, 11,  -12
		 84	MOVI	  0,  0,    2
		 88	LDW 	  1, 11,   -8
		 92	MUL 	  0,  0,    1
		 96	STW 	  0, 11,   -8
		100	LDW 	  0, 11,   -4
		104	DIVI	  0,  0,    2
		108	STW 	  0, 11,   -4
		112	  -18
		116	LDW 	  0, 11,   -4
		120	WRD 	  0,  0,    0
		124	LDW 	  0, 11,   -8
		128	WRD 	  0,  0,    0
		132	LDW 	  0, 11,  -12
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
		180	STW 	  0, 11,   -4
		184	READ	  0,  0,    0
		188	STW 	  0, 11,   -8
		192	LDW 	  0, 11,   -4
		196	STW 	  0, 11,  -12
		200	MOVI	  0,  0,    0
		204	STW 	  0, 11,  -16
		208	LDW 	  0, 11,   -8
		212	STW 	  0, 11,  -20
		216	LDW 	  0, 11,  -20
		220	LDW 	  1, 11,  -12
		224	CMP 	  0,  0,    1
		228	BGT     6
		232	MOVI	  0,  0,    2
		236	LDW 	  1, 11,  -20
		240	MUL 	  0,  0,    1
		244	STW 	  0, 11,  -20
		248	   -8
		252	LDW 	  0, 11,  -20
		256	LDW 	  1, 11,   -8
		260	CMP 	  0,  0,    1
		264	BLE    20
		268	MOVI	  0,  0,    2
		272	LDW 	  1, 11,  -16
		276	MUL 	  0,  0,    1
		280	STW 	  0, 11,  -16
		284	LDW 	  0, 11,  -20
		288	DIVI	  0,  0,    2
		292	STW 	  0, 11,  -20
		296	LDW 	  0, 11,  -20
		300	LDW 	  1, 11,  -12
		304	CMP 	  0,  0,    1
		308	BGT     8
		312	LDW 	  0, 11,  -12
		316	LDW 	  1, 11,  -20
		320	SUB 	  0,  0,    1
		324	STW 	  0, 11,  -12
		328	LDW 	  0, 11,  -16
		332	ADDI	  0,  0,    1
		336	STW 	  0, 11,  -16
		340	  -22
		344	LDW 	  0, 11,   -4
		348	WRD 	  0,  0,    0
		352	LDW 	  0, 11,   -8
		356	WRD 	  0,  0,    0
		360	LDW 	  0, 11,  -16
		364	WRD 	  0,  0,    0
		368	LDW 	  0, 11,  -12
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
		416	STW 	  0, 11,  -16
		420	MOVI	  0,  0,    0
		424	STW 	  0, 11,  -12
		428	LDW 	  0, 11,  -12
		432	LDW 	  1, 11,  -16
		436	CMP 	  0,  0,    1
		440	BGE    11
		444	LDW 	  0, 11,  -12
		448	CHKI	  0,  0,   32
		452	MULI	  0,  0,    4
		456	ADD 	  0, 12,    0
		460	READ	  1,  0,    0
		464	STW 	  0, 15, -148
		468	LDW 	  0, 11,  -12
		472	ADDI	  0,  0,    1
		476	STW 	  0, 11,  -12
		480	  -13
		484	READ	  0,  0,    0
		488	STW 	  0, 11,  -20
		492	MOVI	  0,  0,    0
		496	STW 	  0, 11,   -4
		500	LDW 	  0, 11,  -16
		504	STW 	  0, 11,   -8
		508	LDW 	  0, 11,   -4
		512	LDW 	  1, 11,   -8
		516	CMP 	  0,  0,    1
		520	BGE    21
		524	LDW 	  0, 11,   -4
		528	LDW 	  1, 11,   -8
		532	ADD 	  0,  0,    1
		536	DIVI	  0,  0,    2
		540	STW 	  0, 11,  -12
		544	LDW 	  0, 11,  -12
		548	CHKI	  0,  0,   32
		552	MULI	  0,  0,    4
		556	ADD 	  0, 12,    0
		560	LDW 	  1, 11,  -20
		564	LDW 	  1, 15, -148
		568	CMP 	  1,  1,    2
		572	BGE     4
		576	LDW 	  0, 11,  -12
		580	STW 	  0, 11,   -8
		584	BR     4
		588	LDW 	  0, 11,  -12
		592	ADDI	  0,  0,    1
		596	STW 	  0, 11,   -4
		600	  -23
		604	LDW 	  0, 11,   -4
		608	WRD 	  0,  0,    0
		612	LDW 	  0, 11,   -8
		616	WRD 	  0,  0,    0
		620	LDW 	  0, 11,   -8
		624	CHKI	  0,  0,   32
		628	MULI	  0,  0,    4
		632	ADD 	  0, 12,    0
		636	LDW 	  0, 15, -148
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
		
		OSP.Compile(source: example)
		let generatedCode:String = OSP.Decode()
		XCTAssertEqual(generatedCode, expectedCode)
    }
	
	// ---------------------------------------------------
	func test_emitted_code_for_empty_module()
	{
		let code =
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
		
		OSP.Compile(source: code)
		let generatedCode:String = OSP.Decode()
		XCTAssertEqual(generatedCode, expectedCode)
	}
	
	// ---------------------------------------------------
	func test_emitted_code_for_standard_procedures()
	{
		let code =
		###"""
		MODULE Test;
			PROCEDURE TestFunc;
				VAR x: INTEGER;
			BEGIN
				Read(x); Write(x); WriteLn
			END
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
		 20	STW 	  0, 11,   -4
		 24	LDW 	  0, 11,   -4
		 28	WRD 	  0,  0,    0
		 32	WRL 	  0,  0,    0
		 36	MOV 	 13,  0,   12
		 40	POP 	 12, 13,    4
		 44	POP 	 14, 13,    4
		 48	RET    14
		 52	MOVI	 13,  0, 4096
		 56	PSH 	 14, 13,    4


		"""###
		
		OSP.Compile(source: code)
		let generatedCode:String = OSP.Decode()
		XCTAssertEqual(generatedCode, expectedCode)
	}
}

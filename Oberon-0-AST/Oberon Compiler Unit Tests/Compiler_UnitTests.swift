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
class Compiler_UnitTests: XCTestCase
{
	// ----------------------------------
	func test_compiles_Wirth_example_code()
	{
		let source =
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
		BEGIN
		END Sample.
		"""###

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

		let compiler = Compiler()
		guard let (_, disassembly) =
			compiler.compile(source: source, sourceName: "Test")
		else { return }
		
		XCTAssertEqual(disassembly, expectedCode)
	}
}

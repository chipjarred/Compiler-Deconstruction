//
//  OSSTests.swift
//  OSSTests
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import XCTest
@testable import OSS
import Oberon
import Texts

// ---------------------------------------------------
class OSSTests: XCTestCase
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
    func test_scanner_generates_correct_symbols_for_Wirths_example_source()
	{
		let T: Text = TextDesc(example)
		OSS.Init(T, 0)
		
		// ---------------------------------------------------
		func getSymbol() -> INTEGER
		{
			var sym: INTEGER = 0
			OSS.Get(&sym)
			return sym
		}
		
		// MODULE Sample;
		XCTAssertEqual(getSymbol(), OSS.module)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// PROCEDURE Multiply;
		XCTAssertEqual(getSymbol(), OSS.procedure)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// VAR x, y, z: INTEGER;
		XCTAssertEqual(getSymbol(), OSS.var)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.colon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// BEGIN Read(x); Read(y); z := 0;
		XCTAssertEqual(getSymbol(), OSS.begin)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// WHILE x > 0 DO
		XCTAssertEqual(getSymbol(), OSS.while)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.gtr)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.do)
		
		// IF x MOD 2 = 1 THEN z := z + y END ;
		XCTAssertEqual(getSymbol(), OSS.if)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.mod)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.eql)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.then)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.plus)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// y := 2*y; x := x DIV 2
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.times)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.div)
		XCTAssertEqual(getSymbol(), OSS.number)
		
		// END ;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// Write(x); Write(y); Write(z); WriteLn
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)

		// END Multiply;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)


		// PROCEDURE Divide;
		XCTAssertEqual(getSymbol(), OSS.procedure)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// VAR x, y, r, q, w: INTEGER;
		XCTAssertEqual(getSymbol(), OSS.var)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.colon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// BEGIN Read(x); Read(y); r := x; q := 0; w := y;
		XCTAssertEqual(getSymbol(), OSS.begin)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// WHILE w <= r DO w := 2*w END ;
		XCTAssertEqual(getSymbol(), OSS.while)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.leq)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.do)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.times)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// WHILE w > y DO
		XCTAssertEqual(getSymbol(), OSS.while)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.gtr)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.do)
		
		// q := 2*q; w := w DIV 2;
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.times)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.div)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// IF w <= r THEN r := r - w; q := q + 1 END
		XCTAssertEqual(getSymbol(), OSS.if)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.leq)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.then)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.minus)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.plus)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.end)

		// END ;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// Write(x); Write(y); Write(q); Write(r); WriteLn
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)

		// END Divide;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// PROCEDURE BinSearch;
		XCTAssertEqual(getSymbol(), OSS.procedure)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// VAR i, j, k, n, x: INTEGER;
		XCTAssertEqual(getSymbol(), OSS.var)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.comma)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.colon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// a: ARRAY 32 OF INTEGER;
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.colon)
		XCTAssertEqual(getSymbol(), OSS.array)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.of)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// BEGIN Read(n); k := 0;
		XCTAssertEqual(getSymbol(), OSS.begin)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// WHILE k < n DO Read(a[k]); k := k + 1 END ;
		XCTAssertEqual(getSymbol(), OSS.while)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lss)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.do)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lbrak)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rbrak)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.plus)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// Read(x); i := 0; j := n;
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// WHILE i < j DO
		XCTAssertEqual(getSymbol(), OSS.while)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lss)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.do)
		
		// k := (i+j) DIV 2;
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.plus)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.div)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		
		// IF x < a[k] THEN j := k ELSE i := k+1 END
		XCTAssertEqual(getSymbol(), OSS.if)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lss)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lbrak)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rbrak)
		XCTAssertEqual(getSymbol(), OSS.then)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.else)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.becomes)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.plus)
		XCTAssertEqual(getSymbol(), OSS.number)
		XCTAssertEqual(getSymbol(), OSS.end)

		// END ;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// Write(i); Write(j); Write(a[j]); WriteLn
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lparen)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.lbrak)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.rbrak)
		XCTAssertEqual(getSymbol(), OSS.rparen)
		XCTAssertEqual(getSymbol(), OSS.semicolon)
		XCTAssertEqual(getSymbol(), OSS.ident)

		// END BinSearch;
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.semicolon)

		// END Sample.
		XCTAssertEqual(getSymbol(), OSS.end)
		XCTAssertEqual(getSymbol(), OSS.ident)
		XCTAssertEqual(getSymbol(), OSS.period)

		XCTAssertEqual(getSymbol(), OSS.eof)
	}
}

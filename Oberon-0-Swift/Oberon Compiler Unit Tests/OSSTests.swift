//
//  OSSTests.swift
//  OSSTests
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import XCTest

// ---------------------------------------------------
class OSSTests: XCTestCase
{
	let example =
	###"""
	(* Example program from the Compiler Construction book by Niklaus Wirth *)

	MODULE Sample;
		PROCEDURE Multiply;
			VAR x, y, z: Int;
		BEGIN Read(x); Read(y); z := 0;
			WHILE x > 0 DO
				IF x MOD 2 = 1 THEN z := z + y END ;
				y := 2*y; x := x DIV 2
			END ;
			Write(x); Write(y); Write(z); WriteLn
		END Multiply;

		PROCEDURE Divide;
			VAR x, y, r, q, w: Int;
		BEGIN Read(x); Read(y); r := x; q := 0; w := y;
			WHILE w <= r DO w := 2*w END ;
			WHILE w > y DO
				q := 2*q; w := w DIV 2;
				IF w <= r THEN r := r - w; q := q + 1 END
			END ;
			Write(x); Write(y); Write(q); Write(r); WriteLn
		END Divide;

		PROCEDURE BinSearch;
			VAR i, j, k, n, x: Int;
			a: ARRAY 32 OF Int;
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
		let inputStream = InputStream(contentsOf: example)
		inputStream.open()
		Lexer.Init(sourceStream: inputStream)
		
		// ---------------------------------------------------
		func getSymbol() -> TokenType {
			return Lexer.getToken().symbol
		}
		
		// MODULE Sample;
		XCTAssertEqual(getSymbol(), .module)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// PROCEDURE Multiply;
		XCTAssertEqual(getSymbol(), .procedure)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// VAR x, y, z: Int;
		XCTAssertEqual(getSymbol(), .var)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .colon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// BEGIN Read(x); Read(y); z := 0;
		XCTAssertEqual(getSymbol(), .begin)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// WHILE x > 0 DO
		XCTAssertEqual(getSymbol(), .while)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .greaterThan)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .do)
		
		// IF x MOD 2 = 1 THEN z := z + y END ;
		XCTAssertEqual(getSymbol(), .if)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .mod)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .isEqualTo)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .then)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .plus)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// y := 2*y; x := x DIV 2
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .times)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .div)
		XCTAssertEqual(getSymbol(), .number)
		
		// END ;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// Write(x); Write(y); Write(z); WriteLn
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)

		// END Multiply;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)


		// PROCEDURE Divide;
		XCTAssertEqual(getSymbol(), .procedure)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// VAR x, y, r, q, w: Int;
		XCTAssertEqual(getSymbol(), .var)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .colon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)

		// BEGIN Read(x); Read(y); r := x; q := 0; w := y;
		XCTAssertEqual(getSymbol(), .begin)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)

		// WHILE w <= r DO w := 2*w END ;
		XCTAssertEqual(getSymbol(), .while)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .lessThanOrEqualTo)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .do)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .times)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)

		// WHILE w > y DO
		XCTAssertEqual(getSymbol(), .while)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .greaterThan)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .do)
		
		// q := 2*q; w := w DIV 2;
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .times)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .div)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)

		// IF w <= r THEN r := r - w; q := q + 1 END
		XCTAssertEqual(getSymbol(), .if)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .lessThanOrEqualTo)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .then)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .minus)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .plus)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .end)

		// END ;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// Write(x); Write(y); Write(q); Write(r); WriteLn
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)

		// END Divide;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)

		// PROCEDURE BinSearch;
		XCTAssertEqual(getSymbol(), .procedure)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// VAR i, j, k, n, x: Int;
		XCTAssertEqual(getSymbol(), .var)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .comma)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .colon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// a: ARRAY 32 OF Int;
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .colon)
		XCTAssertEqual(getSymbol(), .array)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .of)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// BEGIN Read(n); k := 0;
		XCTAssertEqual(getSymbol(), .begin)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)

		// WHILE k < n DO Read(a[k]); k := k + 1 END ;
		XCTAssertEqual(getSymbol(), .while)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .lessThan)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .do)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openBracket)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeBracket)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .plus)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)

		// Read(x); i := 0; j := n;
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)

		// WHILE i < j DO
		XCTAssertEqual(getSymbol(), .while)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .lessThan)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .do)
		
		// k := (i+j) DIV 2;
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .plus)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .div)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .semicolon)
		
		// IF x < a[k] THEN j := k ELSE i := k+1 END
		XCTAssertEqual(getSymbol(), .if)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .lessThan)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openBracket)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeBracket)
		XCTAssertEqual(getSymbol(), .then)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .else)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .becomes)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .plus)
		XCTAssertEqual(getSymbol(), .number)
		XCTAssertEqual(getSymbol(), .end)

		// END ;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .semicolon)

		// Write(i); Write(j); Write(a[j]); WriteLn
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openParen)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .openBracket)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .closeBracket)
		XCTAssertEqual(getSymbol(), .closeParen)
		XCTAssertEqual(getSymbol(), .semicolon)
		XCTAssertEqual(getSymbol(), .identifier)

		// END BinSearch;
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .semicolon)

		// END Sample.
		XCTAssertEqual(getSymbol(), .end)
		XCTAssertEqual(getSymbol(), .identifier)
		XCTAssertEqual(getSymbol(), .period)

		XCTAssertEqual(getSymbol(), .eof)
	}
}

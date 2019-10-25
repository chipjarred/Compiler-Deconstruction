//
//  RISCTests.swift
//  RISCTests
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import XCTest

// ---------------------------------------------------
class RISCTests: XCTestCase
{
	// ---------------------------------------------------
	func testExample()
	{
		let source =
		"""
		MODULE Test;
			PROCEDURE TestFunc;
				VAR x: Int;
			BEGIN
				Read(x); Write(x); WriteLn
			END TestFunc;
		BEGIN
			TestFunc
		END Test.
		"""
		OSP.Compile(source: source)
		var code = OSP.program
		XCTAssert(code.count > 1)
		XCTAssertEqual(code[0], OSP.magic)
		let entry = code[1]
		
		code.removeFirst(2)
		RISC.Load(code, Int(code.count))
		let inputs = Texts.TextDesc("5\n")
		var scanner = Texts.Scanner()
		Texts.OpenScanner(&scanner, inputs, 0)
		let outputs = Texts.TextDesc()
		RISC.Execute(entry, &scanner, outputs)
		
		XCTAssertEqual(outputs.description, " 5\n")
	}
}

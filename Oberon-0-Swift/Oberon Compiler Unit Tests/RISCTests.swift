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
		Oberon0Parser.compile(source: source)
		var code = Oberon0Parser.program
		XCTAssert(code.count > 1)
		XCTAssertEqual(code[0], Oberon0Parser.magic)
		let entry = code[1]
		
		code.removeFirst(2)
		var emulator = RISCEmulator()
		emulator.load(code, code.count)
		var scanner = RISCInputScanner(contentsOf: "5\n")
		var outputs: String = ""
		emulator.execute(entry, input: &scanner, output: &outputs)
		
		XCTAssertEqual(outputs, " 5\n")
	}
}

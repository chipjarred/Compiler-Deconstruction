// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
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
class RISCEmulator_UnitTests: XCTestCase
{
	// ---------------------------------------------------
	func test_execute_program_compiled_with_AST_generating_compiler()
	{
		let source =
		"""
		MODULE Test;
			PROCEDURE TestFunc;
				VAR x: INTEGER;
			BEGIN
				Read(x); Write(x); WriteLn
			END TestFunc;
		BEGIN
			TestFunc
		END Test.
		"""
		
		let compiler = Compiler()
		guard var (code, _) = compiler.compile(source: source, sourceName: #function)
		else
		{
			XCTFail("Compile failed with \(compiler.errorCount) errors.")
			return
		}
		XCTAssert(code.count > 1)
		XCTAssertEqual(code[0], Compiler.magic)
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

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

import Foundation
import XCTest

// ----------------------------------
func generateDisassembly(
	from code: String,
	file: StaticString = #file,
	line: UInt = #line) -> String?
{
	let reporter = ErrorReporter(FileHandle.standardError)!
	let parser = NewParser(
		source: code,
		sourceName: "Test.Mod",
		errorsTo: reporter
	)
	
	guard let ast = parser.parse() else
	{
		XCTFail(
			"Got nil AST. \(reporter.errorCount) parser errors",
			file: file,
			line: line
		)
		return nil
	}
	
	let typeChecker = TypeChecker(errorsTo: reporter)
	typeChecker.check(ast)
	
	guard reporter.errorCount == 0 else
	{
		XCTFail(
			"Got \(reporter.errorCount) type checking errors",
			file: file,
			line: line
		)
		return nil
	}
	
	let codeGenerator = CodeGenerator(errorsTo: reporter)
	let _ = codeGenerator.generate(from: ast)
	
	guard reporter.errorCount == 0 else
	{
		XCTFail(
			"Got \(reporter.errorCount) code generation errors",
			file: file,
			line: line
		)
		return nil
	}
	
	
	return codeGenerator.disassemble()
}

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
internal func typeCheck(
	_ code: String,
	file: StaticString = #file,
	line: UInt = #line) -> AbstractSyntaxTree?
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
			"Got type checking \(reporter.errorCount) errors",
			file: file,
			line: line
		)
		return nil
	}
	
	return ast
}

// ---------------------------------------------------
internal func constant(
	named name: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	guard let ast = typeCheck(code) else { return nil }
	
	guard let node =
		ast.findNode(kind: .constantDeclaration, name: name)
	else
	{
		XCTFail("No constant found named \"name\"", file: file, line: line)
		return nil
	}
	
	XCTAssert(
		node.children.count == 2,
		"Expected 2 children nodes, but got \(node.children.count) for "
		+ "constant declaration",
		file: file,
		line: line
	)
	
	return node.children[0]
}


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
internal func findNode(
	named name: String,
	kind: ASTNode.Kind,
	childNodeIndex: Int?,
	expectedChildCount: Int,
	descibedBy description: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	guard let ast = typeCheck(code, file: file, line: line) else { return nil }

	guard let node = ast.findNode(kind: kind, name: name) else
	{
		XCTFail(
			"No \(description) found named \"\(name)\"",
			file: file,
			line: line
		)
		return nil
	}
	
	XCTAssert(
		node.children.count == expectedChildCount,
		"Expected \(expectedChildCount) child nodes, but got"
		+ " \(node.children.count) for \(description)",
		file: file,
		line: line
	)
	
	if let index = childNodeIndex {
		return node.children[index]
	}
	
	return node
}

// ---------------------------------------------------
/**
Find the first `ASTNode` matching `kind` after skipping a specified number of matching nodes.

- Parameters:
	- index: number of matching `ASTNode` to skip
	- kind: `ASTNode.Kind` to match
	- expectedChildCount: number of child node's the matching node is expected to have. If `nil`,
		the number of children in the returned `ASTNode` will not be checked.
	- desribedBy: `String` description of the node type to be used in assertion failure messages
	- code: `String` containing the source code from which the AST to be searched is generated
	- file: `StaticString` holding the unit test file name to be used in assertion failure messages
	- line: `UInt` holding the line number in the unit test file to be used in assertion failure messages

- Returns: the matching `ASTNode`, or `nil` if no match is found.
*/
internal func findNode(
	skip: Int,
	kind: ASTNode.Kind,
	expectedChildCount: Int?,
	descibedBy description: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	guard let ast = typeCheck(code, file: file, line: line) else { return nil }
	
	guard let node = ast.findNode(kind: kind, skip: skip) else
	{
		XCTFail(
			"No \(description) matching node found after skipping \"\(skip)\" "
			+ "matches",
			file: file,
			line: line
		)
		return nil
	}
	
	if let expectedChildren = expectedChildCount
	{
		XCTAssert(
			node.children.count == expectedChildren,
			"Expected \(expectedChildren) child nodes, but got"
			+ " \(node.children.count) for \(description)",
			file: file,
			line: line
		)
	}
	
	return node
}

// ---------------------------------------------------
internal func assignment(
	skip: Int,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	return findNode(
		skip: skip,
		kind: .assignment,
		expectedChildCount: 2,
		descibedBy: "assignment",
		in: code,
		file: file,
		line: line
	)
}

// ---------------------------------------------------
internal func constant(
	named name: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	return findNode(
		named: name,
		kind: .constantDeclaration,
		childNodeIndex: 0,
		expectedChildCount: 2,
		descibedBy: "constant",
		in: code,
		file: file,
		line: line
	)
}

// ---------------------------------------------------
internal func type(
	named name: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	return findNode(
		named: name,
		kind: .typeDeclaration,
		childNodeIndex: 0,
		expectedChildCount: 2,
		descibedBy: "type",
		in: code,
		file: file,
		line: line
	)
}

// ---------------------------------------------------
internal func variable(
	named name: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	return findNode(
		named: name,
		kind: .variableDeclaration,
		childNodeIndex: nil,
		expectedChildCount: 1,
		descibedBy: "variable",
		in: code,
		file: file,
		line: line
	)
}

// ---------------------------------------------------
internal func procedure(
	named name: String,
	in code: String,
	file: StaticString = #file,
	line: UInt = #line) -> ASTNode?
{
	return findNode(
		named: name,
		kind: .procedureDeclaration,
		childNodeIndex: 0,
		expectedChildCount: 7,
		descibedBy: "procedure",
		in: code,
		file: file,
		line: line
	)
}

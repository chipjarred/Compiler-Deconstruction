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
class TypeCheck_Declarations_UnitTests: XCTestCase
{
	// ----------------------------------
	private func compile(
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
			XCTFail("Got type checking \(reporter.errorCount) errors")
			return nil
		}
		
		return ast
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_plain_integer_value()
	{
		let code =
		"""
		MODULE Test;
		CONST ultimateAnswer = 42;
		BEGIN
		END TEST.
		"""
		
		guard let ast = compile(code) else { return }
		
		guard let node =
			ast.findNode(kind: .constantDeclaration, name: "ultimateAnswer")
		else
		{
			XCTFail("No node found")
			return
		}
		
		XCTAssertEqual(node.children.count, 2)
		XCTAssertEqual(node.children[0].typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.children[0].value, 42)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_negative_value()
	{
		let code =
		"""
		MODULE Test;
		CONST x = -99;
		BEGIN
		END TEST.
		"""
		
		guard let ast = compile(code) else { return }
		
		guard let node =
			ast.findNode(kind: .constantDeclaration, name: "x")
		else
		{
			XCTFail("No node found")
			return
		}
		
		XCTAssertEqual(node.children.count, 2)
		XCTAssertEqual(node.children[0].typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.children[0].value, -99)
	}
	
	// ----------------------------------
	func test_integer_constant_declaration_with_unary_positive_value()
	{
		let code =
		"""
		MODULE Test;
		CONST numberOfProblems = +99;
		BEGIN
		END TEST.
		"""
		
		guard let ast = compile(code) else { return }
		
		guard let node =
			ast.findNode(kind: .constantDeclaration, name: "numberOfProblems")
		else
		{
			XCTFail("No node found")
			return
		}
		
		XCTAssertEqual(node.children.count, 2)
		XCTAssertEqual(node.children[0].typeInfo, TypeInfo.integer)
		XCTAssertEqual(node.children[0].value, 99)
	}
	
	// ----------------------------------
	func test_true_boolean_constant_declaration()
	{
		let code =
		"""
		MODULE Test;
		CONST squirrelsLikeNuts = TRUE;
		BEGIN
		END TEST.
		"""
		
		guard let ast = compile(code) else { return }
		
		guard let node =
			ast.findNode(kind: .constantDeclaration, name: "squirrelsLikeNuts")
		else
		{
			XCTFail("No node found")
			return
		}
		
		XCTAssertEqual(node.children.count, 2)
		XCTAssertEqual(node.children[0].typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.children[0].value, 1)
	}
	
	// ----------------------------------
	func test_false_boolean_constant_declaration()
	{
		let code =
		"""
		MODULE Test;
		CONST tigersLikeNuts = FALSE;
		BEGIN
		END TEST.
		"""
		
		guard let ast = compile(code) else { return }
		
		guard let node =
			ast.findNode(kind: .constantDeclaration, name: "tigersLikeNuts")
		else
		{
			XCTFail("No node found")
			return
		}
		
		XCTAssertEqual(node.children.count, 2)
		XCTAssertEqual(node.children[0].typeInfo, TypeInfo.boolean)
		XCTAssertEqual(node.children[0].value, 0)
	}
}

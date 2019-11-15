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
extension XCTestCase
{
	// ----------------------------------
	func parse(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> AbstractSyntaxTree?
	{
		let reporter = ErrorReporter(FileHandle.standardError)!
		let parser = NewParser(source: expression, errorsTo: reporter)
		if let node = parser.parse(allowErrors: true)
		{
			XCTAssertEqual(reporter.errorCount, 0)
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseExpression(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseExpression() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseStatement(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseStatement() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseVariableDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseVariableDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseConstantDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseConstantDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseTypeDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression).parseTypeDeclaration() {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseBeginEndBlock(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression)
			.parseCodeBlock(startingWith: .begin, terminatedBy: [.end])
		{
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}

	// ----------------------------------
	func parseSection(
		_ sectionType: TokenType,
		code expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		assert(TokenType.sectionTypes.contains(sectionType))
		
		if let node = NewParser(source: expression).parseSection(sectionType) {
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}

	// ----------------------------------
	func parseProcedureDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression)
			.parseScopeDeclaration(asModule: false)
		{
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
	
	// ----------------------------------
	func parseModuleDeclaration(
		_ expression: String,
		file: StaticString = #file,
		line: UInt = #line) -> ASTNode?
	{
		if let node = NewParser(source: expression)
			.parseScopeDeclaration(asModule: true)
		{
			return node
		}
		XCTFail("Got empty AST", file: file, line: line)
		return nil
	}
}

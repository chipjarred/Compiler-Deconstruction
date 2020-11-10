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

// ----------------------------------
class ParsingIfStatement_UnitTests: XCTestCase 
{
	// ----------------------------------
	func test_parses_simple_if_statement()
	{
		let code =
		"""
		IF x = y THEN
			x := y + 1
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {x = (y + 1)}, {}}")
	}
	
	// ----------------------------------
	func test_parses_simple_if_statement_with_else()
	{
		let code =
		"""
		IF x = y THEN
			x := y + 1
		ELSE
			y := y DIV 2;
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {x = (y + 1)}, {y = (y DIV 2)}}")
	}
	
	// ----------------------------------
	func test_parses_simple_if_statement_with_elsif()
	{
		let code =
		"""
		IF x = y THEN
			x := y + 1
		ELSIF x < y THEN
			y := y DIV 2;
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {x = (y + 1)}, ELSIF{(x < y), {y = (y DIV 2)}, {}}}")
	}
	
	// ----------------------------------
	func test_parses_simple_if_statement_with_elsif_and_else()
	{
		let code =
		"""
		IF x = y THEN
			x := y + 1
		ELSIF x < y THEN
			y := y DIV 2
		ELSE
			y := y * 2
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {x = (y + 1)}, ELSIF{(x < y), {y = (y DIV 2)}, {y = (y * 2)}}}")
	}
	
	// ----------------------------------
	func test_parses_if_statement_nested_in_then_block()
	{
		let code =
		"""
		IF x = y THEN
			IF a > b THEN
				x := a
			ELSE
				x := b
			END
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {IF{(a > b), {x = a}, {x = b}}}, {}}")
	}
	
	// ----------------------------------
	func test_parses_if_statement_nested_in_else_block()
	{
		let code =
		"""
		IF x = y THEN
			x := y + 1
		ELSE
			IF a > b THEN
				x := a
			ELSE
				x := b
			END
		END;
		"""
		guard let ast = parseStatement(code) else { return }
		
		let result = "\(ast)"
		
		XCTAssertEqual(result, "IF{(x = y), {x = (y + 1)}, {IF{(a > b), {x = a}, {x = b}}}}")
	}
}

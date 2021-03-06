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

// ---------------------------------------------------
public enum TokenType: Int, Comparable
{
	case null = 0
	case times = 1
	case div = 3
	case mod = 4
	case and = 5
	case plus = 6
	case minus = 7
	case or = 8
	case isEqualTo = 9
	case isNotEqualTo = 10
	case lessThan = 11
	case greaterThanOrEqualTo = 12
	case lessThanOrEqualTo = 13
	case greaterThan = 14
	case period = 18
	case comma = 19
	case colon = 20
	case closeParen = 22
	case closeBracket = 23
	case of = 25
	case then = 26
	case `do` = 27
	case openParen = 29
	case openBracket = 30
	case not = 32
	case becomes = 33
	case number = 34
	case identifier = 37
	case semicolon = 38
	case end = 40
	case `else` = 41
	case elsif = 42
	case `if` = 44
	case `while` = 46
	case array = 54
	case record = 55
	case const = 57
	case type = 58
	case `var` = 59
	case procedure = 60
	case begin = 61
	case module = 63
	case eof = 64
	
	// ---------------------------------------------------
	public static func keywordTokenType(for string: String) -> TokenType? {
		return keywordMap[string]
	}
	
	// ---------------------------------------------------
	public static func < (lhs: TokenType, rhs: TokenType) -> Bool {
		return lhs.rawValue < rhs.rawValue
	}

	fileprivate static var keywordMap = makeKeyWords()
	
	// ---------------------------------------------------
	fileprivate static func makeKeyWords() -> [String: TokenType]
	{
		var keywordMap = [String: TokenType]()
		
		keywordMap["BY"] 			= .null
		keywordMap["DO"] 			= .`do`
		keywordMap["IF"] 			= .`if`
		keywordMap["IN"] 			= .null
		keywordMap["IS"] 			= .null
		keywordMap["OF"] 			= .of
		keywordMap["OR"] 			= .or
		keywordMap["TO"] 			= .null
		keywordMap["END"] 			= .end
		keywordMap["FOR"] 			= .null
		keywordMap["MOD"] 			= .mod
		keywordMap["NIL"] 			= .null
		keywordMap["VAR"] 			= .`var`
		keywordMap["CASE"] 			= .null
		keywordMap["ELSE"] 			= .`else`
		keywordMap["EXIT"] 			= .null
		keywordMap["THEN"] 			= .then
		keywordMap["TYPE"] 			= .type
		keywordMap["WITH"] 			= .null
		keywordMap["ARRAY"] 		= .array
		keywordMap["BEGIN"] 		= .begin
		keywordMap["CONST"] 		= .const
		keywordMap["ELSIF"] 		= .elsif
		keywordMap["IMPORT"] 		= .null
		keywordMap["UNTIL"] 		= .null
		keywordMap["WHILE"] 		= .`while`
		keywordMap["RECORD"] 		= .record
		keywordMap["REPEAT"] 		= .null
		keywordMap["RETURN"] 		= .null
		keywordMap["POINTER"] 		= .null
		keywordMap["PROCEDURE"]		= .procedure
		keywordMap["DIV"] 			= .div
		keywordMap["LOOP"] 			= .null
		keywordMap["MODULE"] 		= .module
		
		return keywordMap
	}
}

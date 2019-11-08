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
public struct Token: CustomStringConvertible
{
	public static func null(location: SourceLocation) -> Token {
		Token(.null, location: location)
	}
	public static func null() -> Token {
		return null(location: SourceLocation.none)
	}
	
	public private(set) var symbol: TokenType
	public private(set) var identifier: String
	public private(set) var value: Int
	public private(set) var sourceRange: SourceRange
	
	public var precedence: Int { return symbol.precedence }
	public var associativity: Associativity { return symbol.associativity }
	public var operatorGroup: OperatorGroup { return symbol.operatorGroup }
	
	// ---------------------------------------------------
	public init(
		_ symbol: TokenType,
		identifier: String = "",
		value: Int = 0,
		sourceRange: SourceRange)
	{
		self.symbol = symbol
		self.identifier = identifier
		self.value = value
		self.sourceRange = sourceRange
	}
	
	// ---------------------------------------------------
	public init(
		_ symbol: TokenType,
		identifier: String = "",
		value: Int = 0,
		location: SourceLocation)
	{
		self.init(
			symbol,
			identifier: identifier,
			value: value,
			sourceRange: location.rangeTo(location)
		)
	}
	
	// ---------------------------------------------------
	public var description: String
	{
		var result = "\(symbol)(rawValue: \(symbol.rawValue)"
		
		switch symbol
		{
			case .number: result += ", value: \(value)"
			case .identifier: result += ", identifier: \(identifier)"
			default: break
		}
		
		return result + ")"
	}
	
	// ---------------------------------------------------
	public var srcString: String
	{
		switch self.symbol
		{
			case .null: return "<<NULL>>"
			case .times: return "*"
			case .div: return "DIV"
			case .mod: return "MOD"
			case .and: return "&"
			case .plus: return "+"
			case .minus: return "-"
			case .or: return "OR"
			case .isEqualTo: return "="
			case .isNotEqualTo: return "#"
			case .lessThan: return "<"
			case .greaterThanOrEqualTo: return ">="
			case .lessThanOrEqualTo: return "<="
			case .greaterThan: return ">"
			case .period: return "."
			case .comma: return ","
			case .colon: return ":"
			case .closeParen: return ")"
			case .closeBracket: return "]"
			case .of: return "OF"
			case .then: return "THEN"
			case .do: return "DO"
			case .openParen: return "("
			case .openBracket: return "["
			case .not: return "~"
			case .becomes: return ":="
			case .number: return "\(value)"
			case .identifier: return identifier
			case .semicolon: return ";"
			case .end: return "END"
			case .else: return "ELSE"
			case .elsif: return "ELSIF"
			case .if: return "IF"
			case .while: return "WHILE"
			case .array: return "ARRAY"
			case .record: return "RECORD"
			case .const: return "CONST"
			case .type: return "TYPE"
			case .var: return "VAR"
			case .procedure: return "PROCEDURE"
			case .begin: return "BEGIN"
			case .module: return "MODULE"
			case .eof: return "<<EOF>>"
		}
	}
}

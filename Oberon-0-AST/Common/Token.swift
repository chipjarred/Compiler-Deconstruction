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
	
	public private(set) var symbol: TokenType
	public private(set) var identifier: String
	public private(set) var value: Int
	public private(set) var sourceRange: SourceRange
	
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
}

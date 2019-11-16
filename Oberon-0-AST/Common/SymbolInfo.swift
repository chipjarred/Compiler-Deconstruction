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
public final class SymbolInfo: Equatable
{
	// ---------------------------------------------------
	public enum Kind
	{
		case head
		case variable
		case parameter
		case constant
		case field
		case type
		case procedure
		case standardProcedure
		
		case register
		case condition
	}

	public var kind: Kind = .head
	public var level: Int = 0
	public var type: TypeInfo? = nil
	public var name = ""
	public var value: Int = 0
	public var sourceLocation: SourceLocation? = nil
	public weak var owningScope: SymbolScope? = nil
	public var ownedScope: SymbolScope? = nil

	// ---------------------------------------------------
	public final var isParameter: Bool {
		return (kind == .parameter) || kind == .variable && value > 0
	}
	
	// ---------------------------------------------------
	init(
		name: String = "",
		kind: Kind = .head,
		level: Int = 0,
		type: TypeInfo? = nil,
		value: Int = 0)
	{
		self.name = name
		self.kind = kind
		self.level = level
		self.type = type
		self.value = value
	}
	
	// ---------------------------------------------------
	public static func == (left: SymbolInfo, right: SymbolInfo) -> Bool
	{
		return left.kind == right.kind
			&& left.level == right.level
			&& left.name == right.name
			&& left.value == right.value
			&& left.type == right.type
	}
}

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

fileprivate let asciiRange: Range<UInt8> = 0..<0x7F

// ---------------------------------------------------
/**
Defines Oberon `CHAR` primitive type
*/
public struct CHAR:
	Comparable,
	DefaultInitializable,
	ExpressibleByExtendedGraphemeClusterLiteral,
	ExpressibleByIntegerLiteral
{
	public typealias IntegerLiteralType = UInt8
	public typealias ExtendedGraphemeClusterLiteralType =
		Character.ExtendedGraphemeClusterLiteralType
	
	public private(set) var ascii: UInt8
	
	// ---------------------------------------------------
	public init(_ value: UInt8)
	{
		precondition(
			asciiRange.contains(value),
			"CHAR, \(value), is not in the valid ASCII range, \(asciiRange)"
		)
		
		self.ascii = value
	}
	
	// ---------------------------------------------------
	public init(integerLiteral value: UInt8) {
		self.init(value)
	}
	
	// ---------------------------------------------------
	public init(_ value: Int)
	{
		if value > 0x7f || value < 0
		{
			fatalError( value < 0
				? "\(value) is negative"
				: "\(value) cannot be expressed in 8-bits"
			)
			
		}
		self.init(UInt8(value))
	}
	
	// ---------------------------------------------------
	public init(_ c: Character)
	{
		guard let ascii = c.asciiValue else {
			fatalError("\(c) is not a valid ASCII character")
		}
		
		self.init(ascii)
	}
	
	// ---------------------------------------------------
	public init(
		extendedGraphemeClusterLiteral c: ExtendedGraphemeClusterLiteralType)
	{
		self.init(c)
	}
	
	// ---------------------------------------------------
	public init() {
		self.init(0)
	}
	
	// MARK:- Comparable conformance
	// ---------------------------------------------------
	public static func < (left: CHAR, right: CHAR) -> Bool {
		return left.ascii < right.ascii
	}
	
	// MARK:- CHAR - INTEGER comparisons
	// ---------------------------------------------------
	public static func < (left: CHAR, right: INTEGER) -> Bool {
		return INTEGER(left.ascii) < right
	}

	// ---------------------------------------------------
	public static func <= (left: CHAR, right: INTEGER) -> Bool {
		return INTEGER(left.ascii) <= right
	}
	
	// ---------------------------------------------------
	public static func == (left: CHAR, right: INTEGER) -> Bool {
		return INTEGER(left.ascii) == right
	}
	
	// ---------------------------------------------------
	public static func != (left: CHAR, right: INTEGER) -> Bool {
		return INTEGER(left.ascii) != right
	}

	// ---------------------------------------------------
	public static func > (left: CHAR, right: INTEGER) -> Bool {
		return !(INTEGER(left.ascii) > right)
	}
	
	// ---------------------------------------------------
	public static func >= (left: CHAR, right: INTEGER) -> Bool {
		return INTEGER(left.ascii) >= right
	}

	// ---------------------------------------------------
	public static func < (left: INTEGER, right: CHAR) -> Bool {
		return left < INTEGER(right.ascii)
	}

	// ---------------------------------------------------
	public static func <= (left: INTEGER, right: CHAR) -> Bool {
		return left <= INTEGER(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func == (left: INTEGER, right: CHAR) -> Bool {
		return left == INTEGER(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func != (left: INTEGER, right: CHAR) -> Bool {
		return left != INTEGER(right.ascii)
	}

	// ---------------------------------------------------
	public static func > (left: INTEGER, right: CHAR) -> Bool {
		return left > INTEGER(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func >= (left: INTEGER, right: CHAR) -> Bool {
		return left >= INTEGER(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func + (left: CHAR, right: CHAR) -> CHAR
	{
		let sum = INTEGER(left.ascii) + INTEGER(right.ascii)
		guard sum <= 0x7f else
		{
			fatalError(
				"CHAR overflow, \(left) + \(right) = \(sum) is not valid ascii"
			)
		}
		
		return CHAR(sum)
	}
	
	// ---------------------------------------------------
	public static func - (left: CHAR, right: CHAR) -> CHAR
	{
		let sum = INTEGER(left.ascii) - INTEGER(right.ascii)
		guard sum >= 0 else
		{
			fatalError(
				"CHAR underfloat, \(left) - \(right) = \(sum) is not valid ascii"
			)
		}
		
		return CHAR(sum)
	}
}

// ---------------------------------------------------
extension CHAR: CustomStringConvertible, CustomDebugStringConvertible
{
	// ---------------------------------------------------
	public var description: String {
		return "\(Character(Unicode.Scalar(ascii)))"
	}
	
	// ---------------------------------------------------
	public var debugDescription: String {
		return description
	}
	
}

//
//  CHAR.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

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
	
	// MARK:- CHAR - Int comparisons
	// ---------------------------------------------------
	public static func < (left: CHAR, right: Int) -> Bool {
		return Int(left.ascii) < right
	}

	// ---------------------------------------------------
	public static func <= (left: CHAR, right: Int) -> Bool {
		return Int(left.ascii) <= right
	}
	
	// ---------------------------------------------------
	public static func == (left: CHAR, right: Int) -> Bool {
		return Int(left.ascii) == right
	}
	
	// ---------------------------------------------------
	public static func != (left: CHAR, right: Int) -> Bool {
		return Int(left.ascii) != right
	}

	// ---------------------------------------------------
	public static func > (left: CHAR, right: Int) -> Bool {
		return !(Int(left.ascii) > right)
	}
	
	// ---------------------------------------------------
	public static func >= (left: CHAR, right: Int) -> Bool {
		return Int(left.ascii) >= right
	}

	// ---------------------------------------------------
	public static func < (left: Int, right: CHAR) -> Bool {
		return left < Int(right.ascii)
	}

	// ---------------------------------------------------
	public static func <= (left: Int, right: CHAR) -> Bool {
		return left <= Int(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func == (left: Int, right: CHAR) -> Bool {
		return left == Int(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func != (left: Int, right: CHAR) -> Bool {
		return left != Int(right.ascii)
	}

	// ---------------------------------------------------
	public static func > (left: Int, right: CHAR) -> Bool {
		return left > Int(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func >= (left: Int, right: CHAR) -> Bool {
		return left >= Int(right.ascii)
	}
	
	// ---------------------------------------------------
	public static func + (left: CHAR, right: CHAR) -> CHAR
	{
		let sum = Int(left.ascii) + Int(right.ascii)
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
		let sum = Int(left.ascii) - Int(right.ascii)
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

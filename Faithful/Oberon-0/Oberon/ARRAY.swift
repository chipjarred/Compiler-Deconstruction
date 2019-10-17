//
//  ARRAY.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

// ---------------------------------------------------
/**
Defines a fixed sized array for use in faithfully translated Oberon code
*/
public struct ARRAY<T:DefaultInitializable>
{
	public typealias Element = T
	private var array: [Element]
	
	public var count: INTEGER { return INTEGER(array.count) }
	
	// ---------------------------------------------------
	public subscript (index: Int) -> Element
	{
		// ---------------------------------------------------
		get
		{
			precondition(
				array.indices.contains(index),
				"Index, \(index), out of bounds"
			)
			
			return array[index]
		}
		
		// ---------------------------------------------------
		set
		{
			precondition(
				array.indices.contains(index),
				"Index, \(index), out of bounds"
			)
			
			array[index] = newValue
		}
	}
	
	// ---------------------------------------------------
	public init(count: Int) {
		self.array = [Element](repeating: T(), count: count)
	}
	
	// ---------------------------------------------------
	public init(_ array: [Element]) {
		self.array = array
	}
}

// MARK:- ARRAY of CHAR
// ---------------------------------------------------
extension ARRAY: CustomStringConvertible, CustomDebugStringConvertible
	where Element == CHAR
{
	public var description: String
	{
		var string = ""
		for c in array
		{
			guard c != 0 else { break }
			string.append(c.description)
		}
		
		return string
	}
	
	public var debugDescription: String {
		return description
	}
	
}

// ---------------------------------------------------
extension ARRAY: ExpressibleByUnicodeScalarLiteral where Element == CHAR
{
	public typealias UnicodeScalarLiteralType = CHAR.UnicodeScalarLiteralType
	
	// ---------------------------------------------------
	public init(unicodeScalarLiteral value: CHAR.UnicodeScalarLiteralType) {
		self.init([CHAR(value)])
	}
}

// ---------------------------------------------------
extension ARRAY: ExpressibleByExtendedGraphemeClusterLiteral
	where Element == CHAR
{
	public typealias ExtendedGraphemeClusterLiteralType =
		CHAR.ExtendedGraphemeClusterLiteralType

	// ---------------------------------------------------
	public init(
		extendedGraphemeClusterLiteral c: ExtendedGraphemeClusterLiteralType)
	{
		self.init([CHAR(c)])
	}
}

// ---------------------------------------------------
extension ARRAY: ExpressibleByStringLiteral where Element == CHAR
{
	public typealias StringLiteralType = String
	
	// ---------------------------------------------------
	public init(stringLiteral value: String)
	{
		var array = [Element]()
		
		array.reserveCapacity(value.count)
		for c in value {
			array.append(Element(c))
		}
		
		array.append(Element())
		
		self.init(array)
	}
}

// ---------------------------------------------------
extension ARRAY: Comparable, Equatable where Element == CHAR
{
	// ---------------------------------------------------
	private static func cstrcmp(_ left: ARRAY, _ right: ARRAY) -> Int
	{
		let maxIndex = min(left.count, right.count)

		for i in 0..<maxIndex
		{
			let cLeft = left[i]
			let cRight = right[i]

			if cLeft == cRight
			{
				if cLeft == 0 {
					return 0
				}
				continue
			}

			return cLeft < cRight ? -1 : 1
		}

		return right.count - left.count
	}
	
	// ---------------------------------------------------
	public static func == (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) == 0
	}
	
	// ---------------------------------------------------
	public static func != (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) != 0
	}
	
	// ---------------------------------------------------
	public static func < (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) < 0
	}
	
	// ---------------------------------------------------
	public static func <= (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) <= 0
	}
	
	// ---------------------------------------------------
	public static func > (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) > 0
	}
	
	// ---------------------------------------------------
	public static func >= (left: ARRAY, right: ARRAY) -> Bool {
		return cstrcmp(left, right) >= 0
	}
}

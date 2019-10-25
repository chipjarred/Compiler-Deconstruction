//
//  ARRAY.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
/**
Defines a fixed sized array for use in faithfully translated Oberon code
*/
public struct ARRAY<T:DefaultInitializable>
{
	public typealias Element = T
	private var array: [Element]
	
	public var count: Int { return Int(array.count) }
	public var startIndex: Int { return 0 }
	public var endIndex: Int { return count }
	public var indices: Range<Int> { return 0..<count }
	public var capacity: Int { return Int(array.capacity) }
	
	// ---------------------------------------------------
	public var data: Data
	{
		var data = Data()
		array.withUnsafeBufferPointer { data.append($0) }
		return data
	}
			
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
	public subscript(index: Int32) -> Element
	{
		get { return self[Int(index)] }
		set { self[Int(index)] = newValue }
	}
	
	// ---------------------------------------------------
	public subscript(index: UInt32) -> Element
	{
		get { return self[Int(index)] }
		set { self[Int(index)] = newValue }
	}

	// ---------------------------------------------------
	public init(repeating value: Element = Element(), count: Int) {
		self.array = [Element](repeating: value, count: count)
	}

	// ---------------------------------------------------
	public init(_ array: [Element]) {
		self.array = array
	}
	
	// ---------------------------------------------------
	public init(contentsOf data: Data)
	{
		assert(data.count % MemoryLayout<Element>.size == 0)
		
		self.init(
			data.withUnsafeBytes {
				return [Element]($0.bindMemory(to: Element.self))
			}
		)
	}
	
	// ---------------------------------------------------
	public mutating func reserveCapacity(_ capacity: Int) {
		array.reserveCapacity(capacity)
	}
	
	// ---------------------------------------------------
	public mutating func append(_ newElement: Element) {
		array.append(newElement)
	}
	
	// ---------------------------------------------------
	public mutating func append(contentsOf newElements: ARRAY<Element>) {
		array.append(contentsOf: newElements.array)
	}
	
	// ---------------------------------------------------
	public mutating func append(contentsOf newElements: [Element]) {
		array.append(contentsOf: newElements)
	}

	// ---------------------------------------------------
	public mutating func append<S: Sequence>(contentsOf newElements: S)
		where S.Element == Element
	{
		array.append(contentsOf: newElements)
	}
	
	// ---------------------------------------------------
	public mutating func removeFirst(_ k: Int) {
		array.removeFirst(k)
	}
}

// MARK:- Sequence conformance
// ---------------------------------------------------
extension ARRAY: Sequence
{
	// ---------------------------------------------------
	public struct Iterator: IteratorProtocol
	{
		public typealias Element = ARRAY.Element
		private let array: [Element]
		private var index: Int
		private let endIndex: Int
		
		// ---------------------------------------------------
		internal init(
			array: [Element],
			startIndex: Int = 0,
			endIndex: Int? = nil)
		{
			assert(array.indices.contains(startIndex))
			assert(endIndex ?? 0 >= startIndex)
			assert(endIndex ?? 0 <= array.count)
			
			self.array = array
			self.index = startIndex
			self.endIndex = endIndex ?? array.count
		}
		
		// ---------------------------------------------------
		public mutating func next() -> Element?
		{
			guard index < endIndex else { return nil }
			
			defer { index += 1 }
			return array[index]
		}
	}
	
	// ---------------------------------------------------
	public func makeIterator() -> Iterator {
		return Iterator(array: self.array)
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
		let maxIndex = Swift.min(left.count, right.count)

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

// ---------------------------------------------------
public extension Array
{
	// ---------------------------------------------------
	subscript(index: Int32) -> Element
	{
		get { return self[Int(index)] }
		set { self[Int(index)] = newValue }
	}
	// ---------------------------------------------------
	subscript(index: UInt32) -> Element
	{
		get { return self[Int(index)] }
		set { self[Int(index)] = newValue }
	}
}


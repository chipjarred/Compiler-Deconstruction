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
/**
Defines a fixed sized array for use in faithfully translated Oberon code
*/
public struct ARRAY<T:DefaultInitializable>
{
	public typealias Element = T
	private var array: [Element]
	
	public var count: INTEGER { return INTEGER(array.count) }
	public var startIndex: INTEGER { return 0 }
	public var endIndex: INTEGER { return count }
	public var indices: Range<INTEGER> { return 0..<count }
	public var capacity: INTEGER { return INTEGER(array.capacity) }
	
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
	public subscript(index: LONGINT) -> Element
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
	public init(repeating value: Element = Element(), count: LONGINT) {
		self.init(repeating: value, count: Int(count))
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
	public mutating func reserveCapacity(_ capacity: LONGINT) {
		array.reserveCapacity(Int(capacity))
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
		private var index: INTEGER
		private let endIndex: INTEGER
		
		// ---------------------------------------------------
		internal init(
			array: [Element],
			startIndex: INTEGER = 0,
			endIndex: INTEGER? = nil)
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
	subscript(index: LONGINT) -> Element
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


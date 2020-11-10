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

import Foundation

// ---------------------------------------------------
/**
Represents a location in a source file
*/
public struct SourceLocation: Comparable
{
	public static let none =
		SourceLocation(name: "<<unset>>", offset: -1, line: -1, column: -1)
	
	/// Source file name
	public let name: String
	
	/// Offset from the start of the file, in bytes.
	public let offset: Int
	
	/// Number of new lines encountered.
	public let line: Int
	
	/// Number of characters from the beginning of the line.
	public let column: Int
	
	// ---------------------------------------------------
	public static func < (left: SourceLocation, right: SourceLocation) -> Bool {
		return left.offset < right.offset
	}
	
	// ---------------------------------------------------
	public func rangeTo(_ endLocation: SourceLocation) -> SourceRange
	{
		assert(name == endLocation.name)
		assert(offset <= endLocation.offset)
		assert(line <= endLocation.line)
		
		return SourceRange(start: self, end: endLocation)
	}
}

// ---------------------------------------------------
/**
Represents a contiguous range of locations in a source file
*/
public struct SourceRange
{
	/// Source file name
	public let name: String
	
	public let offsets: Range<Int>
	public let lines: Range<Int>
	public let startColumn: Int
	public let endColumn: Int
	
	// ---------------------------------------------------
	var lowerBound: SourceLocation
	{
		return SourceLocation(
			name: name,
			offset: offsets.lowerBound,
			line: lines.lowerBound,
			column: startColumn
		)
	}
	
	// ---------------------------------------------------
	var upperBound: SourceLocation
	{
		return SourceLocation(
			name: name,
			offset: offsets.upperBound,
			line: lines.upperBound,
			column: endColumn
		)
	}

	// ---------------------------------------------------
	private init(
		name: String,
		offsets: Range<Int>,
		lines: Range<Int>,
		startColumn: Int,
		endColumn: Int)
	{
		self.name = name
		self.offsets = offsets
		self.lines = lines
		self.startColumn = startColumn
		self.endColumn = endColumn
	}
	
	// ---------------------------------------------------
	public init(start: SourceLocation, end: SourceLocation)
	{
		assert(start.name == end.name)
		assert(start.offset <= end.offset)
		assert(start.line <= end.line)
		
		self.init(
			name: start.name,
			offsets: start.offset..<end.offset,
			lines: start.line..<end.line,
			startColumn: start.column,
			endColumn: end.column
		)
	}
}

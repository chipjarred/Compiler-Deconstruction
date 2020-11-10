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
public class ErrorReporter
{
	public private(set) var errorCount: Int = 0
	private var position = SourceLocation.none
	private var outStream: OutputStream
	
	// ---------------------------------------------------
	public init?(_ fileHandle: FileHandle)
	{
		guard let outStream = fileHandle.textOutputStream else { return nil }
		self.outStream = outStream
	}
	
	// ---------------------------------------------------
	public init(_ outputStream: OutputStream) {
		self.outStream = outputStream
	}
	
	// ---------------------------------------------------
	public func emit(_ msg: String)
	{
		print(msg, to: &outStream)
		errorCount += 1
	}

	// ---------------------------------------------------
	public final func mark(
		_ msg: String,
		at location: SourceLocation,
		annotatedWith annotation: String? = nil,
		at noteLocation: SourceLocation? = nil)
	{
		if location.name != position.name
			|| location.offset > position.offset
			|| errorCount == 0
		{
			emit(
				"file: \(location.name), "
				+ "line: \(location.line + 1), "
				+ "col: \(location.column + 1): \(msg)"
			)
			
			annotate(with: annotation, at: noteLocation)
		}
		else { errorCount += 1 }
		
		position = location
	}
	
	// ---------------------------------------------------
	private func annotate(
		with annotation: String?,
		at noteLocation: SourceLocation?)
	{
		// Allows attaching additional information that refers to another
		// line of code (such as referring to the existing defintion of a
		// a symbol for duplicate definition errors), without further
		// changing the error count or position.
		if let note = annotation
		{
			if let noteLoc = noteLocation
			{
				print(
					"file: \(noteLoc.name), "
					+ "line: \(noteLoc.line + 1), "
					+ "col: \(noteLoc.column + 1): \(note)",
					to: &outStream
				)
			}
			else { print(note, to: &outStream) }
		}
	}
	
	// ---------------------------------------------------
	public final func mark(
		_ msg: String,
		at location: SourceLocation?,
		annotatedWith annotation: String? = nil,
		at noteLocation: SourceLocation? = nil)
	{
		mark(
			msg,
			at: location ?? position,
			annotatedWith: annotation,
			at: noteLocation
		)
	}
	
	// ---------------------------------------------------
	public final func mark(
		_ msg: String,
		annotatedWith annotation: String? = nil,
		at noteLocation: SourceLocation? = nil)
	{
		mark(msg, at: position, annotatedWith: annotation, at: noteLocation)
	}
}

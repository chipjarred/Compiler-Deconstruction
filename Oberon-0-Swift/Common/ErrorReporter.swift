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
public class ErrorReporter
{
	public private(set) var errorCount: Int = 0
	private var position = 0
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
	public final func mark(_ msg: String, at location: Int)
	{
		if location > position {
			emit("file: \(location): \(msg)")
		}
		else { errorCount += 1 }
		
		position = location
	}
}

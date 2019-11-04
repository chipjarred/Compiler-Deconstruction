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
public struct UTF8CharacterReader
{
	public typealias Position = Int
	
	private var inputStream: InputStream
	
	public private(set) var offset: Int = 0
	public private(set) var line: Int = 0
	public private(set) var col: Int = 0
	
	public private(set) var endOfInput: Bool = false
	
	// ---------------------------------------------------
	public var position: Position { return offset }
	
	// ---------------------------------------------------
	public init()
	{
		let stream = FileInputStream(fileHandle: FileHandle.standardInput)!
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public init(inputStream: InputStream) {
		self.inputStream = inputStream
	}
	
	// ---------------------------------------------------
	public init?(fileHandle: FileHandle)
	{
		guard let stream = FileInputStream(fileHandle: fileHandle) else {
			return nil
		}
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public init(contentsOf string: String)
	{
		let stream = InputStream(contentsOf: string, encoding: .utf8)
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public mutating func readCharacter(into ch: inout Character) -> Bool
	{
		guard let c = readCharacter() else
		{
			endOfInput = true
			return false
		}
		
		ch = c
		return true
	}
	
	// ---------------------------------------------------
	private var lastChar: Character = "\0"
	public mutating func readCharacter() -> Character?
	{
		guard !endOfInput, let c = inputStream.readUTF8Character() else
		{
			endOfInput = true
			return nil
		}
		
		offset += 1
		if lastChar == "\n" {
			col = 0
			line += 1
		}
		else {
			col += 1
		}
		
		lastChar = c
		return c
	}
}

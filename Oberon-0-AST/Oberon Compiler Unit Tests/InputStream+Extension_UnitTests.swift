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

import XCTest

// ---------------------------------------------------
class InputStream_Extension_UnitTests: XCTestCase
{
	// ---------------------------------------------------
    func test_readUTF8Character_reads_ASCII()
	{
		func buildASCII() -> [UInt8]
		{
			var array = [UInt8](capacity: 127)

			for i:UInt8 in 0..<127 {
				array.append(i)
			}
			
			return array
		}
		
		let ascii = buildASCII()
		let inputStream = InputStream(data: Data(ascii))
		inputStream.open()
		
		var counter = 0
		for asciiCode in ascii
		{
			guard let readC = inputStream.readUTF8Character() else
			{
				XCTFail(
					"Couldn't read character corresponding to asciiCode, "
					+ "\(asciiCode)"
				)
				
				return
			}
			
			let expectedC = Character(Unicode.Scalar(asciiCode))
			XCTAssertEqual(readC, expectedC)
			
			counter += 1
		}
		
		XCTAssertEqual(counter, 127)
    }
	
	// ---------------------------------------------------
	func test_readUTF8Character_reads_multibyte_characters()
	{
		let testStr = "δΔስ㗌賱ꉏ쫳阮ﴚＱ𐂡𝚽𝒲"
		let testChars:[Character] = {
			var a = [Character]()
			for c in testStr { a.append(c) }
			return a
		}()
		let data = testStr.data(using: .utf8)!
		
		let inputStream = InputStream(data: data)
		inputStream.open()
		
		var counter = 0
		for expectedC in testStr
		{
			guard let readC = inputStream.readUTF8Character() else
			{
				XCTFail(
					"Couldn't read character corresponding to unicode "
					+ "character, \"\(expectedC)\""
				)
				
				return
			}
			
			XCTAssertEqual(readC, expectedC)
			
			counter += 1
		}
		
		XCTAssertEqual(counter, testChars.count)
	}
}

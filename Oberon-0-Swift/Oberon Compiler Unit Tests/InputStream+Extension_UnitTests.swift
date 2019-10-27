//
//  InputStream+Extension_UnitTests.swift
//  Oberon Compiler Unit Tests
//
//  Created by Chip Jarred on 10/26/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import XCTest

// ---------------------------------------------------
class InputStream_Extension_UnitTests: XCTestCase
{
	// ---------------------------------------------------
    func test_readUTF8Character_reads_ASCII()
	{
		func buildASCII() -> [UInt8]
		{
			var array = [UInt8]()
			array.reserveCapacity(127)

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

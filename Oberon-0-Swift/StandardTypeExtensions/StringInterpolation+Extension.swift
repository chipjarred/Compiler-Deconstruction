//
//  StringInterpolation+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

fileprivate let lowerHexDigits = [Character]("0123456789abcedf")
fileprivate let upperHexDigits = [Character]("0123456789abcedf")

// ---------------------------------------------------
public extension String.StringInterpolation
{
	// ---------------------------------------------------
	mutating func appendInterpolation<T:FixedWidthInteger>(hex value: T) {
		appendInterpolation(hex: value, with: lowerHexDigits)
    }
	// ---------------------------------------------------
	mutating func appendInterpolation<T:FixedWidthInteger>(HEX value: T) {
		appendInterpolation(hex: value, with: upperHexDigits)
    }
	
	// ---------------------------------------------------
	fileprivate mutating func appendInterpolation<T:FixedWidthInteger>(
		hex value: T,
		with hexDigits: [Character])
	{
		var hexStr = ""
		
		var value = value
		for _ in (0..<MemoryLayout<UInt32>.size * 2)
		{
			let nibble = Int(value & 0x0f)
			hexStr.append(hexDigits[nibble])
			value >>= 4
		}
		
		appendLiteral(String(hexStr.reversed()))
	}
}

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
	mutating func appendInterpolation<T:FixedWidthInteger>(
		hex value: T,
		pad: Int = 0)
	{
		appendInterpolation(hex: value, with: lowerHexDigits, pad: pad)
    }
	
	// ---------------------------------------------------
	mutating func appendInterpolation<T:FixedWidthInteger>(
		HEX value: T,
		pad: Int = 0)
	{
		appendInterpolation(hex: value, with: upperHexDigits, pad: pad)
    }
	
	// ---------------------------------------------------
	mutating func appendInterpolation<T:FixedWidthInteger>(_ value: T, pad: Int)
	{
		appendInterpolation("\(value)", leftPad: pad)
    }
	
	// ---------------------------------------------------
	mutating func appendInterpolation(_ s: String, leftPad pad: Int)
	{
		let paddingLength = pad - s.count
		if paddingLength > 0
		{
			var padding = ""
			
			for _ in 1..<paddingLength {
				padding += " "
			}
			padding.reserveCapacity(padding.count + s.count)
			appendLiteral(padding)
		}
		
		appendLiteral(s)
	}
	

	// ---------------------------------------------------
	fileprivate mutating func appendInterpolation<T:FixedWidthInteger>(
		hex value: T,
		with hexDigits: [Character],
		pad: Int)
	{
		var hexStr = ""
		
		var value = value
		for _ in (0..<MemoryLayout<UInt32>.size * 2)
		{
			let nibble = Int(value & 0x0f)
			hexStr.append(hexDigits[nibble])
			value >>= 4
		}
		
		appendInterpolation(String(hexStr.reversed()), leftPad: pad)
	}
}

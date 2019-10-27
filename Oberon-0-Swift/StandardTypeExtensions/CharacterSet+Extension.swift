//
//  CharacterSet+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public extension CharacterSet
{
	// ---------------------------------------------------
	init(asciiRange: ClosedRange<Character>)
	{
		self.init()
		self.insert(asciiRange: asciiRange)
	}
	
	// ---------------------------------------------------
	func contains(_ c: Character) -> Bool
	{
		let unicode = c.unicodeScalars
		if unicode.count > 1 {
			return false
		}
		return self.contains(unicode.first!)
	}
	
	// ---------------------------------------------------
	mutating func insert(asciiRange: ClosedRange<UInt8>)
	{
		assert(asciiRange.lowerBound < 0x80)
		assert(asciiRange.upperBound < 0x80)
		
		for i in asciiRange {
			self.insert(Unicode.Scalar(i))
		}
	}
	
	// ---------------------------------------------------
	mutating func insert(asciiRange: ClosedRange<Character>)
	{
		guard let lower = asciiRange.lowerBound.asciiValue,
			let upper = asciiRange.upperBound.asciiValue
		else
		{
			fatalError("\(asciiRange) is not a valid ASCII range")
		}
		
		self.insert(asciiRange: lower...upper)
	}
}

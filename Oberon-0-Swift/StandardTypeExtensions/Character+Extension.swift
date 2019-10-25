//
//  Character+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
extension Character
{
	// ---------------------------------------------------
	init(ascii: Int)
	{
		assert((0..<0x80).contains(ascii))
		self.init(Unicode.Scalar(ascii)!)
	}
	
	// ---------------------------------------------------
	init<T: FixedWidthInteger>(ascii: T)
	{
		assert((0..<0x80).contains(ascii))
		self.init(ascii: Int(ascii))
	}
}

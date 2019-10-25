//
//  String+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public extension String
{
	// ---------------------------------------------------
	#warning("Remove after conversion")
	static func == (left: String, right: [CHAR]) -> Bool
	{
		var s = ""
		s.reserveCapacity(right.count)
		
		for c in right
		{
			if c != 0 {
				s.append(c.character)
			}
		}
		
		return left == s
	}
	
	// ---------------------------------------------------
	#warning("Remove after conversion")
	static func == (left: [CHAR], right: String) -> Bool {
		return right == left
	}
	
	// ---------------------------------------------------
	subscript(index: Int) -> Character
	{
		get
		{
			let strIndex = self.index(self.startIndex, offsetBy: index)
			return self[strIndex]
		}
	}
}

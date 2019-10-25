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
	func contains(_ c: Character) -> Bool
	{
		let unicode = c.unicodeScalars
		if unicode.count > 1 {
			return false
		}
		return self.contains(unicode.first!)
	}
}

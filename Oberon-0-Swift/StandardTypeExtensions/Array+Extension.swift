//
//  Array+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public extension Array where Element == Int32
{
	// ---------------------------------------------------
	subscript<T: FixedWidthInteger>(index: T) -> Element
	{
		// ---------------------------------------------------
		get
		{
			assert(
				indices.contains(Int(index)),
				"index, \(index), out of range, \(startIndex)..<\(endIndex)"
			)
			return self[Int(index)]
		}
		// ---------------------------------------------------
		set
		{
			assert(
				indices.contains(Int(index)),
				"index, \(index), out of range, \(startIndex)..<\(endIndex)"
			)
			self[Int(index)] = newValue
		}
	}
}

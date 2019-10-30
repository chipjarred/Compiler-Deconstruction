//
//  TypeInfo.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/29/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public class TypeInfo: Equatable
{
	public var form: Int = 0
	public var fields: SymbolTable.ListNode? = nil
	public var base: TypeInfo? = nil
	public var size: Int = 0
	public var len: Int = 0
	
	// ---------------------------------------------------
	public init() { }

	// ---------------------------------------------------
	public init(form: Int, size: Int)
	{
		self.form = form
		self.size = size
	}
	
	// ---------------------------------------------------
	public static func == (left: TypeInfo, right: TypeInfo) -> Bool
	{
		return left.form == right.form
			&& left.base == right.base
			&& left.size == right.size
			&& left.len == right.len
			&& left.fields == right.fields
	}
}

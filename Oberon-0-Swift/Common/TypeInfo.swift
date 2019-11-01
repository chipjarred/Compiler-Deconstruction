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
	// ---------------------------------------------------
	public enum Form: Int, Comparable, CustomStringConvertible
	{
		case boolean = 0
		case integer = 1
		case array = 2
		case record = 3
		
		// ---------------------------------------------------
		public static func < (lhs: Form, rhs: Form) -> Bool {
			return lhs.rawValue < rhs.rawValue
		}
		
		// ---------------------------------------------------
		public var description: String
		{
			switch self
			{
				case .boolean: return "Boolean"
				case .integer: return "Integer"
				case .array: return "Array"
				case .record: return "Record"
			}
		}
	}
	
	public var form: Form = .boolean
	public var fields: [SymbolInfo] = []
	public var base: TypeInfo? = nil
	public var size: Int = 0
	public var len: Int = 0
	
	// ---------------------------------------------------
	public init() { }
	
	// ---------------------------------------------------
	public func symbolInfoForField(named name: String) -> SymbolInfo?
	{
		for curField in fields
		{
			if curField.name == name {
				return curField
			}
		}
		
		return nil
	}
	
	// ---------------------------------------------------
	public init(form: Form, size: Int)
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

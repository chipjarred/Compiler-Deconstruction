// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

// ---------------------------------------------------
public class TypeInfo: Equatable, CustomStringConvertible
{
	// ---------------------------------------------------
	public enum Form: CustomStringConvertible
	{
		case void
		case boolean
		case integer
		case array
		case record
		case procedure
		
		// ---------------------------------------------------
		public var description: String
		{
			switch self
			{
				case .void: return "Void"
				case .boolean: return "Boolean"
				case .integer: return "Integer"
				case .array: return "Array"
				case .record: return "Record"
				case .procedure: return "Procedure"
			}
		}
	}
	
	public static let void: TypeInfo = TypeInfo(form: .void, size: 0)
	public static let integer: TypeInfo = TypeInfo(form: .integer, size: 4)
	public static let boolean: TypeInfo = TypeInfo(form: .boolean, size: 4)

	public var form: Form = .void
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
	public func addField(named name: String, type fieldType: TypeInfo)
	{
		addField(
			from: SymbolInfo(
				name: name,
				kind: .field,
				type: fieldType
			)
		)
	}
	
	// ---------------------------------------------------
	public func addField(from fieldInfo: SymbolInfo)
	{
		assert(form == .record || form == .procedure)
		assert(fieldInfo.type != nil)
		
		fields.append(fieldInfo)
		size += fieldInfo.type?.size ?? 0
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
	
	// ---------------------------------------------------
	public var description: String
	{
		switch form
		{
			case .void: return ""
			case .boolean: return "BOOLEAN"
			case .integer: return "INTEGER"
			case .array:
				return "ARRAY \(len) OF \(base!)"
			case .record:
				return "RECORD \(fieldDescription) END"
			case .procedure:
				let returnType = (base ?? TypeInfo.void) == TypeInfo.void
					? ""
					: ": \(base!)"
				return "(fieldDescription)" + returnType
		}
	}
	
	// ---------------------------------------------------
	private var fieldDescription: String
	{
		var str = ""
		
		if fields.count > 0
		{
			var curField = fields[0]
			
			if curField.kind == .parameter {
				str += "VAR "
			}
			str += "\(curField.name): \(curField.type!)"
			
			for i in 1..<fields.count
			{
				curField = fields[i]
				str += curField.kind == .parameter ? "; VAR " : "; "
				str += "\(curField.name): \(curField.type!)"
			}
		}
		
		return str
	}
}

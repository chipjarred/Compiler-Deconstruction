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
public class TypeInfo: Equatable
{
	// ---------------------------------------------------
	public enum Form: CustomStringConvertible
	{
		case boolean
		case integer
		case array
		case record
		
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

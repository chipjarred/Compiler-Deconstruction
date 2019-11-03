//
//  CodeGenError.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 11/3/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public enum CodeGenError: Swift.Error, CustomStringConvertible
{
	public typealias Item = RISCCodeGenerator.Item
	
	case indexNotInteger
	case indexOutOfRange(index: Int, range: ClosedRange<Int>)
	case valueTooLarge(value: Int, range: ClosedRange<Int>)
	case wrongForm(expected: TypeInfo.Form, got: TypeInfo.Form?)
	case localOrGlobalOnly
	case expectedArithmeticBinaryOperator(got: TokenType)
	case expectedLogicalBinaryOperator(got: TokenType)
	case incompatibleTypes(_: TypeInfo.Form, _: TypeInfo.Form)
	case illegalAssignment
	case incompatibleAssignment(src: TypeInfo.Form, dst: TypeInfo.Form)
	case incompatbleActualParameter
	case illegalParameterMode(_ mode: Item.Mode)

	// ---------------------------------------------------
	public var localizedDescription: String {
		switch self
		{
			case .indexNotInteger:
				return "Array index is not an integer"
			
			case let .indexOutOfRange(index, range):
				return "Array index, \(index), is out bounds,"
						+ " \(range.lowerBound)...\(range.upperBound)"
			
			case let .valueTooLarge(value, range):
				return "Value, \(value), is not in the range, "
					+ " \(range.lowerBound)...\(range.upperBound)"
		
			case let .wrongForm(expected, actual):
				return "Expected \(aOrAn: expected), but got "
					+ "\(aOrAn: actual) instead."
			
			case .localOrGlobalOnly:
				return "Attempt to reference identifier at a scope that is "
					+ "neither local nor global is not currently supported."
			
			case let .expectedArithmeticBinaryOperator(tokenType):
				return "Expected an arithmetic operator, but got "
					+ "\(tokenType) instead."
			
			case let .expectedLogicalBinaryOperator(tokenType):
				return "Expected an logical operator, but got "
					+ "\(tokenType) instead."
			
			case let .incompatibleTypes(a, b):
				return "Incompatible types, \(a) and \(b)."
			
			case .illegalAssignment:
				return "Illegal assignment."
			
			case let .incompatibleAssignment(src, dst):
				return "Cannot assign value of type, \(src), to variable "
					+ "of type, \(dst)."
			
			case .incompatbleActualParameter:
				return "Actual parameter type does not match formal "
					+ "parameter type."
			
			case let .illegalParameterMode(mode):
				return "Illegal parameter mode, \(mode)."
		}
	}
	
	// ---------------------------------------------------
	public var description: String {
		return localizedDescription
	}
}

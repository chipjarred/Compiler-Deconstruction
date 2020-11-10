// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
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
public enum CodeGenError: Swift.Error, CustomStringConvertible
{
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
    case illegalParameterMode(_ mode: RISCOperand.Mode)

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

// MARK:- Support extensions

fileprivate let upperCasedVowels = CharacterSet(charactersIn: "AEIOU")
// ---------------------------------------------------
fileprivate extension Character {
    var isVowel: Bool { upperCasedVowels.contains(self) }
}

// ---------------------------------------------------
fileprivate extension String.StringInterpolation
{
    // ---------------------------------------------------
    mutating func appendInterpolation(aOrAn form: TypeInfo.Form?)
    {
        let str = " " + (form?.description ?? "nil")
        
        guard str.count > 0 else {
            appendLiteral(str)
            return
        }
        
        let article = str.first!.isVowel ? "an" : "a"
        
        appendLiteral(article + str)
    }
}

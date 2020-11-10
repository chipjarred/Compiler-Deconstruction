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

fileprivate let lowerHexDigits = [Character]("0123456789abcedf")
fileprivate let upperHexDigits = [Character]("0123456789ABCDEF")

// ---------------------------------------------------
public extension String.StringInterpolation
{
    // ---------------------------------------------------
    mutating func appendInterpolation<T:FixedWidthInteger>(
        hex value: T,
        pad: Int = 0)
    {
        appendInterpolation(hex: value, with: lowerHexDigits, pad: pad)
    }
    
    // ---------------------------------------------------
    mutating func appendInterpolation<T:FixedWidthInteger>(
        HEX value: T,
        pad: Int = 0)
    {
        appendInterpolation(hex: value, with: upperHexDigits, pad: pad)
    }
    
    // ---------------------------------------------------
    mutating func appendInterpolation<T:FixedWidthInteger>(_ value: T, pad: Int)
    {
        appendInterpolation("\(value)", leftPad: pad)
    }
    
    // ---------------------------------------------------
    mutating func appendInterpolation(_ s: String, leftPad pad: Int)
    {
        let paddingLength = pad - s.count
        if paddingLength > 0
        {
            var padding = ""
            
            for _ in 1..<paddingLength {
                padding += " "
            }
            padding.reserveCapacity(padding.count + s.count)
            appendLiteral(padding)
        }
        
        appendLiteral(s)
    }
    
    // ----------------------------------
    enum ListStringConjuntion
    {
        case none
        case or
        case and
    }
    
    // ---------------------------------------------------
    mutating func appendInterpolation<T>(
        list elements: [T],
        delimitedBy delimiter: String = ", ",
        _ conjunction: ListStringConjuntion = .none)
    {
        guard elements.count > 0 else { return }
        
        var str = ""
        if elements.count > 1
        {
            for i in 0..<(elements.count - 1) {
                str.append("\"\(elements[i])\"\(delimiter) ")
            }
        }
        
        switch conjunction
        {
            case .none: break
            case .or: str.append("or ")
            case .and: str.append("and ")
        }
        
        str.append("\"\(elements.last!)\"")
        
        appendLiteral(str)
    }

    // ---------------------------------------------------
    fileprivate mutating func appendInterpolation<T:FixedWidthInteger>(
        hex value: T,
        with hexDigits: [Character],
        pad: Int)
    {
        var hexStr = ""
        
        var value = value
        for _ in (0..<MemoryLayout<T>.size * 2)
        {
            let nibble = Int(value & 0x0f)
            hexStr.append(hexDigits[nibble])
            value >>= 4
        }
        
        appendInterpolation(String(hexStr.reversed()), leftPad: pad)
    }
}

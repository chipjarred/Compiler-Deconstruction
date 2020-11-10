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
public extension CharacterSet
{
    // ---------------------------------------------------
    init(asciiRange: ClosedRange<Character>)
    {
        self.init()
        self.insert(asciiRange: asciiRange)
    }
    
    // ---------------------------------------------------
    func contains(_ c: Character) -> Bool
    {
        let unicode = c.unicodeScalars
        if unicode.count > 1 {
            return false
        }
        return self.contains(unicode.first!)
    }
    
    // ---------------------------------------------------
    mutating func insert(asciiRange: ClosedRange<UInt8>)
    {
        assert(asciiRange.lowerBound < 0x80)
        assert(asciiRange.upperBound < 0x80)
        
        for i in asciiRange {
            self.insert(Unicode.Scalar(i))
        }
    }
    
    // ---------------------------------------------------
    mutating func insert(asciiRange: ClosedRange<Character>)
    {
        guard let lower = asciiRange.lowerBound.asciiValue,
            let upper = asciiRange.upperBound.asciiValue
        else
        {
            fatalError("\(asciiRange) is not a valid ASCII range")
        }
        
        self.insert(asciiRange: lower...upper)
    }
}

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
public extension Array
{
    // ---------------------------------------------------
    init(capacity: Int)
    {
        self.init()
        self.reserveCapacity(capacity)
    }
}

// ---------------------------------------------------
public extension Array where Element: Equatable
{
    
    // ---------------------------------------------------
    func contains(_ element: Element?) -> Bool {
        return element == nil ? false : contains(element!)
    }
    
    // ---------------------------------------------------
    func contains(_ element: Element) -> Bool {
        for elem in self {
            if elem == element { return true }
        }
        return false
    }
}

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

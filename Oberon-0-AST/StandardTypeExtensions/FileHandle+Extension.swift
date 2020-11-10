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
import Darwin

// ---------------------------------------------------
extension FileHandle: TextOutputStream
{
    // ---------------------------------------------------
    /**
     Obtains a POSIX `stat` structure containing information about the
     `FileDescriptor` referenced by this `FileHandle`
     */
    var stat: Darwin.stat?
    {
        var statStruct = Darwin.stat()
        guard Darwin.fstat(fileDescriptor, &statStruct) == 0 else { return nil }
        
        return statStruct
    }
    
    // ---------------------------------------------------
    var textOutputStream: FileHandleOutputStream?
    {
        return FileHandleOutputStream(self)
    }
    
    // ---------------------------------------------------
    /**
     Writes up to `len` bytes pointed to by `buffer` to the file represented by
     this `FileHandle`
    
     - Parameters:
        - buffer: pointer to the bytes to be written,
        - len: number of bytes to be written
    
     - Returns: a `Result` which on success contains the number of bytes
        written, and on failure contains an `NSError` containing hte POSIX
        domain error code.
    */
    public func write(
        _ buffer: UnsafePointer<UInt8>,
        maxLength len: Int) -> Result<Int, NSError>
    {
        let bytesWritten = Darwin.write(fileDescriptor, buffer, len)
        guard bytesWritten >= 0 else {
            return .failure(posixError(code: errno))
        }
        
        return .success(bytesWritten)
    }
    
    // ---------------------------------------------------
    private func posixError(code errorCode: Int32) -> NSError
    {
        return NSError(
            domain: NSPOSIXErrorDomain,
            code: Int(errorCode),
            userInfo: nil
        )
    }
    
    // ---------------------------------------------------
    public func write(_ string: String)
    {
        if let data = string.data(using: .utf8) {
            write(data)
        }
    }
    // ---------------------------------------------------
    var openedForReading: Bool {
        return [O_RDONLY, O_RDWR].contains(statusFlags & (O_ACCMODE))
    }
    
    // ---------------------------------------------------
    var openedForWriting: Bool {
        return [O_WRONLY, O_RDWR].contains(statusFlags & (O_ACCMODE))
    }
    
    // ---------------------------------------------------
    var statusFlags: Int32
    {
        return Darwin.fcntl(self.fileDescriptor, F_GETFL)
    }
}

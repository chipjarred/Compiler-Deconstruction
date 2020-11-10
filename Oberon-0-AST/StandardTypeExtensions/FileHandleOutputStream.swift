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
public class FileHandleOutputStream: OutputStream
{
    private let fileHandle: FileHandle

    private var _streamError: Error? = nil
    public override var streamError: Error? {
        return _streamError ?? super.streamError
    }

    // ---------------------------------------------------
    public init?(_ fileHandle: FileHandle)
    {
        guard fileHandle.openedForWriting else { return nil }
        
        self.fileHandle = fileHandle
        super.init(toMemory: ())
    }
    
    // ---------------------------------------------------
    /**
     Opens the receiving stream.
    
     A stream must be created before it can be opened. Once opened, a stream
     cannot be closed and reopened.
    
     - Note: since `FileHandleOutputStream` opens a stream using an existing
     `FileHandle`, which can only be obtained by obtaining a `FileDescriptor`,
     which in turn requires opening the file  using a Unix `open(::...)` call
     or some thing similar, the `FileHandleOutputStream` is implicitly open on
     initialization, so this `open()` method does nothing; however, it is
     necessary since the client code may not know what kind of `OutputStream`
     is being used, and typically they do require a call to `open()` before
     writing to them.
     */
    public override func open() { }

    // ---------------------------------------------------
    /**
     Closes the receiver.
    
     Closing the stream terminates the flow of bytes and releases system
     resources that were reserved for the stream when it was opened. If the
     stream has been scheduled on a run loop, closing the stream implicitly
     removes the stream from the run loop. A stream that is closed can still be
     queried for its properties.
    
     - Note: If the `FileHandleOutputStream` uses `FileHandle.standardOutput` or
     `FileHandle.standardError` as it's `FileHandle` it will not actually close
     the handle, and will set `streamError` to an `NSError` with
     `NSPostixErrorDomain`, code: `EBADF` (Invalid handle).
     */
    public override func close()
    {
        if fileHandle == FileHandle.standardOutput
            || fileHandle == FileHandle.standardError
        {
            setPosixError(EBADF)
        }
        else {
            do { try fileHandle.close() } catch { _streamError = error }
        }
        
        super.close()
    }

    // ---------------------------------------------------
    /**
     Writes up to `len` bytes pointed to by `buffer` to the file represented by
     this `OutputStream`
    
     - Parameters:
        - buffer: pointer to the bytes to be written,
        - len: number of bytes to be written
    
     - Returns: the number of bytes written on success, or on failure, -1, and
        sets `streamError`.
     */
    public override func write(
        _ buffer: UnsafePointer<UInt8>,
        maxLength len: Int) -> Int
    {
        let result = fileHandle.write(buffer, maxLength: len)
        switch result
        {
            case .success(let bytesWritten):
                _streamError = nil
                return bytesWritten
            
            case .failure(let error):
                _streamError = error
                return -1
        }
    }

    // ---------------------------------------------------
    /**
     Sets `streamError` to an `NSError` with domain of `NSPOSIXErrorDomain` and
     the specified `errorCode`.
    
     - Parameter errorCode: POSIX error code use for `streamError`
     */
    private func setPosixError(_ errorCode: Int32)
    {
        _streamError = NSError(
            domain: NSPOSIXErrorDomain,
            code: Int(errorCode),
            userInfo: nil
        )
    }
}

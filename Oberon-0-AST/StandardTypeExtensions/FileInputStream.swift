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
/**
`InputStream` for reading from an existing `FileHandle`.
*/
public class FileInputStream: InputStream
{
    // ---------------------------------------------------
    public private(set) var fileHandle: FileHandle
    private let stat: Darwin.stat

    private var _streamError: Error? = nil
    public override var streamError: Error? {
        return _streamError ?? super.streamError
    }
    
    // ---------------------------------------------------
    public init?(fileHandle: FileHandle)
    {
        guard fileHandle.openedForReading, let stat = fileHandle.stat else {
            return nil
        }
        
        self.stat = stat
        self.fileHandle = fileHandle
        super.init(data: Data())
    }
    
    // ---------------------------------------------------
    /**
     Opens the receiving stream.
    
     A stream must be created before it can be opened. Once opened, a stream
     cannot be closed and reopened.
    
     - Note: since `FileInputStream` opens a stream using an existing
     `FileHandle`, which can only be obtained by obtaining a `FileDescriptor`,
     which in turn requires opening the file using a Unix `open(::...)` call or
     some thing similar, the `FileInputStream` is implicitly open on
     initialization, so this `open()` method does nothing; however, it is
     necessary since the client code may not know what kind of `InputStream` is
     being used, and typically they do require a call to `open()` before
     reading from them.
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
    
     - Note: If the `FileHandleStream` uses `FileHandle.standardInput` as it's
     source it will not actually close the handle, and will set `streamError`
     to an `NSError` with `NSPostixErrorDomain`, code: `EBADF` (Invalid handle).
     */
    public override func close()
    {
        if fileHandle == FileHandle.standardInput {
            setPosixError(EBADF)
        }
        else {
            do { try fileHandle.close() } catch { _streamError = error }
        }
        
        super.close()
    }
    
    // ---------------------------------------------------
    /**
     Reads up to a given number of bytes into a given buffer.
    
     - Parameters:
        - buffer: A data buffer. The buffer must be large enough to contain the
            number of bytes specified by len.
        - len: The maximum number of bytes to read.
    
     - Returns: A number indicating the outcome of the operation:
        - A positive number indicates the number of bytes read.
        - 0 indicates that the end of the buffer was reached.
        - -1 means that the operation failed; more information about the error
            can be obtained with `streamError`.
     */
    public override func read(
        _ buffer: UnsafeMutablePointer<UInt8>,
        maxLength len: Int) -> Int
    {
        let rawPointer = UnsafeMutableRawPointer(buffer)
        let bytesRead = Darwin.read(fileHandle.fileDescriptor, rawPointer, len)
        
        if bytesRead < 0 {
            setPosixError(errno)
        }
        else { _streamError = nil }

        return bytesRead
    }
    
    // ---------------------------------------------------
    /**
     Returns by reference a pointer to a read buffer and, by reference, the
     number of bytes available, and returns a `Bool` value that indicates
     whether the buffer is available.
    
     - Parameters:
        - buffer: Upon return, contains a pointer to a read buffer. The buffer
            is only valid until the next stream operation is performed.
        - len: Upon return, contains the number of bytes available.
    
     - Returns: `true` if the buffer is available, otherwise `false`.

     Subclasses of `InputStream` may return `false` if this operation is not
     appropriate for the stream type.
     */
    public override func getBuffer(
        _ buffer: UnsafeMutablePointer<UnsafeMutablePointer<UInt8>?>,
        length len: UnsafeMutablePointer<Int>) -> Bool
    {
        return false
    }
    
    // ---------------------------------------------------
    /**
     A `Bool` value that indicates whether the receiver has bytes available to
     read.
    
     `true` if the receiver has bytes available to read, otherwise `false`. May
     also return `true` if a read must be attempted in order to determine the
     availability of bytes.
     */
    public override var hasBytesAvailable: Bool
    {
        guard stat.isFile else { return false }

        let curOffset = fileHandle.offsetInFile
        
        return curOffset < stat.st_size
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

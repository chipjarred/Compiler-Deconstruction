//
//  InputStream+Extension.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/26/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
extension InputStream
{
	// ---------------------------------------------------
	public static var emptyStream: InputStream
	{
		let stream = InputStream(data: Data())
		stream.open()
		return stream
	}
	
	// ---------------------------------------------------
	/**
	Initialize an `InputStream` using a `String` as the data source
	
	- Parameters:
		- string: `String` to use as the data source.
		- encoding: `String.Encoding` to use when creating the data source.
	*/
	convenience init(
		contentsOf string: String,
		encoding: String.Encoding = .utf8)
	{
		self.init(data: string.data(using: encoding)!)
	}
	
	// ---------------------------------------------------
	/**
	Read a byte from the `InputStream`
	
	- Returns: the byte read on success, or `nil` if there are no more bytes to be read
	*/
	func readByte() -> UInt8?
	{
		var byte: UInt8 = 0
		return unsafeRead(into: &byte) ? byte : nil
	}
	
	// ---------------------------------------------------
	/**
	Read a `Character` from the `InputStream` assuming UTF-8 encoding
	
	- Returns: the `Character` read on success, or `nil` if there are no more `Characters` to
		be read
	
	- Note: Invalid encoded byte sequences are skipped.
	*/
	func readUTF8Character() -> Character?
	{
		// We keep looping until we either successfully read a character or
		// can't read any more.
		while true
		{
			guard let codePoint = readCodePointFromUTF8() else { return nil }
			guard let unicodeScalar = Unicode.Scalar(codePoint) else {
				continue
			}
			
			return Character(unicodeScalar)
		}
	}
	
	// ---------------------------------------------------
	/**
	Read a Unicode code point from the `InputStream` assuming UTF-8 encoding
	
	- Returns: the `UInt32` for the code point read on success, or `nil` if there are no more
		valid code points to be read
	
	- Note: Invalid encoded byte sequences are skipped.
	*/
	private func readCodePointFromUTF8() -> UInt32?
	{
		// ---------------------------------------------------
		func numberOfContinuationBytes(firstByte: UInt8) -> Int
		{
			if firstByte & 0b1000_0000 == 0b0000_0000 { return 0 }
			if firstByte & 0b1110_0000 == 0b1100_0000 { return 1 }
			if firstByte & 0b1111_0000 == 0b1110_0000 { return 2 }
			if firstByte & 0b1111_1000 == 0b1111_0000 { return 3 }
			return -1 // Invalid first byte
		}
		
		// ---------------------------------------------------
		firstByteLoop: while true
		{
			guard var byte = readByte() else { return nil }
			
			// loop until a valid encoding is found or there are no more bytes
			decodingLoop: while true
			{
				let subsequentBytes =
					numberOfContinuationBytes(firstByte: byte)
				
				var codePoint: UInt32 = UInt32(byte)
				if subsequentBytes == 0 { // Early exit for ASCII
					return codePoint
				}
				if subsequentBytes < 0
				{ // First byte was invalid. Try again with the next byte
					continue firstByteLoop
				}
				
				codePoint &= 0b0011_1111 >> subsequentBytes
				
				// If any subsequent byte has an invalid continuation marker,
				// start over using that byte as the first byte
				for _ in 0..<subsequentBytes
				{
					guard unsafeRead(into: &byte) else { return nil }
					guard byte & 0b1100_0000 == 0b1000_0000 else
					{	// Invalid continuation marker found
						continue decodingLoop
					}
					
					codePoint <<= 6
					codePoint |= UInt32(byte & 0b0011_1111)
				}
				
				return codePoint // Valid encoding
			}
		}
	}
	
	// ---------------------------------------------------
	/**
	Read a  value of type,`T`, from the `InputStream`
	
	- Parameter dst: reference to varable into which to write the data
	
	- Returns: `true` if bytes are read equal to the size of `dst`, or `false` otherwise.
	
	- Note: If `false` is returned, `dst` may not have been read at all, or may have been partially read
		and may be in an invalid state.
	
	The main advantage of this method is that does not allocate any memory on the heap, and so can be fast,
	if the underlying data source for the `InputStream` is fast.
		
	- Important: **USE WITH CAUTION**
	
	The caller should ensure that `T` is a *trivially copyable* type, meaning that copying can be safely done
	by merely copying the bits from one location to another.  If a type could be safely copied using C's
	`memcpy` and does not have any stored reference or pointer properties, it may be a good candidate to
	be read using this method.  Good candidates are integer or floating point types, or structs consisting only
	of integers and floating points types.  Beware of possible Swift boxing of struct properties inside of other
	structs, even if the nested structs containly only integer or floating point properties.
	
	No initializer is called for `dst`.  Bytes are simply read into it; therefore, types with complex initialization
	should *not* be read using this method.  This is includes all reference types since this method has no
	way to ensure reference counts are properly handled.
	
	It is also possible to successfully read a value that is actually invalid for the type, even when the type is
	trivially copyable.  For example, an `enum` that has no associated values for any of its cases is trivially
	copyable, but if read using this method, the bytes are merely stored into the space allocated for the
	`enum`, but those bytes may actually not represent a valid case for the `enum`.
	
	If in doubt, you're better off reading the data into a buffer, and initialize the value using that data in some
	Swift-safe way.
	*/
	func unsafeRead<T>(into dst: inout T) -> Bool
	{
		return withUnsafeMutablePointer(to: &dst)
		{
			let sizeOfDst = MemoryLayout<T>.size
			return $0.withMemoryRebound(to: UInt8.self, capacity: sizeOfDst)
			{
				let bytesRead = read($0, maxLength: sizeOfDst)
				return bytesRead == sizeOfDst
			}
		}
	}
}

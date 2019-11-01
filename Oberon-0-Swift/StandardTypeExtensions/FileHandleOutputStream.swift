//
//  FileHandleOutputStream.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct FileHandleOutputStream: TextOutputStream
{
    private let fileHandle: FileHandle
    public let encoding: String.Encoding

	// ---------------------------------------------------
    public init(_ fileHandle: FileHandle, encoding: String.Encoding = .utf8)
	{
        self.fileHandle = fileHandle
        self.encoding = encoding
    }

	// ---------------------------------------------------
	public mutating func write(_ string: String)
	{
        if let data = string.data(using: encoding) {
            fileHandle.write(data)
        }
    }
}

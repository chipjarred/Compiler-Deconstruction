//
//  FileHandleOutputStream.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
struct FileHandleOutputStream: TextOutputStream
{
    private let fileHandle: FileHandle
    let encoding: String.Encoding

	// ---------------------------------------------------
    init(_ fileHandle: FileHandle, encoding: String.Encoding = .utf8)
	{
        self.fileHandle = fileHandle
        self.encoding = encoding
    }

	// ---------------------------------------------------
	mutating func write(_ string: String)
	{
        if let data = string.data(using: encoding) {
            fileHandle.write(data)
        }
    }
}

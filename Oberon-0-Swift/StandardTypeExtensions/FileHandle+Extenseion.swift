//
//  FileHandle+Extenseion.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
extension FileHandle
{
	// ---------------------------------------------------
	func textOutputStream(encoding: String.Encoding) -> FileHandleOutputStream?
	{
		// FIXME: return nil if handle is not open for writing
		return FileHandleOutputStream(self, encoding: encoding)
	}
}

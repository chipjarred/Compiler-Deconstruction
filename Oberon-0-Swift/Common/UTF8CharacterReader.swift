//
//  UTF8CharacterReader.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/27/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct UTF8CharacterReader
{
	public typealias Position = Int
	
	private var inputStream: InputStream
	
	public private(set) var offset: Int = 0
	public private(set) var line: Int = 0
	public private(set) var col: Int = 0
	
	public private(set) var endOfInput: Bool = false
	
	// ---------------------------------------------------
	public var position: Position { return offset }
	
	// ---------------------------------------------------
	public init()
	{
		let stream = FileInputStream(fileHandle: FileHandle.standardInput)!
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public init(inputStream: InputStream) {
		self.inputStream = inputStream
	}
	
	// ---------------------------------------------------
	public init?(fileHandle: FileHandle)
	{
		guard let stream = FileInputStream(fileHandle: fileHandle) else {
			return nil
		}
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public init(contentsOf string: String)
	{
		let stream = InputStream(contentsOf: string, encoding: .utf8)
		stream.open()
		self.init(inputStream: stream)
	}
	
	// ---------------------------------------------------
	public mutating func readCharacter(into ch: inout Character) -> Bool
	{
		guard let c = readCharacter() else
		{
			endOfInput = true
			return false
		}
		
		ch = c
		return true
	}
	
	// ---------------------------------------------------
	private var lastChar: Character = "\0"
	public mutating func readCharacter() -> Character?
	{
		guard !endOfInput, let c = inputStream.readUTF8Character() else
		{
			endOfInput = true
			return nil
		}
		
		offset += 1
		if lastChar == "\n" {
			col = 0
			line += 1
		}
		else {
			col += 1
		}
		
		lastChar = c
		return c
	}
}

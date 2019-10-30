//
//  Oberon0Lexer.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

fileprivate let nullCharacter = Character(ascii: 0)


// ---------------------------------------------------
public struct Oberon0Lexer
{
	private static let IdLen: Int = 16

	public static var value = Int()
	public static var identifier = ""
	internal static var error = true
	private static var ch = Character(ascii: 0)
	private static var errpos = Int()
	private static var sourceReader = UTF8CharacterReader()

	// ---------------------------------------------------
	public static func mark(_ msg: String)
	{
		let p = sourceReader.position - 1
		if p > errpos
		{
			let outStr = " pos \(p) \(msg)"
			
			print(outStr, terminator: "", to: &OberonLog)
			print(outStr)
		}
		errpos = p;
		error = true
	}

	// ---------------------------------------------------
	public static func getSymbol() -> OberonSymbol
	{
		let lowerAlphabetStr = "abcdefghijklmnopqrstuvwxyz"
		let digitStr = "0123456789"
		let lowerAlphabet = CharacterSet(charactersIn: lowerAlphabetStr)
		let upperAlphabet =
			CharacterSet(charactersIn: lowerAlphabetStr.uppercased())
		let alphabet = lowerAlphabet.union(upperAlphabet)
		let numeric = CharacterSet(charactersIn: digitStr)
		let alphaNumeric = alphabet.union(numeric)
		
		// ---------------------------------------------------
		func identifierToken() -> OberonSymbol
		{
			var i = 0
			identifier.removeAll(keepingCapacity: true)
			repeat
			{
				if i < IdLen
				{
					identifier.append(ch)
					i += 1
				}
			} while sourceReader.readCharacter(into: &ch)
				&& alphaNumeric.contains(ch)

			return OberonSymbol.keywordSymbol(for: identifier) ?? .ident
		}
		
		// ---------------------------------------------------
		func numberToken() -> OberonSymbol
		{
			value = 0
			repeat
			{
				let zeroAscii = Character("0").asciiValue!
				let digitValue = Int(ch.asciiValue! - zeroAscii)
				if value <= (Int.max - digitValue) / 10 {
					value = 10 * value + digitValue
				}
				else {
					mark("number too large")
					value = 0
				}
				let _ = sourceReader.readCharacter(into: &ch)
			}
			while numeric.contains(ch)
			
			return OberonSymbol.number
		}
		
		// ---------------------------------------------------
		func discardComment()
		{
			guard sourceReader.readCharacter(into: &ch) else {
				mark("comment not terminated")
				return
			}
			
			outer: while true
			{
				inner: while true
				{
					while ch == "("
					{
						guard sourceReader.readCharacter(into: &ch) else {
							break inner
						}
						if ch == "*" { discardComment() }
					}
					if ch == "*"
					{
						guard sourceReader.readCharacter(into: &ch) else {
							break inner
						}
						break
					}
					
					guard sourceReader.readCharacter(into: &ch) else {
						break inner
					}
				}

				if ch == ")"
				{
					let _ = sourceReader.readCharacter(into: &ch)
					break
				}
				
				if sourceReader.endOfInput
				{
					mark("comment not terminated")
					break
				}
			}
		}
		
		// ---------------------------------------------------
		while !sourceReader.endOfInput,
			ch <= " ",
			sourceReader.readCharacter(into: &ch)
		{ }
		
		if sourceReader.endOfInput {
			return .eof
		}
		else
		{
			switch ch
			{
				case "0"..."9": return numberToken()
				case "A"..."Z", "a"..."z": return identifierToken()
				default: break
			}
			
			var c = nullCharacter
			let _ = sourceReader.readCharacter(into: &c)
			
			var sym: OberonSymbol
			switch ch
			{
				case "&": sym = .and
				case "*": sym = .times
				case "+": sym = .plus
				case "-": sym = .minus
				case "=": sym = .eql
				case "#": sym = .neq
				case "<":
					if c == "="
					{
						c = sourceReader.readCharacter() ?? nullCharacter
						sym = .leq
						
					} else {
						sym = .lss
					}
				case ">":
					if c == "="
					{
						c = sourceReader.readCharacter() ?? nullCharacter
						sym = .geq
					}
					else {
						sym = .gtr
					}
				case ";": sym = .semicolon
				case ",": sym = .comma
				case ":":
					if c == "="
					{
						c = sourceReader.readCharacter() ?? nullCharacter
						sym = .becomes
					}
					else { sym = .colon }
				case ".": sym = .period
				case "(":
					if c == "*" {
						discardComment()
						
						return getSymbol()
					}
					else {
						sym = .lparen
					}
				case ")": sym = .rparen
				case "[": sym = .lbrak
				case "]": sym = .rbrak
				case "~": sym = .not
				default: sym = .null
			}
			
			ch = c
			
			return sym
		}
	}

	// ---------------------------------------------------
	public static func Init(sourceStream: InputStream)
	{
		error = false
		errpos = 0;
		sourceReader = UTF8CharacterReader(inputStream: sourceStream)
		if !sourceReader.readCharacter(into: &ch) { ch = nullCharacter }
	}
}


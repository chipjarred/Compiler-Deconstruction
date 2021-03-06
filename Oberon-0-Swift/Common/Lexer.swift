// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

fileprivate let nullCharacter = Character(ascii: 0)

fileprivate let lowerAlphabetStr = "abcdefghijklmnopqrstuvwxyz"
fileprivate let digitStr = "0123456789"
fileprivate let lowerAlphabet =
	CharacterSet(charactersIn: lowerAlphabetStr)
fileprivate let upperAlphabet =
	CharacterSet(charactersIn: lowerAlphabetStr.uppercased())
fileprivate let alphabet = lowerAlphabet.union(upperAlphabet)
fileprivate let numeric = CharacterSet(charactersIn: digitStr)
fileprivate let alphaNumeric = alphabet.union(numeric)


// ---------------------------------------------------
public struct Lexer
{
	private static let IdLen: Int = 16

	/*
	We could now move identifier to be a local variable in identiferToken(),
	but it's actually a bit more efficient to keep it here, allowing
	identiferToken() to just re-use the existing storage for it rather than
	having to reallocate it for every call.
	*/
	private var identifier = ""
	private var ch = Character(ascii: 0)
	private var sourceReader = UTF8CharacterReader()
	public var errorReporter: ErrorReporter

	// ---------------------------------------------------
	private func mark(_ msg: String) {
		errorReporter.mark(msg, at: sourceReader.position)
	}

	// ---------------------------------------------------
	private mutating func identifierToken() -> Token
	{
		var i = 0
		identifier.removeAll(keepingCapacity: true)
		repeat
		{
			if i < Lexer.IdLen
			{
				identifier.append(ch)
				i += 1
			}
		} while sourceReader.readCharacter(into: &ch)
			&& alphaNumeric.contains(ch)

		if let keywordSymbol = TokenType.keywordTokenType(for: identifier) {
			return Token(keywordSymbol)
		}
		return Token(.identifier, identifier: identifier)
	}
	
	// ---------------------------------------------------
	private mutating func numberToken() -> Token
	{
		var value = 0
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
		}
		while sourceReader.readCharacter(into: &ch) && numeric.contains(ch)
		
		return Token(.number, value: value)
	}

	// ---------------------------------------------------
	private mutating func discardComment()
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
	public mutating func getToken() -> Token
	{
		while !sourceReader.endOfInput,
			ch <= " ",
			sourceReader.readCharacter(into: &ch)
		{ }
		
		if sourceReader.endOfInput {
			return Token(.eof)
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
			
			var sym: TokenType
			switch ch
			{
				case "&": sym = .and
				case "*": sym = .times
				case "+": sym = .plus
				case "-": sym = .minus
				case "=": sym = .isEqualTo
				case "#": sym = .isNotEqualTo
				case "<":
					if c == "="
					{
						c = sourceReader.readCharacter() ?? nullCharacter
						sym = .lessThanOrEqualTo
						
					} else {
						sym = .lessThan
					}
				case ">":
					if c == "="
					{
						c = sourceReader.readCharacter() ?? nullCharacter
						sym = .greaterThanOrEqualTo
					}
					else {
						sym = .greaterThan
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
						
						return getToken()
					}
					else {
						sym = .openParen
					}
				case ")": sym = .closeParen
				case "[": sym = .openBracket
				case "]": sym = .closeBracket
				case "~": sym = .not
				default: sym = .null
			}
			
			ch = c
			
			return Token(sym)
		}
	}
	
	// ---------------------------------------------------
	public init(sourceStream: InputStream, errorsTo reporter: ErrorReporter)
	{
		self.errorReporter = reporter
		self.sourceReader = UTF8CharacterReader(inputStream: sourceStream)
		if !self.sourceReader.readCharacter(into: &ch) { ch = nullCharacter }
	}
}


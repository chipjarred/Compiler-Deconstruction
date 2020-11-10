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

fileprivate let newlineChar = Character("\n")
fileprivate let decimalDigits = CharacterSet(asciiRange: "0"..."9")
fileprivate let lowerAlpha = CharacterSet(asciiRange: "a"..."z")
fileprivate let upperAlpha = CharacterSet(asciiRange: "A"..."Z")
fileprivate let alphabet = lowerAlpha.union(upperAlpha)
fileprivate let alphaNumeric = decimalDigits.union(alphabet)
fileprivate let nameChars: CharacterSet =
{
	var nameChars = CharacterSet()
	
	// german characters (not really, but in original Oberon Texts module)
	nameChars.insert(asciiRange: 0x80...0x96)
	nameChars.formUnion(alphaNumeric)
	nameChars.insert("@") 	// mail, compiler
	nameChars.insert(".")	// mail, filenames, compiler
	nameChars.insert("/")	// filenames
	nameChars.insert(":")	// filenames (Classic Mac)
	nameChars.insert("_")
	
	return nameChars
}()

fileprivate let zeroASCII = Character("0").asciiValue!
fileprivate let aASCII = Character("A").asciiValue!



// ---------------------------------------------------
public final class RISCInputScanner
{
	
	private var inputStream: InputStream
	
	public private(set) var lineNumber = 0
	public private(set) var column = 0
	
	// ---------------------------------------------------
	public init(_ inputStream: InputStream) {
		self.inputStream = inputStream
	}
	
	// ---------------------------------------------------
	public convenience init(contentsOf string: String) {
		let stream = InputStream(contentsOf: string)
		stream.open()
		self.init(stream)
	}
	
	// ---------------------------------------------------
	public convenience init()
	{
		let stream = FileInputStream(fileHandle: FileHandle.standardInput)!
		stream.open()
		self.init(stream)
	}
	
	// ---------------------------------------------------
	public enum Token: CustomStringConvertible
	{
		case invalid(_ contents: String)
		case char(_ c: Character)
		case name(_ n: String)
		case string(_ s: String)
		case integer(_ v: Int)
		case float(_ v: Float)
		case double(_ v: Double)
		
		// ---------------------------------------------------
		public var description: String
		{
			switch self
			{
				case .invalid: return "invalid token"
				case let .char(c): return "\(c)"
				case let .name(n): return "\(n)"
				case let .string(s): return "\(s)"
				case let .integer(v): return "\(v)"
				case let .float(v): return "\(v)"
				case let .double(v): return "\(v)"
			}
		}
	}
	
	// ---------------------------------------------------
	/**
	Read the next symbol. Whitespace is ignored. `\n` increments the line counter.
	*/
	public func scan() -> Token?
	{
		guard var ch = nextNonWhiteSpaceCharacter() else { return nil }
		
		var result: Token
		if alphabet.contains(ch) || ch == "." || ch == "/" {
			return readNameToken(startingWith: ch)
		}
		else if ch == "\"" {
			return readStringLiteralToken(startingWith: ch, closedBy: ch)
		}
		else
		{
			let neg = ch == "-"
			let sign = neg || ch == "+"
			
			if sign {
				guard nextChar(into: &ch) else { return .invalid("\(ch)") }
			}
			
			if decimalDigits.contains(ch) {
				result = readNumberToken(startingWith: ch, sign: neg ? -1 : 1)
			}
			else
			{
				if sign
				{
					putbackChar(ch)
					result = .char(neg ? "-" : "+")
				}
				else {
					result = .char(ch)
				}
			}
		}
		
		return result
	}
	
	// ---------------------------------------------------
	private func readNameToken(startingWith firstChar: Character) -> Token
	{
		var ch = firstChar
		var accumulatedStr = ""
		var numChars = 0
		repeat
		{
			accumulatedStr.append(ch)
			numChars += 1
			guard nextChar(into: &ch) else { break }
		} while (nameChars.contains(ch))
		
		defer { putbackChar(ch) }
		
		return numChars == 1 && !alphabet.contains(firstChar)
			? .char(firstChar)
			: .name(accumulatedStr)
	}
	
	// ---------------------------------------------------
	private func readStringLiteralToken(
		startingWith firstChar: Character,
		closedBy closeQuote: Character) -> Token
	{
		var accumulatedStr = ""
		var ch = firstChar
		guard nextChar(into: &ch) else { return .invalid("\(firstChar)") }
		
		while ch != closeQuote && ch != newlineChar
		{
			accumulatedStr.append(ch)
			guard nextChar(into: &ch) else {
				return .invalid("\(firstChar)\(accumulatedStr)")
			}
		}

		return .string(accumulatedStr)
	}
	
	// ---------------------------------------------------
	private func readNumberToken(
		startingWith firstChar: Character,
		sign: Int) -> Token
	{
		var digits = [Character]()
		
		var ch = firstChar
		let hex = readDigits(startingWith: firstChar, into: &digits)
		
		guard nextChar(into: &ch) else
		{
			return hex
				? .invalid(String(digits))
				: .integer(sign * decimalValue(from: digits))
		}
		
		if ch == "H" {
			return .integer(sign * hexValue(from: digits))
		}
		
		if ch == "."
		{
			guard !hex else { return .invalid(String(digits) + ".") }

			let doubleValue =
				Double(decimalValue(from: digits)) + readFractionalValue()
			
			guard nextChar(into: &ch) else {
				return .double(doubleValue)
			}
			
			let E = ch.uppercased()
			if "DE".contains(E)
			{
				let exponent = readExponentValue()
				
				return E == "D"
					? .double(pow(doubleValue, exponent))
					: .float(pow(Float(doubleValue), Float(exponent)))
			}
			
			return .double(doubleValue)
		}
		else
		{
			putbackChar(ch)
			return hex
				? .invalid(String(digits))
				: .integer(sign * decimalValue(from: digits))
		}
	}
	
	// ---------------------------------------------------
	/**
	- Returns: `true` if `digits` contains hexadecimal digits, or `false` if it contains only '
		decimal digits.
	*/
	private func readDigits(
		startingWith firstChar: Character,
		into digits: inout [Character]) -> Bool
	{
		var ch = firstChar
		var hex = false
		while true
		{
			digits.append(ch)
			guard nextChar(into: &ch) else { return hex }

			if ch < "0" { break }
			if ch > "9"
			{
				if "ABCDEF".contains(ch.uppercased()) {
					hex = true
				}
				else { break }
			}
		}
		
		putbackChar(ch)
		return hex
	}
	
	// ---------------------------------------------------
	private func readDecimalValue<T: Numeric>() -> T
	{
		var value: T = 0
		var digit: Character = "0"
		
		while nextChar(into: &digit)
		{
			guard decimalDigits.contains(digit) else
			{
				putbackChar(digit)
				break
			}
			
			value = 10 * value + T(exactly: digit.asciiValue! - zeroASCII)!
		}
		
		return value
	}
	
	// ---------------------------------------------------
	private func readFractionalValue() -> Double
	{
		var value: Double = 0
		var multiplier: Double = 0.1
		var digit: Character = "0"
		
		while nextChar(into: &digit)
		{
			guard decimalDigits.contains(digit) else
			{
				putbackChar(digit)
				break
			}
			
			value += multiplier * Double(digit.asciiValue! - zeroASCII)
			multiplier /= 10
		}
		
		return value
	}
	
	// ---------------------------------------------------
	private func readExponentValue() -> Double
	{
		var ch: Character = "0"
		guard nextChar(into: &ch) else {
			return 0
		}
		
		let neg = ch == "-"
		if "+-".contains(ch)
		{
			guard nextChar(into: &ch) else
			{
				putbackChar(ch)
				return 0
			}
		}
		else
		{
			putbackChar(ch)
			if !decimalDigits.contains(ch) {
				return 0
			}
		}
		
		return readDecimalValue() * (neg ? -1 : 1)
	}
	
	// ---------------------------------------------------
	private func decimalValue(from digits: [Character]) -> Int
	{
		var value = 0
		
		for c in digits
		{
			assert(decimalDigits.contains(c))
			
			value *= 10
			value += Int(c.asciiValue! - zeroASCII)
		}
		
		return value
	}
	
	// ---------------------------------------------------
	private func hexValue(from digits: [Character]) -> Int
	{
		var value = 0
		
		for c in digits
		{
			value <<= 4
			if decimalDigits.contains(c) {
				value += Int(c.asciiValue! - zeroASCII)
			}
			else
			{
				assert("ABCDEF".contains(c))
				
				value += Int(c.asciiValue! - aASCII + 10)
			}
		}
		
		return value
	}

	private var putbackStack = [Character]()
	// ---------------------------------------------------
	private func putbackChar(_ c: Character) {
		putbackStack.append(c)
	}
	
	// ---------------------------------------------------
	private func nextChar(into c: inout Character) -> Bool
	{
		if putbackStack.count > 0
		{
			c = putbackStack.removeLast()
			return true
		}
		
		guard let ch = inputStream.readUTF8Character() else { return false }
		
		c = ch
		return true
	}
	
	// ---------------------------------------------------
	private func nextNonWhiteSpaceCharacter() -> Character?
	{
		while let ch = inputStream.readUTF8Character()
		{
			if ch == newlineChar {
				lineNumber += 1
			}
			else if !CharacterSet.whitespacesAndNewlines.contains(ch) {
				return ch
			}
		}
		
		return nil
	}
}

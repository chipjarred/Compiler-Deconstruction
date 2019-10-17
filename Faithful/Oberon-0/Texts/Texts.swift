//
//  Texts.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/15/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Oberon

// ---------------------------------------------------
/*
Minimal implementation of Oberon's Texts module.  The purpose is not to fully
implement Oberon's library, but rather to hack together just enough so that a
faithful translation of Niklaus Wirth's Oberon-0 compiler from
"Compiler Construction" into Swift will compile and run.  More than that is
unnecessary since a full Swift should use Foundation's Streams

An actual Oberon implementation of the Texts module can be found at

	https://en.wikibooks.org/wiki/Oberon/A2/Oberon.Texts.Mod

This is a useful source for fleshing out interface details.
*/

// ---------------------------------------------------
/**
The `Texts` module implements the text abstract data type. `Text`s are sequences of
characters and objects, with different colors, different fonts, and vertical offsets.
*/

// Scanner symbol classes.
public var Inval = 0      // Invalid symbol.
public var Name = 1       // Name s (of length len).
public var String = 2     // Quoted string s (length len).
public var Int = 3        // Integer i (decimal or hexadecimal).
public var Real = 4       // Real number x.
public var LongReal = 5   // Long real number y.
public var Char = 6       // Special character c.
public var Object = 7     // Object obj.

internal let TAB = CHAR(0x9)
internal let CR = CHAR(0xd)
internal let LF = CHAR(0xa)

internal let OldTextBlockId = 0x01
internal let OldTextSpex = 0xF0

/*
TextBlock = TextBlockId type hlen run {run} 0 tlen {AsciiCode} [font block].
  run = font [name] col voff len.
*/

internal let BufSize = 64


// Placeholder types until I know what they really should be
public typealias Buffer = [CHAR]

// ---------------------------------------------------
public typealias Text = TextDesc?
public class TextDesc
{
	private var bytes = [CHAR]()
	
	public var count: Int { return bytes.count }
	
	// ---------------------------------------------------
	public subscript (index: Int) -> CHAR
	{
		// ---------------------------------------------------
		get
		{
			precondition(
				bytes.indices.contains(index),
				"Index \(index) out of range, \(bytes.indices)"
			)
			return bytes[index]
		}
		
		// ---------------------------------------------------
		set
		{
			precondition(
				bytes.indices.contains(index),
				"Index \(index) out of range, \(bytes.indices)"
			)
			bytes[index] = newValue
		}
	}
	
	// ---------------------------------------------------
	public subscript (index: LONGINT) -> CHAR
	{
		get { return self[Swift.Int(index)] }
		set { self[Swift.Int(index)] = newValue }
	}
	
	// ---------------------------------------------------
	public func append(_ c: CHAR) {
		bytes.append(c)
	}
	
	// ---------------------------------------------------
	public func append(contentsOf chars: [CHAR]) {
		bytes.append(contentsOf: chars)
	}
	
	// ---------------------------------------------------
	public func append(_ chars: ARRAY<CHAR>)
	{
		bytes.reserveCapacity(bytes.count + chars.count)
		for i in 0..<chars.count {
			append(chars[i])
		}
	}
	
	// ---------------------------------------------------
	public init() { }

	// ---------------------------------------------------
	public init(_ string: String)
	{
		bytes.reserveCapacity(string.count)
		for c in string {
			bytes.append(CHAR(c))
		}
	}
}

// ---------------------------------------------------
public struct Reader
{
	public var T: Text = nil
	public var off: LONGINT = 0
	public var eot: BOOLEAN = false	// Reader has reached end of the text stream.

	// Originally this represented the Color index of last character read, but
	// I'm using it for the column (ie, offset from start of line)
	public var col: SHORTINT = 0	//


	// Originally this represted "vertical offset" of the last character read,
	// whatever that meant.  I'm taking it to mean 0-based line numbers
	public var voff: SHORTINT = 0
	public init() { }
}

// ---------------------------------------------------
public struct Writer
{
	public var buf = Buffer()
	
	public init() { }
}

// ---------------------------------------------------
/**
Return reader's position within the text.
*/
public func Pos(_ R: Reader) -> LONGINT {
	return R.off
}

// ---------------------------------------------------
/**
Write string `s` to `W'`s buffer
*/
public func WriteString(_ W: inout Writer, _ s: ARRAY<CHAR>)
{
	for c in s {
		Write(&W, c)
	}
}

// ---------------------------------------------------
public func WriteString(_ W: inout Writer, _ s: String)
{
	let a = ARRAY<CHAR>.init(stringLiteral: s)
	WriteString(&W, a)
}

// ---------------------------------------------------
/**
Write integer `x` to `W'`s buffer. Spaces are padded to the left until the number field is at least `n`
characters long.
*/
public func WriteInt(_ W: inout Writer, _ x: LONGINT, _ n: LONGINT)
{
	var intStr = ARRAY<CHAR>(stringLiteral: "\(x)")
	let paddingLength = n - LONGINT(intStr.count)
	if paddingLength > 0
	{
		var padding = ARRAY<CHAR>(repeating: " ", count: paddingLength)
		padding.reserveCapacity(padding.count + intStr.count)
		padding.append(contentsOf: intStr)
		intStr = padding
	}
	
	WriteString(&W, intStr)
}

// ---------------------------------------------------
/**
Write character `ch` to writer `W'`s buffer.
*/
public func Write(_ W: inout Writer, _ ch: CHAR) {
	W.buf.append(ch)
}

// ---------------------------------------------------
/**
Write an end-of-line character to `W`'s buffer.
*/
public func WriteLn(_ W: inout Writer) {
	Write(&W, CHAR("\n"))
}

// ---------------------------------------------------
/**
Append buffer to the end of text `T`. `B` is emptied.
*/
public func Append(_ T: Text, _ B: inout Buffer)
{
	guard let T = T else { fatalError("Attempt to append to nil text") }
	T.append(contentsOf: B)
	B.removeAll(keepingCapacity: true)
}

// ---------------------------------------------------
/**
Read next character into `ch`. `R.eot` is set when the last character is read. The fields `lib`, `voff`
and `col` of `R` give information about the last character read
*/
public func Read(_ R: inout Reader, _ ch: inout CHAR)
{
	guard let T = R.T else {
		fatalError("Read nil text")
	}
	R.eot = R.off >= T.count
	guard !R.eot else {
		ch = 0
		return
	}

	// We test the previous character before reading the next so the line
	// number (voff) of a previously read newline remains the same line that it
	// terminates, however, at the very start off the file, there is no
	// previously read character, so line/column numbers need updating
	if R.off > 0
	{
		let lastChar = T[R.off - 1]
		if lastChar == LF
		{
			R.col = 0
			R.voff += 1
		}
		else {
			R.col += 1
		}
	}
	else
	{
		assert(R.voff == 0)
		assert(R.col == 0)
	}
	
	ch = T[R.off]
	R.off += 1
}

// ---------------------------------------------------
/**
Open text reader `R` and set it up at position `pos` in text `T`.
*/
public func OpenReader(_ R: inout Reader, _ T: Text, _ pos: LONGINT)
{
	R.T = T
	R.col = 0
	R.voff = 0
	R.off = 0
	R.eot = (T?.count ?? 0) == 0
}

// ---------------------------------------------------
/**
Open a new writer `W`.
*/
public func OpenWriter(_ W: inout Writer) {
	W = Writer()
}

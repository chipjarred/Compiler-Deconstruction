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

public var nameChars = initNameChars()

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
public class TextDesc: DefaultInitializable, CustomStringConvertible
{
	private var bytes = [CHAR]()
	
	public var count: Int { return bytes.count }
	
	// ---------------------------------------------------
	public var description: String
	{
		var result: String = ""
		result.reserveCapacity(count)
		
		for c in bytes {
			result += c.description
		}
		
		return result
	}
	
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
	public required init() { }

	// ---------------------------------------------------
	public init(_ string: String)
	{
		bytes.reserveCapacity(string.count)
		for c in string {
			bytes.append(CHAR(c))
		}
	}
	
	// ---------------------------------------------------
	public func clear() {
		bytes.removeAll(keepingCapacity: true)
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

	// Scanner properties
	/// Character immediately following the last symbol scanned
	public var nextCh: CHAR = 0
	
	/// != carriage returns scanned so far.
	public var line: INTEGER = 0
	
	/// Scan result: Int, Real, String etc.
	public var `class`: INTEGER = 0

	public var i: LONGINT = 0
	public var x: REAL = 0
	public var y: LONGREAL = 0
	public var c: CHAR = 0
	
	/// Length of name or string scanned.
	public var len: SHORTINT = 0
	public var s = ARRAY<CHAR>(count: 256)
	
	// public var obj: Objects.Object
}

// ---------------------------------------------------
public struct Writer
{
	public var buf = Buffer()
	
	public init() { }
}

// ---------------------------------------------------
/**
Scanner for symbol streams
*/
public typealias Scanner = Reader

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
	for c in s
	{
		if c.ascii == 0 { break }
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
Write a hexadecimal representation of x to W's buffer.
*/
public func WriteHex(_ W: inout Writer, _ x: LONGINT)
{
	var a = ARRAY<CHAR>(count: 10)
	var x = x
    var i = 0
	Write(&W, " ");
	repeat
	{
		let y = x % 0x10
		a[i] = y < 10 ? CHR(y + 0x30) : CHR(y + 0x37)
		x = x / 0x10
		i += 1
	} while i != 8
	
	repeat
	{
		i -= 1
		Write(&W, a[i])
	} while i != 0

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
	guard let T = T else {
		fatalError("Attempt to append to nil text")
	}
	for c in B
	{
		if c.ascii != 0 {
			T.append(c)
		}
		else { break }
	}
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

// ---------------------------------------------------
/**
Open text scanner `S` and set it up at position `pos` in text `T`.
*/
public func OpenScanner(_ S: inout Scanner, _ T: Text, _ pos: LONGINT)
{
	OpenReader(&S, T, pos)
	S.line = 0
	S.class = Inval
	var nextChar: CHAR = 0
	Read(&S, &nextChar)
	S.nextCh = nextChar
}

// ---------------------------------------------------
/**
Open text `T` from file specified by name. A new text is opened when `name = ""`.
*/
public func Open(T: Text, name: ARRAY<CHAR>)
{
	#if false
	/*
	This is a translation of the Oberon A2 code.  Oberion (the OS) is
	a visual system, and it's assume that source code should be in some
	text view.  We don't want that.  We just want to read from a file using
	Unix command line.
	*/
	var f: Files.File
	var R: Files.Rider
	var len: LONGINT
	var ch: CHAR

	obs = nil
	f = Files.Old(name)
	if f != NIL
	{
		Files.Set(R, f, 0)
		Files.Read(R, ch)
		
		if ch == DocBlockId {
			ReadDocHeader(R, ch)
		}
		if (ch == TextBlockId) || (ch == OldTextBlockId) {
			Load(T, f, Files.Pos(R), len)
		}
		else {
			LoadAscii(T, f)
		}
	}
	else {
		GenNew(T)
	}
	#else
	fatalError("Not running on actual Oberon System")
	#endif
}


// ---------------------------------------------------
/**
Read the next symbol. Whitespace is ignored. `CR` increments the line counter.
*/
public func Scan(_ S: inout Scanner)
{
	// ---------------------------------------------------
	func RealsTen(_ e: LONGINT) -> LONGREAL
	{
		guard e >= -307 else { return 0 }
		guard e <= 308 else { return 1 / 0 }
	
		if e == 0 { return 1 }
		
		var result: LONGREAL = 10
		
		if e > 0 {
			for _ in 0..<e {
				result *= 10
			}
		}
		else {
			for _ in 0..<(-e) {
				result /= 10
			}
		}
		
		return result
	}
	
	// ---------------------------------------------------
	func RealsExpo(_ x: REAL) -> LONGINT {
		return LONGINT(x.exponent)
	}
	
	// ---------------------------------------------------
	func RealsExpoL(_ x: LONGREAL) -> LONGINT {
		return LONGINT(x.exponent)
	}

	// fixed size: maxD <= LEN(S.s)!
	let maxD = 256
	var ch, E: CHAR
	var  neg, negE, hex, sign: BOOLEAN
	var  i, j, h, e, k, k1, k2, k3: LONGINT
	var  y: LONGREAL
	var  d = ARRAY<CHAR>(count: maxD)

	ch = S.nextCh
	i = 0
	var useNewlineInsteadOfCarriageReturn:Bool { return true }
	let newlineChar = useNewlineInsteadOfCarriageReturn ? LF : CR
	let otherNewlineChar = useNewlineInsteadOfCarriageReturn ? CR : LF
	while true
	{
		// This condition is in the original
//		if S.lib == nil || !(S.lib is Fonts.Font) {
//			break
//		}
		
		if ch == newlineChar {
			S.line += 1
		}
		else if ch != " " && ch != TAB && ch != otherNewlineChar {
			break
		}
		Read(&S, &ch)
	}
	
//	if S.lib == nil
//	{
//		S.class = Inval
//		S.eot = true
//		Read(S, ch)
//	}
//	else if !(S.lib is Fonts.Font)
//	{
//		S.class = Object
//		S.lib.GetObj(S.lib, ORD(ch), S.obj)
//		Read(S, ch)
//	}
//	else if ("A" <= CAP(ch)) && (CAP(ch) <= "Z") || (ch = ".") || (ch = "/") /*OR (ch = ":")*/
	if ("A" <= CAP(ch))
		&& (CAP(ch) <= "Z")
		|| (ch == ".")
		|| (ch == "/") /*OR (ch = ":")*/
	{
		/*name*/
		repeat
		{
			S.s[i] = ch
			i += 1
			Read(&S, &ch)
		} while !(!(nameChars[ORD(ch)]) /* || !(S.lib is Fonts.Font) */ || (i == LEN(S.s) - 1))
		
		S.s[i] = 0
		if (i == 1) && ((CAP(S.s[0]) < "A") || (CAP(S.s[0]) > "Z"))
		{
			S.c = S.s[0]
			S.class = Char
		}
		else
		{
			S.len = SHORT(SHORT(i))
			S.class = Name
		}
	}
	else if ch == 0x22
	{
		/*literal string*/
		Read(&S, &ch)
		while (ch != 0x22) && (ch >= " ") /* && (S.lib is Fonts.Font)  */ && (i != LEN(S.s) - 1)
		{
			S.s[i] = ch
			i += 1
			Read(&S, &ch)
		}
		while (ch != 0x22) && (ch >= " ") /* && (S.lib is Fonts.Font) */ {
			Read(&S, &ch)
		}
		S.s[i] = 0
		S.len = SHORT(SHORT(i))
		Read(&S, &ch)
		S.class = String
	}
	else
	{
		switch ch
		{
			case "-":
				sign = true
				neg = true
			case "+":
				sign = true
				neg = false
			default:
				sign = false
				neg = false
		}
		if sign { Read(&S, &ch) }
		
		if ("0" <= ch) && (ch <= "9") /* && (S.lib is Fonts.Font) */
		{
			/*number*/
			hex = false
			j = 0
			while true
			{
				d[i] = ch
				i += 1
				Read(&S, &ch)
				if (ch < "0") /* || ~(S.lib is Fonts.Font) */ || (i >= maxD) {
					break
				}
				if "9" < ch
				{
					if ("A" <= ch) && (ch <= "F")
					{
						hex = true
						ch = CHR(ORD(ch)-7)
					}
					else if ("a" <= ch) && (ch <= "f")
					{
						hex = true
						ch = CHR(ORD(ch)-0x27)
					}
					else { break }
				}
			}
			if (ch == "H") /* && (S.lib is Fonts.Font) */
			{
				/*hex number*/
				Read(&S, &ch)
				S.class = Int
				if i-j > 8 {
					j = i - 8
				}
				k = ORD(d[j]) - 0x30
				j += 1
				if (i - j == 7) && (k >= 8) {
					k -= 16
				}
				while j < i
				{
					k = k * 0x10 + (ORD(d[j]) - 0x30)
					j += 1
				}
				
				S.i = neg ? -k : k
			}
			else if (ch == ".") /* && (S.lib is Fonts.Font) */
			{
				/*read real*/
				Read(&S, &ch)
				h = i
				while ("0" <= ch) && (ch <= "9") /* && (S.lib is Fonts.Font) */ && (i < maxD)
				{
					d[i] = ch
					i += 1
					Read(&S, &ch)
				}
				/*-------- begin floating-point handling BM 1993.3.10 -----------------------------------*/
				while i % 8 != 0
				{
					d[i] = "0"
					i += 1
				}
				j = 0
				k = 0
				k1 = 0
				k2 = 0
				k3 = 0
				/* store digits 0..7, 8..15, 16..23, 24..31 in k, k1, k2, k3 */
				func storeDigits(into k: inout LONGINT, j: inout LONGINT, limit: LONGINT)
				{
					while j < limit
					{
						k = k*10 + ORD(d[j]) - ORD("0")
						j += 1
					}
				}
				storeDigits(into: &k, j: &j, limit: 8)
				if 8 < i {
					storeDigits(into: &k1, j: &j, limit: 16)
				}
				if 16 < i {
					storeDigits(into: &k2, j: &j, limit: 24)
				}
				if 24 < i {
					storeDigits(into: &k3, j: &j, limit: 32)
				}
				e = 0
				E = ch
				if ((E == "D") || (E == "E")) /* && (S.lib is Fonts.Font) */
				{
					Read(&S, &ch)
					if (ch == "-") /* && (S.lib is Fonts.Font) */
					{
						negE = true
						Read(&S, &ch)
					}
					else
					{
						negE = false
						if (ch == "+") /* && (S.lib is Fonts.Font) */ {
							Read(&S, &ch)
						}
					}
					while ("0" <= ch) && (ch <= "9") /* && (S.lib is Fonts.Font) */
					{
						e = e*10 + ORD(ch) - ORD("0")
						Read(&S, &ch)
					}
					if negE {
						e = -e
					}
				}
				y = LONGREAL(k3) * RealsTen(-32) + LONGREAL(k2) * RealsTen(-24)
				y = y + LONGREAL(k1) * RealsTen(-16)
				if abs(e + h) < 308 {
					y = (y + LONGREAL(k) * RealsTen(-8)) / RealsTen(-e-h)
				}
				else
				{
					y = (y + LONGREAL(k) * RealsTen(-8)) * RealsTen(h)
					if (e <= 308-32) || (e <= 308) && (y < MAX(LONGREAL.self) / RealsTen(e))
					{
						y = y * RealsTen(e)
					}
					else {
						y = MAX(LONGREAL.self)
					}
				}
				if E == "D"
				{
					if y == MAX(LONGREAL.self) {
						S.class = Inval /* NaN */
					}
					else
					{
						S.class = LongReal
						S.y = neg ? -y : y

						if RealsExpoL(S.y) == 0 {
							S.y = 0
						}
					}
				}
				else if MAX(REAL.self) < REAL(y) {
					S.class = Inval /* NaN */
				}
				else
				{
					S.class = Real
					S.x = neg ? SHORT(-y) : SHORT(y)

					if RealsExpo(S.x) == 0 {
						S.x = 0
					}
				}
				/*-------- end floating-point handling BM 1993.3.10 -----------------------------------*/
				if hex {
					S.class = Inval
				}
			}
			else
			{
				/*decimal integer*/
				S.class = `Int`
				k = 0
				while (j != i) && ((k < MAX(LONGINT.self) / 10) || (k == MAX(LONGINT.self) / 10) && ((ORD(d[j]) - 0x30) <= MAX(LONGINT.self) % 10))
				{ /*JG*/
					k = k*10 + (ORD(d[j]) - 0x30)
					j += 1
				}
				if j != i {
					S.class = Inval
				}
				else
				{
					if neg {
						S.i = -k
					}
					else {
						S.i = k
					}
					S.class = hex ? Inval : `Int`
				}
			}
		}
		else
		{
			S.class = Char
			if sign {
				S.c = neg ? "-" : "+"
			}
			else
			{
				S.c = ch
				Read(&S, &ch)
			}
		}
	}
	S.nextCh = ch
}

fileprivate func initNameChars() -> ARRAY<Bool>
{
	var nameChars = ARRAY<Bool>(repeating: false, count: 256)
	func set(range: ClosedRange<LONGINT>) {
		for i in range { nameChars[i] = true }
	}
	set(range: 0x80...0x96) // german characters (not really, but in original)
	set(range: ORD("0")...ORD("9"))
	set(range: ORD("A")...ORD("Z"))
	set(range: ORD("a")...ORD("z"))
	nameChars[ORD("@")] = true	// mail, compiler
	nameChars[ORD(".")] = true	// mail, filenames, compiler
	nameChars[ORD("/")] = true	// filenames
	nameChars[ORD(":")] = true	// filenames (Mac)
	nameChars[ORD("_")] = true
	
	return nameChars
}

//
//  Texts.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/15/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
/**
Minimal implementation of Oberon's Texts module.  The purpose is not to fully
implement Oberon's library, but rather to hack together just enough so that a
faithful translation of Niklaus Wirth's Oberon-0 compiler from
"Compiler Construction" into Swift will compile and run.  More than that is
unnecessary since a full Swift should use Foundation's Streams

An actual Oberon implementation of the Texts module can be found at

	https://en.wikibooks.org/wiki/Oberon/A2/Oberon.Texts.Mod

This is a useful source for fleshing out interface details.
*/
public struct Texts
{
	// ---------------------------------------------------
	/**
	The `Texts` module implements the text abstract data type. `Text`s are sequences of
	characters and objects, with different colors, different fonts, and vertical offsets.
	*/

	// Scanner symbol classes.
	public static var Inval = 0      // Invalid symbol.
	public static var Name = 1       // Name s (of length len).
	public static var String = 2     // Quoted string s (length len).
	public static var Integer = 3    // Integer i (decimal or hexadecimal).
	public static var Real = 4       // Real number x.
	public static var LongReal = 5   // Long real number y.
	public static var Char = 6       // Special character c.
	public static var Object = 7     // Object obj.

	public static var nameChars = initNameChars()

	internal static let TAB = Character(ascii: 0x9)
	internal static let CR = Character(ascii: 0xd)
	internal static let LF = Character(ascii: 0xa)

	internal static let OldTextBlockId = 0x01
	internal static let OldTextSpex = 0xF0

	/*
	TextBlock = TextBlockId type hlen run {run} 0 tlen {AsciiCode} [font block].
	  run = font [name] col voff len.
	*/

	internal static let BufSize = 64


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
		public func append(_ c: CHAR) {
			bytes.append(c)
		}
		
		// ---------------------------------------------------
		public func append(contentsOf chars: [CHAR]) {
			bytes.append(contentsOf: chars)
		}
		
		// ---------------------------------------------------
		public func append(_ chars: [CHAR])
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
		public var off: Int = 0
		public var eot: Bool = false	// Reader has reached end of the text stream.

		// Originally this represented the Color index of last character read, but
		// I'm using it for the column (ie, offset from start of line)
		public var col: Int16 = 0	//


		// Originally this represted "vertical offset" of the last character read,
		// whatever that meant.  I'm taking it to mean 0-based line numbers
		public var voff: Int16 = 0
		public init() { }

		// Scanner properties
		/// Character immediately following the last symbol scanned
		public var nextCh = Character(ascii: 0)
		
		/// != carriage returns scanned so far.
		public var line: Int = 0
		
		/// Scan result: Int, Real, String etc.
		public var `class`: Int = 0

		public var i: Int = 0
		public var x: Float = 0
		public var y: Double = 0
		public var c: CHAR = 0
		
		/// Length of name or string scanned.
		public var len: Int16 = 0
		public var s = ""
		
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
	public static func Pos(_ R: Reader) -> Int {
		return R.off
	}

	// ---------------------------------------------------
	/**
	Write string `s` to `W'`s buffer
	*/
	public static func WriteString(_ W: inout Writer, _ s: [CHAR])
	{
		for c in s
		{
			if c.ascii == 0 { break }
			Write(&W, c)
		}
	}

	// ---------------------------------------------------
	public static func WriteString(_ W: inout Writer, _ s: String)
	{
		var a = [CHAR]()
		a.reserveCapacity(s.count)
		for c in s {
			a.append(CHAR(c))
		}
		WriteString(&W, a)
	}

	// ---------------------------------------------------
	/**
	Write integer `x` to `W'`s buffer. Spaces are padded to the left until the number field is at least `n`
	characters long.
	*/
	public static func WriteInt(_ W: inout Writer, _ x: Int, _ n: Int)
	{
		var intStr = "\(x)"
		let paddingLength = n - Int(intStr.count)
		if paddingLength > 0
		{
			var padding = ""
			
			for _ in 1..<paddingLength {
				padding += " "
			}
			padding.reserveCapacity(padding.count + intStr.count)
			padding.append(intStr)
			intStr = padding
		}
		
		WriteString(&W, intStr)
	}

	// ---------------------------------------------------
	/**
	Write a hexadecimal representation of x to W's buffer.
	*/
	public static func WriteHex(_ W: inout Writer, _ x: Int)
	{
		var a = [CHAR](repeating: 0, count: 10)
		var x = x
		var i = 0
		Write(&W, " ");
		repeat
		{
			let y = x % 0x10
			a[i] = CHAR(integerLiteral: UInt8(y < 10 ? y + 0x30 : y + 0x37))
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
	public static func Write(_ W: inout Writer, _ ch: CHAR) {
		W.buf.append(ch)
	}

	// ---------------------------------------------------
	/**
	Write an end-of-line character to `W`'s buffer.
	*/
	public static func WriteLn(_ W: inout Writer) {
		Write(&W, CHAR("\n"))
	}

	// ---------------------------------------------------
	/**
	Append buffer to the end of text `T`. `B` is emptied.
	*/
	public static func Append(_ T: Text, _ B: inout Buffer)
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
	public static func Read(_ R: inout Reader, _ ch: inout Character)
	{
		guard let T = R.T else
		{
			// If R.T is nil, use stdin
			let c = getchar()
			if c == EOF {
				R.eot = true
				ch = Character(ascii: 0)
			}
			else {
				ch = Character(ascii: c)
			}
			return
		}
		R.eot = R.off >= T.count
		guard !R.eot else {
			ch = Character(ascii: 0)
			return
		}

		// We test the previous character before reading the next so the line
		// number (voff) of a previously read newline remains the same line that it
		// terminates, however, at the very start off the file, there is no
		// previously read character, so line/column numbers need updating
		if R.off > 0
		{
			let lastChar = T[R.off - 1].character
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
		
		ch = T[R.off].character
		R.off += 1
	}

	// ---------------------------------------------------
	/**
	Open text reader `R` and set it up at position `pos` in text `T`.
	*/
	public static func OpenReader(_ R: inout Reader, _ T: Text, _ pos: Int)
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
	public static func OpenWriter(_ W: inout Writer) {
		W = Writer()
	}

	// ---------------------------------------------------
	/**
	Open text scanner `S` and set it up at position `pos` in text `T`.
	*/
	public static func OpenScanner(_ S: inout Scanner, _ T: Text, _ pos: Int)
	{
		OpenReader(&S, T, pos)
		S.line = 0
		S.class = Inval
		var nextChar = Character(ascii: 0)
		Read(&S, &nextChar)
		S.nextCh = nextChar
	}

	// ---------------------------------------------------
	/**
	Open text `T` from file specified by name. A new text is opened when `name = ""`.
	*/
	public static func Open(T: Text, name: [CHAR])
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
		var len: Int
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
	public static func Scan(_ S: inout Scanner)
	{
		// ---------------------------------------------------
		func RealsTen(_ e: Int) -> Double
		{
			guard e >= -307 else { return 0 }
			guard e <= 308 else { return 1 / 0 }
		
			if e == 0 { return 1 }
			
			var result: Double = 10
			
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
		func RealsExpo(_ x: Float) -> Int {
			return Int(x.exponent)
		}
		
		// ---------------------------------------------------
		func RealsExpoL(_ x: Double) -> Int {
			return Int(x.exponent)
		}

		// fixed size: maxD <= LEN(S.s)!
		let maxD = 256
		var	E: Character
		var neg, negE, hex, sign: Bool
		var j, h, e, k, k1, k2, k3: Int
		var y: Double
		var d = [CHAR](repeating: 0, count: maxD)

		var ch = S.nextCh
		var i = 0
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
		let chUpper = Character(ch.uppercased())
		if ("A" <= chUpper)
			&& (chUpper <= "Z")
			|| (ch == ".")
			|| (ch == "/") /*OR (ch = ":")*/
		{
			/*name*/
			repeat
			{
				S.s.append(ch)
				i += 1
				Read(&S, &ch)
			} while !(!(nameChars.contains(ch)) /* || !(S.lib is Fonts.Font) */ || (i == S.s.count - 1))
			
			if (i == 1) && ((S.s.first!.uppercased() < "A") || (S.s.first!.uppercased() > "Z"))
			{
				S.c = CHAR(S.s.first!)
				S.class = Char
			}
			else
			{
				S.len = Int16(i)
				S.class = Name
			}
		}
		else if ch == Character(ascii: 0x22)
		{
			/*literal string*/
			Read(&S, &ch)
			while (ch != Character(ascii: 0x22)) && (ch >= " ") /* && (S.lib is Fonts.Font)  */ && (i != S.s.count - 1)
			{
				S.s.append(ch)
				i += 1
				Read(&S, &ch)
			}
			while (ch != Character(ascii: 0x22)) && (ch >= " ") /* && (S.lib is Fonts.Font) */ {
				Read(&S, &ch)
			}
			S.len = Int16(i)
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
			
			if "0123456789".contains(ch)
			{
				/*number*/
				hex = false
				j = 0
				while true
				{
					d[i] = CHAR(ch)
					i += 1
					Read(&S, &ch)
					if (ch < "0")  || (i >= maxD) {
						break
					}
					if "9" < ch
					{
						if "ABCDEF".contains(ch)
						{
							hex = true
							ch = Character(ascii: ch.asciiValue! - 7)
						}
						else if "abcdef".contains(ch)
						{
							hex = true
							ch = Character(ascii: ch.asciiValue! - 0x27)
						}
						else { break }
					}
				}
				if (ch == "H") /* && (S.lib is Fonts.Font) */
				{
					/*hex number*/
					Read(&S, &ch)
					S.class = Integer
					if i-j > 8 {
						j = i - 8
					}
					k = Int(d[j].ascii - 0x30)
					j += 1
					if (i - j == 7) && (k >= 8) {
						k -= 16
					}
					while j < i
					{
						k = k * 0x10 + Int(d[j].ascii - 0x30)
						j += 1
					}
					
					S.i = neg ? -k : k
				}
				else if (ch == ".")
				{
					/*read real*/
					Read(&S, &ch)
					h = i
					while ("0" <= ch) && (ch <= "9")  && (i < maxD)
					{
						d[i] = CHAR(ch)
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
					func storeDigits(into k: inout Int, j: inout Int, limit: Int)
					{
						let zeroAscii = Int(CHAR("0").ascii)
						while j < limit
						{
							k = k*10 + Int(d[j].ascii) - zeroAscii
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
						if ch == "-"
						{
							negE = true
							Read(&S, &ch)
						}
						else
						{
							negE = false
							if ch == "+" {
								Read(&S, &ch)
							}
						}
						let zeroASCII = UInt8(CHAR("0").ascii)
						while "0123456789".contains(ch)
						{
							e = e * 10 + Int(ch.asciiValue! - zeroASCII)
							Read(&S, &ch)
						}
						if negE {
							e = -e
						}
					}
					y = Double(k3) * RealsTen(-32) + Double(k2) * RealsTen(-24)
					y = y + Double(k1) * RealsTen(-16)
					if abs(e + h) < 308 {
						y = (y + Double(k) * RealsTen(-8)) / RealsTen(-e-h)
					}
					else
					{
						y = (y + Double(k) * RealsTen(-8)) * RealsTen(h)
						if (e <= 308-32) || (e <= 308) && (y < Double.greatestFiniteMagnitude / RealsTen(e))
						{
							y = y * RealsTen(e)
						}
						else {
							y = Double.greatestFiniteMagnitude
						}
					}
					if E == "D"
					{
						if y == Double.greatestFiniteMagnitude {
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
					else if Float.greatestFiniteMagnitude < Float(y) {
						S.class = Inval /* NaN */
					}
					else
					{
						S.class = Real
						S.x = Float(neg ? -y : y)

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
					S.class = Integer
					k = 0
					while (j != i) && ((k < Int.max / 10) || (k == Int.max / 10) && ((Int(d[j].ascii) - 0x30) <= Int.max % 10))
					{ /*JG*/
						k = k*10 + (Int(d[j].ascii) - 0x30)
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
						S.class = hex ? Inval : Integer
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
					S.c = CHAR(ch)
					Read(&S, &ch)
				}
			}
		}
		S.nextCh = ch
	}

	// ---------------------------------------------------
	fileprivate static func initNameChars() -> CharacterSet
	{
		var nameChars = CharacterSet()
		
		// ---------------------------------------------------
		func set(range: ClosedRange<Int>)
		{
			for i in range {
				nameChars.insert(Unicode.Scalar(i)!)
			}
		}
		set(range: 0x80...0x96) // german characters (not really, but in original)
		set(range: Int(CHAR("0").ascii)...Int(CHAR("9").ascii))
		set(range: Int(CHAR("A").ascii)...Int(CHAR("Z").ascii))
		set(range: Int(CHAR("a").ascii)...Int(CHAR("z").ascii))
		nameChars.insert("@") // mail, compiler
		nameChars.insert(".")	// mail, filenames, compiler
		nameChars.insert("/")	// filenames
		nameChars.insert(":")	// filenames (Classic Mac)
		nameChars.insert("_")
		
		return nameChars
	}
}


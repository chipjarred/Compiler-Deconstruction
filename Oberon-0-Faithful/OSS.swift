//
//  OSS.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

public struct OSS
{
	public static let IdLen: INTEGER = 16
	internal static let KW : INTEGER = 34

	/* symbols */
	internal static let null: INTEGER = 0
	public static let times: INTEGER = 1
	public static let div: INTEGER = 3
	public static let mod: INTEGER = 4
	public static let and: INTEGER = 5
	public static let plus: INTEGER = 6
	public static let minus: INTEGER = 7
	public static let or: INTEGER = 8
	public static let eql: INTEGER = 9
	public static let neq: INTEGER = 10
	public static let lss: INTEGER = 11
	public static let geq: INTEGER = 12
	public static let leq: INTEGER = 13
	public static let gtr: INTEGER = 14
	public static let period: INTEGER = 18
	public static let comma: INTEGER = 19
	public static let colon: INTEGER = 20
	public static let rparen: INTEGER = 22
	public static let rbrak: INTEGER = 23
	public static let of: INTEGER = 25
	public static let then: INTEGER = 26
	public static let `do`: INTEGER = 27
	public static let lparen: INTEGER = 29
	public static let lbrak: INTEGER = 30
	public static let not: INTEGER = 32
	public static let becomes: INTEGER = 33
	public static let number: INTEGER = 34
	public static let ident: INTEGER = 37
	public static let semicolon: INTEGER = 38
	public static let end: INTEGER = 40
	public static let `else`: INTEGER = 41
	public static let elsif: INTEGER = 42
	public static let `if`: INTEGER = 44
	public static let `while`: INTEGER = 46
	public static let array: INTEGER = 54
	public static let record: INTEGER = 55
	public static let const: INTEGER = 57
	public static let type: INTEGER = 58
	public static let `var`: INTEGER = 59
	public static let procedure: INTEGER = 60
	public static let begin: INTEGER = 61
	public static let module: INTEGER = 63
	public static let eof: INTEGER = 64

	public typealias Ident = ARRAY<CHAR> /* count = IdLen */
	public static func makeIdent() -> Ident { return Ident(count: IdLen) }

	internal struct KeywordTableEntry: DefaultInitializable
	{
		var sym = INTEGER()
		var id = ARRAY<CHAR>(count: KW)
	}

	public static var val = LONGINT()
	public static var id = makeIdent()
	public static var error = true
	internal static var ch = CHAR()
	internal static var errpos = LONGINT()
	internal static var R = Texts.Reader()
	internal static var W = makeWriter()

	internal static var keyTab = makeKeyWords()
	internal static var nkw: Int { return keyTab.count }

	public static func Mark(_ msg: ARRAY<CHAR>)
	{
		let p = Texts.Pos(R) - 1
		if p > errpos
		{
			Texts.WriteString(&W, " pos ")
			Texts.WriteInt(&W, p, 1)
			Texts.Write(&W, " ")
			Texts.WriteString(&W, msg)
			Texts.WriteLn(&W)
			Texts.Append(OberonLog, &W.buf)
			print(" pos \(p) \(msg)")
		}
		errpos = p;
		error = true
	}

	// Convenience function for emitting better error messages without
	// litering code with construction of ARRAY<CHAR> at the call sites.
	public static func Mark(_ msg: String) {
		Mark(ARRAY<CHAR>(stringLiteral: msg))
	}

	public static func Get(_ sym: inout INTEGER)
	{
		func Ident()
		{
			var i, k: INTEGER
			i = 0
			repeat
			{
				if i < IdLen
				{
					id[i] = ch
					i += 1
				}
				Texts.Read(&R, &ch)
			}
			while !((ch < "0") || ((ch > "9") && (CAP(ch) < "A")) || (CAP(ch) > "Z"))
			id[i] = 0
			k = 0
			while (k < nkw) && (id != keyTab[k].id) {
				k += 1
			}
			if k < nkw {
				sym = keyTab[k].sym
			}
			else {
				sym = ident
			}
		}
		
		func Number()
		{
			val = 0
			sym = number
			repeat
			{
				if val <= (MAX(LONGINT.self) - ORD(ch) + ORD("0")) / 10 {
				  val = 10 * val + (ORD(ch) - ORD("0"))
				}
				else {
					Mark("number too large")
					val = 0
				}
				Texts.Read(&R, &ch)
			}
			while !((ch < "0") || (ch > "9"))
		}
		
		func comment()
		{
			Texts.Read(&R, &ch)
			outer: while true
			{
				inner: while true
				{
					while ch == "("
					{
						Texts.Read(&R, &ch)
						if ch == "*" { comment() }
					}
					if ch == "*"
					{
						Texts.Read(&R, &ch)
						break
					}
					if R.eot {
						break
					}
					Texts.Read(&R, &ch)
				}

				if ch == ")"
				{
					Texts.Read(&R, &ch)
					break
				}
				
				if R.eot
				{
					Mark("comment not terminated")
					break
				}
			}
		}
		
		while !R.eot && (ch <= " ") {
			Texts.Read(&R, &ch)
		}
		
		if R.eot {
			sym = eof
		}
		else
		{
			switch ch
			{
				case "&": Texts.Read(&R, &ch); sym = and
				case "*": Texts.Read(&R, &ch); sym = times
				case "+": Texts.Read(&R, &ch); sym = plus
				case "-": Texts.Read(&R, &ch); sym = minus
				case "=": Texts.Read(&R, &ch); sym = eql
				case "#": Texts.Read(&R, &ch); sym = neq
				case "<":
					Texts.Read(&R, &ch)
					if ch == "="
					{
						Texts.Read(&R, &ch)
						sym = leq
						
					} else {
						sym = lss
					}
				case ">":
					Texts.Read(&R, &ch)
					if ch == "="
					{
						Texts.Read(&R, &ch)
						sym = geq
					}
					else {
						sym = gtr
					}
				case ";": Texts.Read(&R, &ch); sym = semicolon
				case ",": Texts.Read(&R, &ch); sym = comma
				case ":":
					Texts.Read(&R, &ch)
					if ch == "="
					{
						Texts.Read(&R, &ch)
						sym = becomes
					}
					else { sym = colon }
				case ".": Texts.Read(&R, &ch); sym = period
				case "(":
					Texts.Read(&R, &ch)
					if ch == "*" {
						comment()
						Get(&sym)
					}
					else {
						sym = lparen
					}
				case ")": Texts.Read(&R, &ch); sym = rparen
				case "[": Texts.Read(&R, &ch); sym = lbrak
				case "]": Texts.Read(&R, &ch); sym = rbrak
				case "0"..."9": Number()
				case "A"..."Z", "a"..."z": Ident()
				case "~": Texts.Read(&R, &ch); sym = not
				default: Texts.Read(&R, &ch); sym = null
			}
		}
	}

	public static func Init(_ T: Texts.Text, _ pos: LONGINT)
	{
		error = false
		errpos = pos;
		Texts.OpenReader(&R, T, pos);
		Texts.Read(&R, &ch)
	}

	fileprivate static func EnterKW (
		_ sym: INTEGER,
		_ name: ARRAY<CHAR>,
		into keyTab: inout ARRAY<KeywordTableEntry>)
	{
		keyTab.append(KeywordTableEntry(sym: sym, id: name))
	}

	fileprivate static func makeWriter() -> Texts.Writer
	{
		var W = Texts.Writer()
		Texts.OpenWriter(&W)
		return W
	}

	fileprivate static func makeKeyWords() -> ARRAY<KeywordTableEntry>
	{
		var keyTab = ARRAY<KeywordTableEntry>(count: 0)
		keyTab.reserveCapacity(KW)
		
		EnterKW(null, "BY", into: &keyTab)
		EnterKW(`do`, "DO", into: &keyTab)
		EnterKW(`if`, "IF", into: &keyTab)
		EnterKW(null, "IN", into: &keyTab)
		EnterKW(null, "IS", into: &keyTab)
		EnterKW(of, "OF", into: &keyTab)
		EnterKW(or, "OR", into: &keyTab)
		EnterKW(null, "TO", into: &keyTab)
		EnterKW(end, "END", into: &keyTab)
		EnterKW(null, "FOR", into: &keyTab)
		EnterKW(mod, "MOD", into: &keyTab)
		EnterKW(null, "NIL", into: &keyTab)
		EnterKW(`var`, "VAR", into: &keyTab)
		EnterKW(null, "CASE", into: &keyTab)
		EnterKW(`else`, "ELSE", into: &keyTab)
		EnterKW(null, "EXIT", into: &keyTab)
		EnterKW(then, "THEN", into: &keyTab)
		EnterKW(type, "TYPE", into: &keyTab)
		EnterKW(null, "WITH", into: &keyTab)
		EnterKW(array, "ARRAY", into: &keyTab)
		EnterKW(begin, "BEGIN", into: &keyTab)
		EnterKW(const, "CONST", into: &keyTab)
		EnterKW(elsif, "ELSIF", into: &keyTab)
		EnterKW(null, "IMPORT", into: &keyTab)
		EnterKW(null, "UNTIL", into: &keyTab)
		EnterKW(`while`, "WHILE", into: &keyTab)
		EnterKW(record, "RECORD", into: &keyTab)
		EnterKW(null, "REPEAT", into: &keyTab)
		EnterKW(null, "RETURN", into: &keyTab)
		EnterKW(null, "POINTER", into: &keyTab)
		EnterKW(procedure, "PROCEDURE", into: &keyTab)
		EnterKW(div, "DIV", into: &keyTab)
		EnterKW(null, "LOOP", into: &keyTab)
		EnterKW(module, "MODULE", into: &keyTab)
		
		return keyTab
	}
}


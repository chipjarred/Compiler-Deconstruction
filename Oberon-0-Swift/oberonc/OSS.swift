//
//  OSS.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

public struct OSS
{
	public static let IdLen: Int = 16
	internal static let KW : Int = 34

	/* symbols */
	internal static let null: Int = 0
	public static let times: Int = 1
	public static let div: Int = 3
	public static let mod: Int = 4
	public static let and: Int = 5
	public static let plus: Int = 6
	public static let minus: Int = 7
	public static let or: Int = 8
	public static let eql: Int = 9
	public static let neq: Int = 10
	public static let lss: Int = 11
	public static let geq: Int = 12
	public static let leq: Int = 13
	public static let gtr: Int = 14
	public static let period: Int = 18
	public static let comma: Int = 19
	public static let colon: Int = 20
	public static let rparen: Int = 22
	public static let rbrak: Int = 23
	public static let of: Int = 25
	public static let then: Int = 26
	public static let `do`: Int = 27
	public static let lparen: Int = 29
	public static let lbrak: Int = 30
	public static let not: Int = 32
	public static let becomes: Int = 33
	public static let number: Int = 34
	public static let ident: Int = 37
	public static let semicolon: Int = 38
	public static let end: Int = 40
	public static let `else`: Int = 41
	public static let elsif: Int = 42
	public static let `if`: Int = 44
	public static let `while`: Int = 46
	public static let array: Int = 54
	public static let record: Int = 55
	public static let const: Int = 57
	public static let type: Int = 58
	public static let `var`: Int = 59
	public static let procedure: Int = 60
	public static let begin: Int = 61
	public static let module: Int = 63
	public static let eof: Int = 64

	public typealias Ident = String /* count = IdLen */
	internal struct KeywordTableEntry: DefaultInitializable
	{
		var sym = Int()
		var id = ""
	}

	public static var val = Int()
	public static var id = ""
	public static var error = true
	internal static var ch = Character(ascii: 0)
	internal static var errpos = Int()
	internal static var R = Texts.Reader()
	internal static var W = makeWriter()

	internal static var keyTab = makeKeyWords()
	internal static var nkw: Int { return keyTab.count }

	public static func Mark(_ msg: [CHAR])
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

	// ---------------------------------------------------
	// Convenience function for emitting better error messages without
	// litering code with construction of [CHAR] at the call sites.
	public static func Mark(_ msg: String)
	{
		var a = [CHAR]()
		a.reserveCapacity(msg.count)
		
		Mark(a)
	}

	// ---------------------------------------------------
	public static func Get(_ sym: inout Int)
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
		func Ident()
		{
			var i, k: Int
			i = 0
			id.removeAll(keepingCapacity: true)
			repeat
			{
				if i < IdLen
				{
					id.append(ch)
					i += 1
				}
				Texts.Read(&R, &ch)
			}
			while alphaNumeric.contains(ch)

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
		
		// ---------------------------------------------------
		func Number()
		{
			val = 0
			sym = number
			repeat
			{
				let zeroAscii = Character("0").asciiValue!
				let digitValue = Int(ch.asciiValue! - zeroAscii)
				if val <= (Int.max - digitValue) / 10 {
					val = 10 * val + digitValue
				}
				else {
					Mark("number too large")
					val = 0
				}
				Texts.Read(&R, &ch)
			}
			while numeric.contains(ch)
		}
		
		// ---------------------------------------------------
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
		
		// ---------------------------------------------------
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

	public static func Init(_ T: Texts.Text, _ pos: Int)
	{
		error = false
		errpos = pos;
		Texts.OpenReader(&R, T, pos);
		Texts.Read(&R, &ch)
	}
	
	// ---------------------------------------------------
	fileprivate static func EnterKW (
		_ sym: Int,
		_ name: String,
		into keyTab: inout [KeywordTableEntry])
	{
		keyTab.append(KeywordTableEntry(sym: sym, id: name))
	}
	
	fileprivate static func makeWriter() -> Texts.Writer
	{
		var W = Texts.Writer()
		Texts.OpenWriter(&W)
		return W
	}

	// ---------------------------------------------------
	fileprivate static func makeKeyWords() -> [KeywordTableEntry]
	{
		var keyTab = [KeywordTableEntry](repeating: KeywordTableEntry(), count: 0)
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


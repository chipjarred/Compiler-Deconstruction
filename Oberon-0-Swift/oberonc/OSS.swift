//
//  OSS.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct OSS
{
	private static let IdLen: Int = 16
	internal static let KW : Int = 34

	public typealias Ident = String /* count = IdLen */
	internal struct KeywordTableEntry: DefaultInitializable
	{
		var sym = OberonSymbol.null
		var id = ""
	}

	public static var val = Int()
	public static var id = ""
	public static var error = true
	internal static var ch = Character(ascii: 0)
	internal static var errpos = Int()
	internal static var sourceCodeReader = Texts.Reader()
	internal static var diagnosticWriter = makeWriter()

	internal static var keyTab = makeKeyWords()
	internal static var nkw: Int { return keyTab.count }

	// ---------------------------------------------------
	public static func Mark(_ msg: String)
	{
		let p = Texts.Pos(sourceCodeReader) - 1
		if p > errpos
		{
			Texts.WriteString(&diagnosticWriter, " pos ")
			Texts.WriteInt(&diagnosticWriter, p, 1)
			Texts.Write(&diagnosticWriter, " ")
			Texts.WriteString(&diagnosticWriter, msg)
			Texts.WriteLn(&diagnosticWriter)
			Texts.Append(OberonLog, &diagnosticWriter.buf)
			print(" pos \(p) \(msg)")
		}
		errpos = p;
		error = true
	}

	// ---------------------------------------------------
	public static func Get(_ sym: inout OberonSymbol)
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
				ch = sourceCodeReader.readCharacter()!
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
				sym = .ident
			}
		}
		
		// ---------------------------------------------------
		func Number()
		{
			val = 0
			sym = .number
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
				ch = sourceCodeReader.readCharacter()!
			}
			while numeric.contains(ch)
		}
		
		// ---------------------------------------------------
		func comment()
		{
			ch = sourceCodeReader.readCharacter()!
			outer: while true
			{
				inner: while true
				{
					while ch == "("
					{
						ch = sourceCodeReader.readCharacter()!
						if ch == "*" { comment() }
					}
					if ch == "*"
					{
						ch = sourceCodeReader.readCharacter()!
						break
					}
					if sourceCodeReader.eot {
						break
					}
					ch = sourceCodeReader.readCharacter()!
				}

				if ch == ")"
				{
					ch = sourceCodeReader.readCharacter()!
					break
				}
				
				if sourceCodeReader.eot
				{
					Mark("comment not terminated")
					break
				}
			}
		}
		
		// ---------------------------------------------------
		while !sourceCodeReader.eot && (ch <= " ") {
			ch = sourceCodeReader.readCharacter()!
		}
		
		if sourceCodeReader.eot {
			sym = .eof
		}
		else
		{
			switch ch
			{
				case "0"..."9": Number(); return
				case "A"..."Z", "a"..."z": Ident(); return
				default: break
			}
			
			let nullCharacter = Character(ascii: 0)
			guard var c = sourceCodeReader.readCharacter() else
			{
				Mark("Unexpected end of input")
				sym = .null
				ch = nullCharacter
				return
			}
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
						c = sourceCodeReader.readCharacter() ?? nullCharacter
						sym = .leq
						
					} else {
						sym = .lss
					}
				case ">":
					if c == "="
					{
						c = sourceCodeReader.readCharacter() ?? nullCharacter
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
						c = sourceCodeReader.readCharacter() ?? nullCharacter
						sym = .becomes
					}
					else { sym = .colon }
				case ".": sym = .period
				case "(":
					if c == "*" {
						comment()
						Get(&sym)
						return
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
		}
	}

	// ---------------------------------------------------
	public static func Init(_ T: Texts.Text, _ pos: Int)
	{
		error = false
		errpos = pos;
		Texts.OpenReader(&sourceCodeReader, T, pos);
		ch = sourceCodeReader.readCharacter()!
	}
	
	// ---------------------------------------------------
	fileprivate static func EnterKW (
		_ sym: OberonSymbol,
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
		
		EnterKW(.null, "BY", into: &keyTab)
		EnterKW(.`do`, "DO", into: &keyTab)
		EnterKW(.`if`, "IF", into: &keyTab)
		EnterKW(.null, "IN", into: &keyTab)
		EnterKW(.null, "IS", into: &keyTab)
		EnterKW(.of, "OF", into: &keyTab)
		EnterKW(.or, "OR", into: &keyTab)
		EnterKW(.null, "TO", into: &keyTab)
		EnterKW(.end, "END", into: &keyTab)
		EnterKW(.null, "FOR", into: &keyTab)
		EnterKW(.mod, "MOD", into: &keyTab)
		EnterKW(.null, "NIL", into: &keyTab)
		EnterKW(.`var`, "VAR", into: &keyTab)
		EnterKW(.null, "CASE", into: &keyTab)
		EnterKW(.`else`, "ELSE", into: &keyTab)
		EnterKW(.null, "EXIT", into: &keyTab)
		EnterKW(.then, "THEN", into: &keyTab)
		EnterKW(.type, "TYPE", into: &keyTab)
		EnterKW(.null, "WITH", into: &keyTab)
		EnterKW(.array, "ARRAY", into: &keyTab)
		EnterKW(.begin, "BEGIN", into: &keyTab)
		EnterKW(.const, "CONST", into: &keyTab)
		EnterKW(.elsif, "ELSIF", into: &keyTab)
		EnterKW(.null, "IMPORT", into: &keyTab)
		EnterKW(.null, "UNTIL", into: &keyTab)
		EnterKW(.`while`, "WHILE", into: &keyTab)
		EnterKW(.record, "RECORD", into: &keyTab)
		EnterKW(.null, "REPEAT", into: &keyTab)
		EnterKW(.null, "RETURN", into: &keyTab)
		EnterKW(.null, "POINTER", into: &keyTab)
		EnterKW(.procedure, "PROCEDURE", into: &keyTab)
		EnterKW(.div, "DIV", into: &keyTab)
		EnterKW(.null, "LOOP", into: &keyTab)
		EnterKW(.module, "MODULE", into: &keyTab)
		
		return keyTab
	}
}


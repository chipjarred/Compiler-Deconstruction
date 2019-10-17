//
//  OSS.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Oberon
import Texts

public let IdLen: INTEGER = 16
internal let KW : INTEGER = 34

/* symbols */
internal let null: INTEGER = 0
public let times: INTEGER = 1
public let div: INTEGER = 3
public let mod: INTEGER = 4
public let and: INTEGER = 5
public let plus: INTEGER = 6
public let minus: INTEGER = 7
public let or: INTEGER = 8
public let eql: INTEGER = 9
public let neq: INTEGER = 10
public let lss: INTEGER = 11
public let geq: INTEGER = 12
public let leq: INTEGER = 13
public let gtr: INTEGER = 14
public let period: INTEGER = 18
public let comma: INTEGER = 19
public let colon: INTEGER = 20
public let rparen: INTEGER = 22
public let rbrak: INTEGER = 23
public let of: INTEGER = 25
public let then: INTEGER = 26
public let `do`: INTEGER = 27
public let lparen: INTEGER = 29
public let lbrak: INTEGER = 30
public let not: INTEGER = 32
public let becomes: INTEGER = 33
public let number: INTEGER = 34
public let ident: INTEGER = 37
public let semicolon: INTEGER = 38
public let end: INTEGER = 40
public let `else`: INTEGER = 41
public let elsif: INTEGER = 42
public let `if`: INTEGER = 44
public let `while`: INTEGER = 46
public let array: INTEGER = 54
public let record: INTEGER = 55
public let const: INTEGER = 57
public let type: INTEGER = 58
public let `var`: INTEGER = 59
public let procedure: INTEGER = 60
public let begin: INTEGER = 61
public let module: INTEGER = 63
public let eof: INTEGER = 64

public typealias Ident = ARRAY<CHAR> /* count = IdLen */
internal func makeIdent() -> Ident { return Ident(count: IdLen) }

internal struct KeywordTableEntry: DefaultInitializable
{
	var sym = INTEGER()
	var id = ARRAY<CHAR>(count: KW)
}

public var val = LONGINT()
public var id = makeIdent()
public var error = BOOLEAN()
internal var ch = CHAR()
internal var nkw = INTEGER()
internal var errpos = LONGINT()
internal var R = Texts.Reader()
internal var W = Texts.Writer()
internal var keyTab = ARRAY<KeywordTableEntry>(count: KW)

public func Mark(_ msg: ARRAY<CHAR>)
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
	}
	errpos = p;
	error = true
}

public func Get(_ sym: inout INTEGER)
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

public func Init(_ T: Texts.Text, _ pos: LONGINT)
{
	/*
	Presumably Oberon's runtime calls the module's main code after loading the
	module (or at least on the first access to anything in the module).  While
	Swift globals initialization does something similar, its approach is to
	initialize globals when they are first accessed.  We could hook into that
	mechanism to simulate Oberon's runtime, but a more straight-forward method
	is just to call the initialization in the Init, because presumably it must
	be called before doing anything in the module.
	*/
	_moduleInit()
	
	error = false
	errpos = pos;
	Texts.OpenReader(&R, T, pos);
	Texts.Read(&R, &ch)
}

internal func EnterKW (_ sym: INTEGER, _ name: ARRAY<CHAR>)
{
	keyTab[nkw].sym = sym
	COPY(name, &keyTab[nkw].id)
	nkw += 1
}

fileprivate var moduleInitialized = false
fileprivate func _moduleInit()
{
	/*
	It is assumed that Oberon's module main code would be called just once, so
	we guard with a boolean to simulate that.
	*/
	guard !moduleInitialized else { return }
	defer { moduleInitialized = true }
	
	Texts.OpenWriter(&W)
	error = true
	nkw = 0
	EnterKW(null, "BY")
	EnterKW(`do`, "DO")
	EnterKW(`if`, "IF")
	EnterKW(null, "IN")
	EnterKW(null, "IS")
	EnterKW(of, "OF")
	EnterKW(or, "OR")
	EnterKW(null, "TO")
	EnterKW(end, "END")
	EnterKW(null, "FOR")
	EnterKW(mod, "MOD")
	EnterKW(null, "NIL")
	EnterKW(`var`, "VAR")
	EnterKW(null, "CASE")
	EnterKW(`else`, "ELSE")
	EnterKW(null, "EXIT")
	EnterKW(then, "THEN")
	EnterKW(type, "TYPE")
	EnterKW(null, "WITH")
	EnterKW(array, "ARRAY")
	EnterKW(begin, "BEGIN")
	EnterKW(const, "CONST")
	EnterKW(elsif, "ELSIF")
	EnterKW(null, "IMPORT")
	EnterKW(null, "UNTIL")
	EnterKW(`while`, "WHILE")
	EnterKW(record, "RECORD")
	EnterKW(null, "REPEAT")
	EnterKW(null, "RETURN")
	EnterKW(null, "POINTER")
	EnterKW(procedure, "PROCEDURE")
	EnterKW(div, "DIV")
	EnterKW(null, "LOOP")
	EnterKW(module, "MODULE")
}

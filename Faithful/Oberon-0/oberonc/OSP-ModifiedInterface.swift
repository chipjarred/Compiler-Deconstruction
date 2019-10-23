//
//  NonOberonInterfaceFunctions.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/19/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Oberon
import Texts
import OSS
import OSG

// ---------------------------------------------------
fileprivate func printLog()
{
	print(
		"\n\n ---- Oberon Log ----\n"
		+ "\(OberonLog?.description ?? "-- EMPTY LOG --")"
	)
	OberonLog?.clear()
}

public let magic: LONGINT = 0x656e7472 // "entr"
public var program: ARRAY<LONGINT>
{
	let objCode = OSG.getObjectCode()
	var program = [LONGINT]()
	program.reserveCapacity(objCode.count + 2)
	program.append(magic)
	program.append(OSG.entry * 4)
	program.append(contentsOf: objCode)
	return ARRAY(program)
}

// ---------------------------------------------------
/**
Compile Oberon-0 code from a `String`
*/
public func Compile(source: String)
{
	defer { printLog() }
	let sourceCode: Text = TextDesc(source)
	var S = Texts.Scanner()

	Texts.OpenScanner(&S, sourceCode, 0)
	OSS.Init(sourceCode, 0)
	OSS.Get(&sym)
	Module(&S)
}

// ---------------------------------------------------
public func Decode() -> String
{
	defer { printLog() }
	var result: Text = TextDesc()
	OSG.Decode(&result)
	let r = result?.description ?? "!!!!! NO OUTPUT !!!!!"
	return r
}

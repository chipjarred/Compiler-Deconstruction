//
//  OSP-ModifiedInterface.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/19/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

// ---------------------------------------------------
fileprivate func printLog()
{
	print(
		"\n\n ---- Oberon Log ----\n"
		+ "\(OberonLog?.description ?? "-- EMPTY LOG --")"
	)
	OberonLog?.clear()
}

// ---------------------------------------------------
public extension OSP
{
	static var magic: UInt32 {
		return UInt32(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	static var program: ARRAY<UInt32>
	{
		let objCode = OSG.getObjectCode()
		var program = [UInt32]()
		program.reserveCapacity(objCode.count + 2)
		program.append(OSP.magic)
		program.append(UInt32(OSG.entry * 4))
		program.append(contentsOf: objCode)
		return ARRAY(program)
	}

	// ---------------------------------------------------
	/**
	Compile Oberon-0 code from a `String`
	*/
	static func Compile(source: String)
	{
		defer { printLog() }
		let sourceCode: Texts.Text = Texts.TextDesc(source)
		var S = Texts.Scanner()

		Texts.OpenScanner(&S, sourceCode, 0)
		OSS.Init(sourceCode, 0)
		OSS.Get(&sym)
		Module(&S)
	}

	// ---------------------------------------------------
	static func Decode() -> String
	{
		defer { printLog() }
		var result: Texts.Text = Texts.TextDesc()
		OSG.Decode(&result)
		let r = result?.description ?? "!!!!! NO OUTPUT !!!!!"
		return r
	}
}


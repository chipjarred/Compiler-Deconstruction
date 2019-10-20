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
	return result!.description
}

// ---------------------------------------------------
public func Load(programInputs: String)
{
	defer { printLog() }
	
	let inputs: Text = TextDesc(programInputs)
	var S = Texts.Scanner()
	
	if !OSS.error && !loaded
	{
		Texts.OpenScanner(&S, inputs, 0)
		OSG.Load(&S)
		loaded = true
	}
}

// ---------------------------------------------------
public func Exec(programInputs: String)
{
	defer { printLog() }
	
	let inputs: Text = TextDesc(programInputs)
	var S = Texts.Scanner()
	
	if loaded
	{
		Texts.OpenScanner(&S, inputs, 0)
		Texts.Scan(&S)
		if S.class == Texts.Name {
			OSG.Exec(&S)
		}
	}
}

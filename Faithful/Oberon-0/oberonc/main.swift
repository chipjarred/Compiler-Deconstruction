//
//  main.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

import Oberon
import Texts
import OSP

var useSymbolFiles: Bool { return false }

// ---------------------------------------------------
fileprivate func Help()
{
	let helpStr = useSymbolFiles
	?
	"""
	Oberon-0 compiler v1.0

	Usage: oberonc outputFolder sourceFile1.Mod[+] sourceFile2.Mod[+] ...

	It compiles the list of provided source module files and places the generated
	classes in the existing 'outputFolder'.
	The optional suffix '+' allows the compiler to create a new symbol file. If this
	option is not specified, a change in the interface of the module results in a
	compilation error.
	"""
	:
	"""
	Oberon-0 compiler v1.0

	Usage: oberonc outputFolder sourceFile1.Mod sourceFile2.Mod ...

	It compiles the list of provided source module files and places the generated
	classes in the existing 'outputFolder'.
	"""

	print(helpStr)
}

// ---------------------------------------------------
fileprivate func save(data: Data, to fileName: String, in outputFolder: URL)
{
	let outputURL = URL(fileURLWithPath: fileName, relativeTo: outputFolder)
	
	let fileMgr = FileManager.default
	guard fileMgr.createFile(
		atPath: outputURL.path,
		contents: data,
		attributes: nil
	)
	else
	{
		print("Error writing to \(fileName), with URL, \(outputURL)")
		print("Aborting...")
		exit(-1)
	}
}

// ---------------------------------------------------
fileprivate func save(
	disassembly: String,
	to fileName: String,
	in outputFolder: URL)
{
	save(data: disassembly.data(using: .utf8)!, to: fileName, in: outputFolder)
}


// ---------------------------------------------------
fileprivate func save(
	binary: ARRAY<LONGINT>,
	to fileName: String,
	in outputFolder: URL)
{
	save(data: binary.data, to: fileName, in: outputFolder)
}


// ---------------------------------------------------
/*
- FIXME: This argument processing is pretty lame.  We don't check file
extensions, for example.  We also don't use the "+" suffixes (which for a Unixy
world, really should be a command line option. 
*/
if CommandLine.argc < 2 {
	Help()
}
else
{
	enum argProcessingState
	{
		case thisExecutableName
		case outputFolder
		case sourceFiles
	}
	var argState = argProcessingState.thisExecutableName
	var outputFolderName: String = "."
	var sourceFiles: [(source: String, createSymbolName: Bool)] = []
	
	for arg in CommandLine.arguments
	{
		switch argState
		{
			case .thisExecutableName:
				argState = .outputFolder
			
			case .outputFolder:
				outputFolderName = arg
				argState = .sourceFiles
			
			case .sourceFiles:
				var fileName = arg
				var createSymbolFile = false
				if useSymbolFiles, fileName.hasSuffix("+")
				{
					fileName.removeLast()
					createSymbolFile = true
				}
				sourceFiles.append((fileName, createSymbolFile))
		}
	}
	
	let outputFolderURL =
		URL(fileURLWithPath: outputFolderName, isDirectory: true)
	
	// Note we don't current use createSymbolFile
	for (sourceFile, _) in sourceFiles
	{
		let sourceFileURL = URL(fileURLWithPath: sourceFile)
		guard let sourceCode = try? String(contentsOf: sourceFileURL) else
		{
			print("Unable to read source file, \"\(sourceFile)\".  Aborting...")
			exit(-1)
		}
		OSP.Compile(source: sourceCode)
	}
	
	save(disassembly: OSP.Decode(), to: "a.asm", in: outputFolderURL)
	save(binary: OSP.program, to: "a.risc", in: outputFolderURL)
}

exit(0)

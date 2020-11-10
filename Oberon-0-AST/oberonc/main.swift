// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

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
	binary: [UInt32],
	to fileName: String,
	in outputFolder: URL)
{
	var data = Data()
	binary.withUnsafeBufferPointer { data.append($0) }
	save(data: data, to: fileName, in: outputFolder)
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
	print("Oberon-0 compiler v1.")
	
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
	
	if sourceFiles.count == 0
	{
		print("Must specify at least one source file")
		exit(-1)
	}
	else if sourceFiles.count > 1
	{
		print(
			"Sorry, this poor little compiler only supports compiling one"
			+ " source file.  You weren't expecting to write a real program in"
			+ " such a limited language as Oberon-0 were you?"
		)
		exit(-1)
	}
	
	// Note we don't currently use createSymbolFile
	let compiler = Compiler()
	
	var program: [UInt32] = []
	var disassembly: String = ""
	
	for (sourceFile, _) in sourceFiles
	{
		let sourceFileURL = URL(fileURLWithPath: sourceFile)
		guard let sourceCode = try? String(contentsOf: sourceFileURL) else
		{
			print("Unable to read source file, \"\(sourceFile)\".  Aborting...")
			exit(-1)
		}
		
		guard let (prog, listing) = compiler.compile(
			source: sourceCode,
			sourceName: sourceFileURL.path
		)
		else
		{
			print("Compilation failed with \(compiler.errorCount) errors.")
			exit(-1)
		}
		
		program = prog
		disassembly = listing
	}
		
	save(disassembly: disassembly, to: "a.asm", in: outputFolderURL)
	save(binary: program, to: "a.risc", in: outputFolderURL)
}

exit(0)

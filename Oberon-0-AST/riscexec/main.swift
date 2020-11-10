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

// ---------------------------------------------------
fileprivate func Help()
{
	let helpStr =
	"""
	RISC Binary Runner v1.0

	Usage: riscexec filename.risc

	Executes a binary generated by oberonc in Oberon's RISC emulator.
	"""

	print(helpStr)
}

// ---------------------------------------------------
/*
- FIXME: This argument processing is pretty lame.  We don't check file
extensions, for example.  We also don't use the "+" suffixes (which for a Unixy
world, really should be a command line option.
*/
if CommandLine.argc != 2 {
	Help()
}
else
{
	let binaryName = CommandLine.arguments[1]
	
	guard let codeData = try? Data(contentsOf: URL(fileURLWithPath: binaryName))
	else
	{
		print("Unable to read file, \(binaryName)")
		exit(-1)
	}
	
	guard codeData.count % MemoryLayout<Int>.size == 0 else
	{
		print("\(binaryName) has improper length to be a RISC binary")
		exit(-1)
	}
	
	var code = codeData.withUnsafeBytes {
		return [UInt32]($0.bindMemory(to: UInt32.self))
	}
	guard code[0] == Compiler.magic else {
		print("Invalid program signature.  Aborting...")
		exit(-1)
	}
	let entry = code[1]
	code.removeFirst(2) // Strip off magic and entry point entries
	var emulator = RISCEmulator()
	emulator.load(code, Int(code.count))
	
	print("\(binaryName) loaded.")
	
	var scanner = RISCInputScanner()
	emulator.execute(entry, input: &scanner)
}

exit(0)

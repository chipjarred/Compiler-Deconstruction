// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

// ----------------------------------
public class Compiler
{
	private let reporter: ErrorReporter
	private var codeGenerator: CodeGenerator? = nil
	
	public var errorCount: Int { return reporter.errorCount }
	
	// ----------------------------------
	public init(errorsTo reporter: ErrorReporter? = nil) {
		self.reporter = reporter ?? ErrorReporter(FileHandle.standardError)!
	}
	
	// ----------------------------------
	public func compile(source: String, sourceName: String = "<<NONE>>")
		-> (program: [UInt32], disassembly: String)?
	{
		let parser = NewParser(
			source: source,
			sourceName: sourceName,
			errorsTo: reporter
		)
		
		let ast = TypeChecker(errorsTo: reporter).check(parser.parse())
		
		codeGenerator = CodeGenerator(errorsTo: reporter)
		guard let program = codeGenerator!.generate(from: ast) else {
			return nil
		}
		
		let disassembly = codeGenerator!.disassemble()
		
		return (
			program: addMagic(to: program, entryPoint: codeGenerator!.entry),
			disassembly: disassembly
		)
	}
	
	// ---------------------------------------------------
	/// Program signature/marker
	public static var magic: UInt32 {
		return UInt32(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	/**
	Adds magic signature and entry point to the beginning of the program binary.
	*/
	private func addMagic(
		to rawBinary: [UInt32],
		entryPoint: UInt32) -> [UInt32]
	{
		var program = [UInt32](capacity: rawBinary.count + 2)
		program.append(Compiler.magic)
		program.append(entryPoint)
		program.append(contentsOf: rawBinary)
		return program
	}

}

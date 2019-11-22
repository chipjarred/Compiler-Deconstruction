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
class CodeGenerator: CompilerPhase
{
	private var code = [UInt8]()

	// ---------------------------------------------------
	public override init(errorsTo reporter: ErrorReporter) {
		super.init(errorsTo: reporter)
	}

	// ---------------------------------------------------
	public final func generate(from ast: AbstractSyntaxTree) -> [UInt8]
	{
		code = [UInt8]()
		
		return code
	}
	
	// ---------------------------------------------------
	private func generate(startingAt node: ASTNode)
	{
		switch node.kind
		{
			case .variableDeclaration:
				allocateVariableStorage(for: node)
			
			case .varSection, .codeBlock:
				for child in node.children {
					generate(startingAt: child)
				}
			
			case .procedureDeclaration:
				fallthrough
			case .moduleDeclaration:
				generate(startingAt: node.varSection)
				generate(startingAt: node.body)

			default: break
		}
	}
	
	// ---------------------------------------------------
	private func allocateVariableStorage(for declaration: ASTNode)
	{
		guard let symbolInfo = declaration.symbolInfo else
		{
			let varName = declaration.children[0]
			emitError(
				"Undefined symbol, \(varName.name).",
				at: varName.sourceLocation
			)
			return
		}
		
		if symbolInfo.level < 2 {
			allocateGlobalVariableStorage(for: symbolInfo)
		}
		else {
			allocateLocalVariableStorage(for: symbolInfo)
		}
	}
	
	// ---------------------------------------------------
	private func allocateGlobalVariableStorage(for symbolInfo: SymbolInfo)
	{
		fatalError("Unimplemented")
	}
	
	// ---------------------------------------------------
	private func allocateLocalVariableStorage(for symbolInfo: SymbolInfo)
	{
		fatalError("Unimplemented")
	}
}

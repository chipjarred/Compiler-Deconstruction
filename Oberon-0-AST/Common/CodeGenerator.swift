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
	private var codeGenImpl: RISCCodeGenerator

	// ---------------------------------------------------
	public override init(errorsTo reporter: ErrorReporter)
	{
		self.codeGenImpl = RISCCodeGenerator()
		super.init(errorsTo: reporter)
	}

	// ---------------------------------------------------
	public final func generate(from ast: AbstractSyntaxTree) -> [UInt32]
	{
		generateProgram(ast.root)
		
		return codeGenImpl.code
	}
	
	// ---------------------------------------------------
	/**
	Disassemble a compiled Oberon-0 program to a `String`
	*/
	public final func disassemble() -> String
	{
		var result = ""
		codeGenImpl.decode(to: &result)
		return result
	}

	// ---------------------------------------------------
	private func generateProgram(_ node: ASTNode)
	{
		assert(node.kind == .program)
		
		for module in node.children {
			generateModule(for: module)
		}
	}
	
	// ---------------------------------------------------
	private func generateModule(for node: ASTNode)
	{
		assert(node.kind == .moduleDeclaration)
		
		codeGenImpl.open()
		
		var declarationSize = generateGlobalVarSection(for: node.varSection)
		declarationSize += generateProcedures(node.procedureList)
		
		codeGenImpl.header(declarationSize)
		let _ = generateCodeBlock(for: node.body)
		
		codeGenImpl.close()
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by global variable declarations
	*/
	private func generateGlobalVarSection(for node: ASTNode) -> Int
	{
		assert(node.kind == .varSection)
		assert(node.parent!.kind == .moduleDeclaration)
		assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
		
		var declarationsSize = 0
		
		for varDeclaration in node.children
		{
			declarationsSize +=
				generateGlobalVariableDeclaration(for: varDeclaration)
		}
		
		return declarationsSize
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by the global variable declaration
	*/
	private func generateGlobalVariableDeclaration(for node: ASTNode) -> Int
	{
		assert(node.kind == .variableDeclaration)
		assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
		
		return node.symbolInfo.type!.size
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by procedure code
	*/
	private func generateProcedures(_ procedures: [ASTNode]) -> Int
	{
		for procedure in procedures
		{
			assert(procedure.kind == .procedureDeclaration)
		}
		
		return 0
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Bytes used code generated by the code block
	*/
	private func generateCodeBlock(for node: ASTNode) -> Int
	{
		let startingAddress = codeGenImpl.code.count
		
		return (codeGenImpl.code.count - startingAddress) * MemoryLayout<UInt32>.stride
	}
}

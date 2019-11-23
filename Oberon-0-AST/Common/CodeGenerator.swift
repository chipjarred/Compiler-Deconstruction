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
		
		var declarationSize = allocateGlobalVarSection(for: node.varSection)
		declarationSize += generateProcedures(node.procedureList)
		
		codeGenImpl.header(declarationSize)
		let _ = generateCodeBlock(for: node.body)
		
		codeGenImpl.close()
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by global variable declarations
	*/
	private func allocateGlobalVarSection(for node: ASTNode) -> Int
	{
		assert(node.kind == .varSection)
		assert(node.parent!.kind == .moduleDeclaration)
		assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
		
		var declarationsSize = 0
		
		for varDeclaration in node.children
		{
			declarationsSize +=
				allocateGlobalVariableDeclaration(for: varDeclaration)
			varDeclaration.symbolInfo.value  = -declarationsSize
		}
		
		return declarationsSize
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by the global variable declaration
	*/
	private func allocateGlobalVariableDeclaration(for node: ASTNode) -> Int
	{
		assert(node.kind == .variableDeclaration)
		assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
		
		node.symbolInfo.level = node.scope.depth
		return node.symbolInfo.type!.size
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by all procedure code
	*/
	private func generateProcedures(_ procedures: [ASTNode]) -> Int
	{
		var proceduresSize = 0
		for procedure in procedures
		{
			assert(procedure.kind == .procedureDeclaration)
			proceduresSize += generateProcedure(procedure)
		}
		
		return proceduresSize
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by procedure code
	*/
	private func generateProcedure(_ procedure: ASTNode) -> Int
	{
		assert(procedure.kind == .procedureDeclaration)
		
		codeGenImpl.IncLevel(1)

		let procInfo = procedure.children[0].symbolInfo!
		
		let paramSize = allocateFormalParameters(procInfo.type!.fields)
		let localVarSize = allocateLocalVarSection(for: procedure.varSection)
		let _ = generateProcedures(procedure.procedureList)
		
		procInfo.value = codeGenImpl.pc
		codeGenImpl.enter(localVarSize)
		
		let _ = generateCodeBlock(for: procedure.body)
		
		codeGenImpl.procedureReturn(paramSize)
		codeGenImpl.IncLevel(-1)
		
		return 0
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by formal parameters
	*/
	private func allocateFormalParameters(_ parameters: [SymbolInfo]) -> Int
	{
		var parametersSize = 0
		
		for parameter in parameters {
			parametersSize += parameter.type!.size
		}
		
		return parametersSize
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by local variables
	*/
	private func allocateLocalVarSection(for node: ASTNode) -> Int
	{
		assert(node.kind == .varSection)
		assert(node.parent!.kind == .procedureDeclaration)
		assert(node.scope.depth > 0)
		
		var declarationsSize = 0
		
		for varDeclaration in node.children
		{
			declarationsSize +=
				allocateLocalVariableDeclaration(for: varDeclaration)
			varDeclaration.symbolInfo.value  = -declarationsSize
		}
		
		return declarationsSize
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Number of bytes used by the local variable declaration
	*/
	private func allocateLocalVariableDeclaration(for node: ASTNode) -> Int
	{
		assert(node.kind == .variableDeclaration)
		assert(node.scope.depth > 0, "Scope depth is \(node.scope.depth)")
		
		node.symbolInfo.level = node.scope.depth
		return node.symbolInfo.type!.size
	}
	
	// ---------------------------------------------------
	/**
	- Returns: Bytes used code generated by the code block
	*/
	private func generateCodeBlock(for node: ASTNode) -> Int
	{
		assert(node.kind == .codeBlock)
		
		let startIndex = codeGenImpl.pc
		
		for statement in node.children {
			generateStatement(statement)
		}
		
		let endIndex = codeGenImpl.pc
		return (endIndex - startIndex) * MemoryLayout<UInt32>.stride
	}
	
	// ---------------------------------------------------
	private func generateStatement(_ statement: ASTNode)
	{
		assert(statement.isStatement)
		
		switch statement.kind
		{
			case .assignment:
				generateAssignment(statement)
			
			default:
				emitError(
					"Cannot generate code for \(statement.srcStr)",
					at: statement.sourceLocation
				)
		}
	}
	
	// ---------------------------------------------------
	private func generateAssignment(_ assignment: ASTNode)
	{
		assert(assignment.kind == .assignment)
		
		let destination = assignment.children[0]
		assert(destination.isAssignable)
		
		let source = assignment.children[1]
		assert(source.isExpression)
		
		guard var left = makeOperand(from: destination) else {
			return
		}
		
		var right = generateExpression(source)
		
		emitErrorOnThrow {
			try codeGenImpl.emitAssignment(into: &left, from: &right)
		}
	}
	
	// ---------------------------------------------------
	private func generateExpression(_ expression: ASTNode) -> RISCOperand
	{
		switch expression.kind
		{
			case .constant:
				return codeGenImpl.makeConstItem(
					expression.symbolInfo.type,
					expression.value
				)
				
			case .variable:
				return makeOperand(from: expression.symbolInfo)
			
			case .unaryOperator:
				return generateUnaryOperation(expression)
			
			default: break
		}
		
		#warning("Implement code generation of general expressions")
		fatalError()
	}
	
	// ---------------------------------------------------
	private func generateUnaryOperation(_ operation: ASTNode) -> RISCOperand
	{
		guard var operand = makeOperand(from: operation.children[0]) else {
			return codeGenImpl.makeDefaultOperand()
		}
		
		switch operation.symbol
		{
			case .unaryPlus, .unaryMinus, .not:
				emitErrorOnThrow
				{
					try codeGenImpl.emitUnaryExpression(
						operation.symbol,
						&operand
					)
				}
			
			default:
				emitError(
					"Cannot generated code for unary operatator, "
					+ "\"\(operation.srcStr)\".",
					at: operation.sourceLocation
				)
				
		}

		return operand
	}
	
	// ---------------------------------------------------
	private func makeOperand(from node: ASTNode) -> RISCOperand?
	{
		switch node.kind
		{
			case .constant:
				return codeGenImpl.makeConstItem(
					node.symbolInfo.type,
					node.value
				)
				
			case .variable:
				return makeOperand(from: node.symbolInfo)
				
			case .arrayElement:
				guard let arrayOp = makeOperand(from: node.children[0]) else {
					break
				}
				
				let indexOp = generateExpression(node.children[1])
				return makeArrayElementOperand(arrayOp, index: indexOp)
			
			case .recordField:
				guard var recordField = makeOperand(from: node.children[0])
				else { break }
				
				recordField.setFieldInfo(from: node.children[1].symbolInfo)
				return recordField
				
			default:
				emitError(
					"Unable to generate operand from \(node.kind)",
					at: node.sourceLocation
				)
		}
		
		return nil
	}
	
	// ---------------------------------------------------
	private func makeArrayElementOperand(
		_ array: RISCOperand,
		index: RISCOperand) -> RISCOperand
	{
		assert(array.type!.form == .array)
		
		var array = array
		
		emitErrorOnThrow {
			try array.index(at: index, for: &codeGenImpl)
		}
		
		return array
	}
	
	// ---------------------------------------------------
	private func makeOperand(from symbolInfo: SymbolInfo) -> RISCOperand
	{
		do
		{
			switch symbolInfo.kind
			{
				case .constant:
					return codeGenImpl.makeConstItem(
						symbolInfo.type!,
						symbolInfo.value
					)
				
				case .variable:
					return try codeGenImpl.makeOperand(symbolInfo)
				
				default:
					emitError(
						"Unable to generate code",
						at: symbolInfo.sourceLocation
					)
			}
		}
		catch { emitErrorOnThrow { throw error } }
		
		return codeGenImpl.makeDefaultOperand()
	}
	
	// ---------------------------------------------------
	private func emitErrorOnThrow(for block: () throws -> Void)
	{
		do { return try block() }
		catch {
			emitError(error.localizedDescription)
		}
	}
}

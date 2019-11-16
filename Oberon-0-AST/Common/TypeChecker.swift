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

// ---------------------------------------------------
class TypeChecker
{
	
	private static let nullArrayInfo:TypeInfo = {
		let result = TypeInfo(form: .array, size: 0)
		result.base = TypeInfo.integer
		return result
	}()
	
	private let errorReporter: ErrorReporter
	
	// ---------------------------------------------------
	init(errorsTo reporter: ErrorReporter) {
		self.errorReporter = reporter
	}
	
	// ---------------------------------------------------
	public final func check(_ ast: AbstractSyntaxTree) {
		ast.traverse { self.check($0) }
	}
	
	// ---------------------------------------------------
	private func check(_ node: ASTNode)
	{
		switch node.kind
		{
			// Declarations
			case .moduleDeclaration: declareModule(node)
			case .procedureDeclaration: declareProcedure(node)
			case .constantDeclaration: declareConstant(node)
			case .typeDeclaration: 	declareType(node)
			case .variableDeclaration, .valueParam, .referenceParam:
				declareVariable(node)
			
			// Type specifications
			case .typeName: setTypeNameType(node)
			case .typeSpec: setTypeSpecType(node)
			case .array: setArrayType(node)
			case .record: setRecordType(node)
			case .fieldDeclaration: declareField(node)
			
			// Expressions
			case .variable: setVariableType(node)
			case .constant: setConstantType(node)
			case .unaryOperator: setUnaryOperatorType(node)
			case .binaryOperator: setBinaryOperatorType(node)
			case .functionCall: setFunctionCallType(node)
			
			// Statements
			case .assignment: checkAssignment(node)
			case .ifStatement, .whileStatement: checkIfOrWhileStatement(node)

			default: break
		}
	}
	
	// MARK:- Declarations
	// ---------------------------------------------------
	private func declareModule(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .moduleDeclaration)
		
		let moduleInfo = declareSymbol(
			from: node,
			as: .head,
			in: node.parent!
		)
		
		moduleInfo.value = -1
		moduleInfo.ownedScope = node.scope
	}
	
	// ---------------------------------------------------
	private func declareConstant(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .constSection)
		assert(node.kind == .constantDeclaration)
		assert(node.children.count == 2)
		
		let name = node.children[0]
		let value = node.children[1]

		let constInfo = declareSymbol(from: name, as: .constant, in: node)
		
		if value.kind == .constant
		{
			constInfo.type = value.typeInfo
			constInfo.value = value.value
		}
		else if value.kind == .variable
		{
			guard let valueInfo = node.scope[value.name],
				valueInfo.kind == .constant
			else
			{
				emitError(
					"Expected constant expression",
					at: value.sourceLocation
				)
				return
			}
			
			constInfo.type = valueInfo.type
			constInfo.value = valueInfo.value
		}
		
		name.kind = .constant
		name.value = constInfo.value
		name.symbolInfo = constInfo
		
		node.typeInfo = TypeInfo.void
	}
	
	// ---------------------------------------------------
	private func declareType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .typeSection)
		assert(node.kind == .typeDeclaration)
		assert(node.children.count == 2)
		
		let newTypeName = node.children[0]
		let existingTypeName = node.children[1]
		
		let typeNameInfo = declareSymbol(
			from: newTypeName,
			as: .type,
			in: node
		)
		
		typeNameInfo.type = existingTypeName.typeInfo
		newTypeName.symbolInfo = typeNameInfo
		newTypeName.kind = .typeSpec
	}

	// ---------------------------------------------------
	private func declareVariable(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(
			node.parent!.kind == .varSection
			|| node.parent!.kind == .procedureDeclaration
		)
		assert(
			node.kind == .valueParam
			|| node.kind == .referenceParam
			|| node.kind == .variableDeclaration
		)
		
		let variableInfo = declareSymbol(
			from: node,
			as: node.kind == .referenceParam ? .parameter : .variable,
			in: node
		)
		
		variableInfo.type = node.children[0].typeInfo
		node.symbolInfo = variableInfo
	}
	
	// ---------------------------------------------------
	private func declareField(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .record)
		assert(node.kind == .fieldDeclaration)
		assert(node.children.count == 1)
		
		let fieldName = node.name
		let fieldType = constructTypeInfo(from: node.children[0])
		
		node.symbolInfo = SymbolInfo(name: fieldName, type: fieldType)
	}
	
	// ---------------------------------------------------
	private func declareProcedure(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .procedureDeclaration)
		
		let procedureInfo = declareSymbol(
			from: node,
			as: .procedure,
			in: node.parent!
		)
		
		procedureInfo.value = -1
		procedureInfo.ownedScope = node.scope
		
		/*
		We currently don't support value-returning procedures (ie. function
		procedures), so for now we just set the return type to void.
		*/
		procedureInfo.type = constructProcedureDeclInfo(
			from: node,
			returnType: TypeInfo.void
		)
		
		node.symbolInfo = procedureInfo
	}

	
	// MARK:- type specifications
	// ---------------------------------------------------
	private func setArrayType(_ node: ASTNode) {
		node.typeInfo = constructArrayTypeInfo(from: node)
	}
	
	// ---------------------------------------------------
	private func setRecordType(_ node: ASTNode) {
		node.typeInfo = constructRecordTypeInfo(from: node)
	}
	
	// ---------------------------------------------------
	private func setTypeNameType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .typeDeclaration)
		assert(node.kind == .typeName)
	}
	
	// ---------------------------------------------------
	private func setTypeSpecType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .typeSpec)
		
		if let symInfo = node.scope.hierarchy[node.name],
			symInfo.kind == .type
		{
			node.symbolInfo = symInfo
		}
		else
		{
			emitError(
				"Undefined type, \"\(node.name)\"",
				at: node.sourceLocation
			)
		}
	}

	// MARK:- Expression type checking
	// ---------------------------------------------------
	private func setVariableType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .variable)
		
		if let symInfo = node.scope.hierarchy[node.name]
		{
			node.symbolInfo = symInfo
			
			if symInfo.kind == .constant {
				node.kind = .constant
			}
		}
		else
		{
			switch node.parent!.kind
			{
				case .constantDeclaration, .variableDeclaration: break
					
				default:
					emitError(
						"Undefined symbol, \"\(node.name)\"",
						at: node.sourceLocation
					)
			}
		}
	}
	
	// ---------------------------------------------------
	private func setConstantType(_ node: ASTNode)
	{
		assert(node.kind == .constant)
		
		// Oberon-0 only supports integer and boolean literals, and the boolean
		// literals, TRUE and FALSE, are handled as identifiers defined when
		// the global scope is created, so we only need to handle integers (ie.
		// numbers) here.
		assert(node.symbol == .number)
		
		node.typeInfo = TypeInfo.integer
		node.value = node.token.value
	}
	
	// ---------------------------------------------------
	private func setUnaryOperatorType(_ node: ASTNode)
	{
		assert(node.kind == .unaryOperator)
		assert(node.children.count == 1)
		
		let operand = node.children[0]
		
		switch node.symbol
		{
			case .not: setNotType(operand, into: node)
			case .unaryPlus: setUnaryPlusType(operand, into: node)
			case .unaryMinus: setUnaryMinusType(operand, into: node)
			
			default:
				emitError(
					"Unknown unary operator, \(node.symbol)",
					at: node.sourceLocation
				)
				
				node.typeInfo = TypeInfo.integer
		}
	}
	
	// ---------------------------------------------------
	private func setNotType(_ operand: ASTNode, into result: ASTNode)
	{
		guard expectType(is: TypeInfo.boolean, for: operand) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if operand.kind == .constant
		{
			result.value = operand.value == 0 ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setUnaryMinusType(_ operand: ASTNode, into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: operand) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if operand.kind == .constant
		{
			result.value = -operand.value
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setUnaryPlusType(_ operand: ASTNode, into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: operand) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if operand.kind == .constant
		{
			result.value = operand.value
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.integer
	}

	// ---------------------------------------------------
	private func setBinaryOperatorType(_ node: ASTNode)
	{
		assert(node.kind == .binaryOperator)
		assert(node.children.count == 2)
		
		let left = node.children[0]
		let right = node.children[1]
		
		switch node.symbol
		{
			// Boolean operators
			case .and: setAndType(left, right, into: node)
			case .or: setOrType(left, right, into: node)
			
			// Arithmetic operators
			case .plus: setPlusType(left, right, into: node)
			case .minus: setMinusType(left, right, into: node)
			case .times: setMultiplyType(left, right, into: node)
			case .div: setDivideType(left, right, into: node)
			case .mod: setModulusType(left, right, into: node)
			
			// Comparison operators
			case .isEqualTo: setIsEqualType(left, right, into: node)
			case .isNotEqualTo: setIsNotEqualType(left, right, into: node)
			case .lessThan: setLessThanType(left, right, into: node)
			case .greaterThan: setGreaterThanType(left, right, into: node)
			
			case .lessThanOrEqualTo:
				setLessThanOrEqualType(left, right, into: node)
			case .greaterThanOrEqualTo:
				setGreaterThanType(left, right, into: node)
			
			default:
				emitError(
					"Unknown binary operator, \(node.symbol)",
					at: node.sourceLocation
				)
				node.typeInfo = TypeInfo.integer
		}
	}
	
	// ---------------------------------------------------
	private func setAndType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.boolean, for: left, right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right {
			result.parent?.replace(child: result, with: left)
		}
		else if left.kind == .constant
		{
			if left.value == 0
			{
				result.kind = .constant
				result.value = 0
			}
			else { result.parent?.replace(child: result, with: right) }
		}
		else if right.kind == .constant
		{
			if right.value == 0
			{
				if isPureValue(left)
				{
					result.kind = .constant
					result.value = 0
				}
			}
			else { result.parent?.replace(child: result, with: left) }
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setOrType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.boolean, for: left, right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right {
			result.parent?.replace(child: result, with: left)
		}
		else if left.kind == .constant
		{
			if left.value != 0
			{
				result.kind = .constant
				result.value = 1
			}
			else { result.parent?.replace(child: result, with: right) }
		}
		else if right.kind == .constant
		{
			if right.value != 0
			{
				if isPureValue(left)
				{
					result.kind = .constant
					result.value = 1
				}
			}
			else { result.parent?.replace(child: result, with: left) }
		}

		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setPlusType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: left, right) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if left.kind == .constant
		{
			if left.value == 0 {
				result.parent?.replace(child: result, with: right)
			}
			else if right.kind == .constant
			{
				result.value = left.value + right.value
				result.kind = .constant
			}
		}
		else if right.kind == .constant
		{
			if right.value == 0 {
				result.parent?.replace(child: result, with: left)
			}
		}
		
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setMinusType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: left, right) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 0
			result.kind = .constant
		}
		else if left.kind == .constant
		{
			if left.value == 0
			{
				let unaryMinus = ASTNode(token: result.token)
				unaryMinus.kind = .unaryOperator
				unaryMinus.addChild(right)
				setUnaryOperatorType(unaryMinus)
				
				result.parent?.replace(child: result, with: unaryMinus)
			}
			else if right.kind == .constant
			{
				result.value = left.value - right.value
				result.kind = .constant
			}
		}
		else if right.kind == .constant
		{
			if right.value == 0 {
				result.parent?.replace(child: result, with: left)
			}
		}
		
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setMultiplyType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: left, right) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if left.kind == .constant
		{
			if left.value == 1 {
				result.parent?.replace(child: result, with: right)
			}
			else if right.kind == .constant
			{
				result.value = left.value * right.value
				result.kind = .constant
			}
		}
		else if right.kind == .constant
		{
			if right.value == 1 {
				result.parent?.replace(child: result, with: left)
			}
		}
		
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setDivideType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: left, right) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 1
			result.kind = .constant
		}
		else if left.kind == .constant
		{
			if right.kind == .constant
			{
				result.value = left.value / right.value
				result.kind = .constant
			}
		}
		else if right.kind == .constant
		{
			if right.value == 1 {
				result.parent?.replace(child: result, with: left)
			}
		}
		
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setModulusType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard expectType(is: TypeInfo.integer, for: left, right) else
		{
			result.typeInfo = TypeInfo.integer
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 0
			result.kind = .constant
		}
		else if left.kind == .constant
		{
			if right.kind == .constant
			{
				result.value = left.value % right.value
				result.kind = .constant
			}
		}
		else if right.kind == .constant
		{
			if right.value == 1 && isPureValue(left) {
				result.parent?.replace(child: result, with: left)
			}
		}
			
		result.typeInfo = TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func setIsEqualType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == TypeInfo.boolean || left.typeInfo == .integer
		else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 1
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value == right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setIsNotEqualType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == TypeInfo.boolean || left.typeInfo == .integer
		else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 0
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value != right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setLessThanType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == .integer else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 0
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value < right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setLessThanOrEqualType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == .integer else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 1
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value <= right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setGreaterThanType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == .integer else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 0
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value > right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setGreaterThanOrEqualType(
		_ left: ASTNode,
		_ right: ASTNode,
		into result: ASTNode)
	{
		guard left.typeInfo == .integer else
		{
			emitError(
				"Cannot use type \(left.typeInfo!) with \"\(result.symbol)\" "
				+ "operator",
				at: left.sourceLocation
			)
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		guard expectType(is: left.typeInfo, for: right) else
		{
			result.typeInfo = TypeInfo.boolean
			return
		}
		
		if isPureValue(left) && left === right
		{
			result.value = 1
			result.kind = .constant
		}
		else if left.kind == .constant, right.kind == .constant
		{
			result.value = left.value >= right.value ? 1 : 0
			result.kind = .constant
		}
		
		result.typeInfo = TypeInfo.boolean
	}
	
	// ---------------------------------------------------
	private func setFunctionCallType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .functionCall)
		
		guard let procInfo = node.scope[node.name] else
		{
			emitError("Unknown function, \(node.name)", at: node.sourceLocation)
			node.typeInfo = TypeInfo.void
			return
		}
		
		node.typeInfo = procInfo.type?.base ?? TypeInfo.void
		
		let formalParams = procInfo.type?.fields ?? []
		let actualParams = node.children
		
		guard formalParams.count == actualParams.count else
		{
			emitError(
				"Expected \(formalParams.count) parameters, but got "
				+ "\(actualParams.count)",
				at: node.sourceLocation
			)
			return
		}
		
		for (formalParam, actualParam) in zip(formalParams, actualParams) {
			if !expectType(is: formalParam.type!, for: actualParam) { return }
		}
	}
	
	// MARK:- Statement type checking
	// ---------------------------------------------------
	private func checkAssignment(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.kind == .assignment)
		assert(node.children.count == 2)
		
		let left = node.children[0]
		let right = node.children[1]
		
		if !left.isAssignable
		{
			emitError(
				"Illegal assignment destination",
				at: left.sourceLocation
			)
			return
		}
		
		if !right.isExpression {
			emitError("Expected experssion", at: right.sourceLocation)
		}
		
		let _ = expectType(is: left.typeInfo, for: right)
		
		node.typeInfo = TypeInfo.void
	}
	
	// ---------------------------------------------------
	private func checkIfOrWhileStatement(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(
			(node.kind == .ifStatement && node.children.count == 3)
			|| (node.kind == .whileStatement && node.children.count == 2)
		)
		
		let condition = node.children[0]
		
		// ---------------------------------------------------
		if condition.typeInfo != TypeInfo.boolean
		{
			emitError(
				"Expected boolean expression for \(node.name) condition",
				at: condition.sourceLocation
			)
		}
		
		node.typeInfo = TypeInfo.void
		return
	}

	// MARK:- TypeInfo construction
	// ---------------------------------------------------
	private func constructTypeInfo(from node: ASTNode) -> TypeInfo
	{
		assert(node.isTypeSpec)
		
		switch node.kind
		{
			case .typeSpec: return node.typeInfo
			case .array: return constructArrayTypeInfo(from: node)
			case .record: return constructRecordTypeInfo(from: node)
			
			default:
				emitError(
					"Illegal type specifier, \"\(node)\"",
					at: node.sourceLocation
				)
		}
		
		return TypeInfo.integer
	}
	
	// ---------------------------------------------------
	private func constructArrayTypeInfo(from node: ASTNode) -> TypeInfo
	{
		assert(node.kind == .array)
		assert(node.children.count == 2)
		
		let elementCountNode = node.children[0]
		let elementTypeNode = node.children[1]
		
		// We don't currently support run-time sized arrays.  Array sizes have
		// to be known at compile-time.
		let elementCount: Int
		if elementCountNode.kind == .constant {
			elementCount = elementCountNode.value
		}
		else
		{
			emitError(
				"Expected compile-time constant for array index",
				at: elementCountNode.sourceLocation
			)
			elementCount = 0
		}
		
		let elementType: TypeInfo
		if let elemType = elementTypeNode.typeInfo {
			elementType = elemType
		}
		else
		{
			emitError(
				"Expected array element type specifier",
				at: elementTypeNode.sourceLocation
			)
			elementType = TypeInfo.integer
		}
		
		let arrayInfo = TypeInfo(
			form: .array,
			size: elementCount * elementType.size
		)
		arrayInfo.len = elementCount
		arrayInfo.base = elementType

		return arrayInfo
	}
	
	// ---------------------------------------------------
	private func constructRecordTypeInfo(from node: ASTNode) -> TypeInfo
	{
		assert(node.kind == .record)

		let recordInfo = TypeInfo(form: .record, size: 0)
		
		for field in node.recordFields
		{
			assert(field.kind == .fieldDeclaration)
			recordInfo.addField(from: field.symbolInfo)
		}
		
		return recordInfo
	}
	
	// ---------------------------------------------------
	private func constructProcedureDeclInfo(
		from node: ASTNode,
		returnType: TypeInfo) -> TypeInfo
	{
		assert(node.kind == .procedureDeclaration)
		
		let procInfo = TypeInfo(form: .procedure, size: 0)
		
		for param in node.parameters
		{
			assert(param.kind == .valueParam || param.kind == .referenceParam)
			procInfo.addField(from: param.symbolInfo)
		}
		
		procInfo.base = returnType
		
		return procInfo
	}
	
	// MARK:- Type checking / symbol declaration utilities
	// ---------------------------------------------------
	private func declareSymbol(
		from node: ASTNode,
		as kind: SymbolInfo.Kind,
		in scopeNode: ASTNode) -> SymbolInfo
	{
		do
		{
			return try node.scope.defineSymbol(
				named: node.name,
				kind: kind
			)
		}
		catch SymbolScope.Error.duplicateSymbolDefinition(let existingInfo)
		{
			emitDuplicateSymbolError(for: node, existingInfo: existingInfo)
			return existingInfo
		}
		catch { unexpectedError(error) }
	}
	
	// ---------------------------------------------------
	/**
	A "pure" value is one that does not have side-effects, which allows for some simple optimizations and
	value reductions that might not otherwise be allowed.
	
	For example, traditional short-circuiting in an AND operation means that if the left operand is false, the
	right operand never evaluated, because the expression is known to be false at that point, BUT if the right
	operand is false, even though we know that the expression will evaluate to false, we still have to evaluate
	the left expression, because it may have side-effects that must be allowed to happen.  However, if we
	know that the left expression does not have side effects and always gives the same value, even if that
	value is not known at compile time (ie. is a pure value), then we could skip evaluating the left expression
	anyway.
	
	This is useful in arithmetic operations as well.  For example if x is pure, then x - x can be replaced with 0.
	If it's not pure, then x could return different values, or have side effects that depend on its being evaluated
	twice in that expression, so the optimization can't be done in that case.
	*/
	private func isPureValue(_ node: ASTNode) -> Bool {
		return node.kind == .variable || node.kind == .constant
	}
	
	// MARK:- Diagnostic reporting
	// ---------------------------------------------------
	private func emitError(
		_ message: String,
		at location: SourceLocation? = nil)
	{
		if let location = location {
			errorReporter.mark(message, at: location)
		}
		else { errorReporter.mark(message) }
	}
	
	// ---------------------------------------------------
	private func emitDuplicateSymbolError(
		for node: ASTNode,
		existingInfo: SymbolInfo)
	{
		emitError(
			"The symbol, \"\(node.name)\" is already defined",
			at: node.sourceLocation
		)
		emitError(
			"\tprevious declaration is here.",
			at: existingInfo.sourceLocation
		)
	}
	
	// ---------------------------------------------------
	private func unexpectedError(_ error: Error) -> Never
	{
		fatalError(
			"Unexpected error: \(error): \(error.localizedDescription)"
		)
	}
	
	// ---------------------------------------------------
	private func expectType(
		is expectedType: TypeInfo,
		for nodes: ASTNode...) -> Bool
	{
		for node in nodes
		{
			if !node.type(is: expectedType)
			{
				emitError(
					"Expected expression of type, \(expectedType), but got "
					+ "expression of type, \(node.typeInfo!)",
					at: node.sourceLocation
				)
				return false
			}
		}
		
		return true
	}
}

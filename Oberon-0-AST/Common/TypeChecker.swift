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
			case .moduleDeclaration:
				declareModule(node)
			
			case .procedureDeclaration:
				declareProcedure(node)
			
			case .variableDeclaration, .valueParam, .referenceParam:
				declareVariable(node)
			
			case .constantDeclaration:
				declareConstant(node)
			
			case .typeDeclaration:
				declareType(node)
			
			case .array:
				setArrayType(node)
			
			case .record:
				setRecordType(node)
			
			case .fieldDeclaration:
				declareField(node)
			
			case .variable:
				setVariableType(node)
			
			case .constant:
				setConstantType(node)
			
			case .unaryOperator:
				setUnaryOperatorType(node)
			
			case .binaryOperator:
				setBinaryOperatorType(node)
			
			default: break
		}
	}
	
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
		assert(node.parent!.kind == .nodeList)
		assert(node.kind == .constantDeclaration)
		assert(node.children.count == 2)
		
		let constInfo = declareSymbol(
			from: node.children[0],
			as: .constant,
			in: node
		)
		
		constInfo.type = TypeInfo.integer
		
		let typeSpecNode = node.children[1]
		constInfo.value = expectType(is: TypeInfo.integer, for: typeSpecNode)
			? typeSpecNode.value
			: 0
	}
	
	// ---------------------------------------------------
	private func declareType(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .nodeList)
		assert(node.kind == .typeDeclaration)
		assert(node.children.count == 2)
		
		let typeNameInfo = declareSymbol(
			from: node.children[0],
			as: .type,
			in: node
		)
		
		typeNameInfo.type = node.children[1].typeInfo
	}

	// ---------------------------------------------------
	private func declareVariable(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .nodeList)
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
	}
	
	// ---------------------------------------------------
	private func declareField(_ node: ASTNode)
	{
		assert(node.parent != nil)
		assert(node.parent!.kind == .nodeList)
		assert(node.kind == .fieldDeclaration)
		assert(node.children.count == 2)
		
		let fieldName = node.children[0].name
		let fieldType = constructTypeInfo(from: node.children[1])
		
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
	}
	
	// ---------------------------------------------------
	private func setArrayType(_ node: ASTNode) {
		node.typeInfo = constructArrayTypeInfo(from: node)
	}
	
	// ---------------------------------------------------
	private func setRecordType(_ node: ASTNode) {
		node.typeInfo = constructRecordTypeInfo(from: node)
	}

	// ---------------------------------------------------
	private func setVariableType(_ node: ASTNode)
	{
		assert(node.kind == .variable)
		
		guard let symInfo = node.scope[node.name] else
		{
			emitError(
				"Undefined symbol, \"\(node.name)\"",
				at: node.sourceLocation
			)
			return
		}
		
		if symInfo.kind == .constant {
			node.kind = .constant
		}
		
		node.symbolInfo = symInfo
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
	}
	
	// ---------------------------------------------------
	private func setUnaryOperatorType(_ node: ASTNode)
	{
		assert(node.kind == .unaryOperator)
		assert(node.children.count == 1)
		
		// Oberon-0 only supports ~ (boolean NOT) as unary operator
		guard node.symbol == .not else
		{
			emitError(
				"Unknown unary operator, \(node.symbol)",
				at: node.sourceLocation
			)
			
			node.typeInfo = TypeInfo.integer
			return
		}
		
		let operand = node.children[0]
		
		switch node.symbol
		{
			case .not: setNotType(operand, into: node)
			case .plus: setUnaryPlusType(operand, into: node)
			case .minus: setUnaryMinusType(operand, into: node)
			
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

	// ---------------------------------------------------
	private func constructTypeInfo(from node: ASTNode) -> TypeInfo
	{
		assert(node.isTypeSpec)
		
		if node.kind == .typeName
		{
			if let symbolInfo = node.scope[node.name]
			{
				assert(symbolInfo.type != nil)
				return symbolInfo.type!
			}
			else
			{
				emitError(
					"Unknown type, \"\(node.name)\"",
					at: node.sourceLocation
				)
			}
		}
		else if node.kind == .array {
			return constructArrayTypeInfo(from: node)
		}
		else if node.kind == .record {
			return constructRecordTypeInfo(from: node)
		}
		else {
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
		
		let elementCount: Int
		if let elemCount = node.children[0].symbolInfo?.value {
			elementCount = elemCount
		}
		else
		{
			emitError(
				"Expected compile-time constant for array index",
				at: node.children[0].sourceLocation
			)
			elementCount = 0
		}
		
		let elementType: TypeInfo
		if let elemType = node.children[1].typeInfo {
			elementType = elemType
		}
		else
		{
			emitError(
				"Expected array element type specifier",
				at: node.children[1].sourceLocation
			)
			elementType = TypeInfo.integer
		}
		
		let arrayInfo = TypeInfo(
			form: .array,
			size: elementCount * elementType.size
		)
		arrayInfo.len = elementCount

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
	
	This is useful arithmetic operations as well.  For example if x is pure, then x - x can be replaced with 0.
	If it's not pure, then x could return different values, or have side effects that depend on its being evaluated
	twice in that expression, so the optimization can't be done in that case.
	*/
	private func isPureValue(_ node: ASTNode) -> Bool
	{
		return node.kind == .variable || node.kind == .constant
	}
	
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
}

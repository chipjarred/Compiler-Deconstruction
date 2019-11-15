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
/**
Abstract syntax tree node.
*/
// ----------------------------------
public class ASTNode: CustomStringConvertible
{
	// ----------------------------------
	public enum Kind
	{
		case empty
		case constant
		case variable
		case arrayElement
		case recordField
		case binaryOperator
		case unaryOperator
		case function
		case codeBlock
		case assignment
		case ifStatement
		case whileStatement
		case typeName
		case variableDeclaration
		case fieldDeclaration
		case constantDeclaration
		case typeDeclaration
		case procedureDeclaration
		case moduleDeclaration
		case nodeList
		case array
		case record
		case varSection
		case constSection
		case typeSection
		case valueParam
		case referenceParam
		case program
		
		// ----------------------------------
		var isTypeSpec: Bool {
			return self == .typeName || self == .array || self == .record
		}
		
		// ----------------------------------
		var isSection: Bool
		{
			return self == .codeBlock
				|| self == .varSection
				|| self == .constSection
				|| self == .typeSection
		}
	}
	
	public var kind: Kind
	public private(set) var token: Token
	public private(set) var children: [ASTNode] = []
	public weak var parent: ASTNode? = nil
		
	// ----------------------------------
	public private(set) final lazy var scope: SymbolScope =
	{
		assert(kind == .program || parent != nil)
		
		switch kind
		{
			case .program:
				return SymbolScope.makeGlobalScope()
			
			case .moduleDeclaration, .procedureDeclaration:
				return parent!.scope.openScope()
			
			default:
				return parent!.scope
		}
	}()
	
	public final var symbol: TokenType { return token.symbol }

	public final var srcStr: String { return token.srcString }
	public final var sourceLocation: SourceLocation? {
		return token.sourceRange.lowerBound
	}
	
	public final var symbolInfo: SymbolInfo! = nil
	
	// ----------------------------------
	public var typeInfo: TypeInfo!
	{
		// ----------------------------------
		get
		{
			guard let symInfo = symbolInfo else { return nil }
			return symInfo.type
		}
		
		// ----------------------------------
		set
		{
			if symbolInfo == nil {
				symbolInfo = SymbolInfo(type: newValue)
			}
			else {
				symbolInfo.type = newValue
			}
		}
	}

	// ----------------------------------
	public final var isStatement: Bool {
		return kind == .assignment || kind == .function
	}
	
	public final var isSection: Bool { return kind.isSection }
	
	public final var isTypeSpec: Bool { return kind.isTypeSpec }
	
	// ----------------------------------
	public final var isArrayIndexable: Bool
	{
		switch kind
		{
			case .variable,
				 .arrayElement,
				 .recordField: return true
				
			default: return false
		}
	}
	
	// ----------------------------------
	public final var isFieldSelectable: Bool { return isArrayIndexable }
	
	// ----------------------------------
	public final var isExpression: Bool
	{
		switch kind
		{
			case .variable,
				 .arrayElement,
				 .recordField,
				 .constant,
				 .function,
				 .binaryOperator,
				 .unaryOperator: return true
			
			default: return false
		}
	}
	
	// ----------------------------------
	public final var isAssignable: Bool
	{
		switch kind
		{
			case .variable,
				 .arrayElement,
				 .recordField: return true
			
			default: return false
		}
	}
	
	// ----------------------------------
	public final var isScope: Bool
	{
		switch kind
		{
			case .moduleDeclaration,
				 .procedureDeclaration: return true
				
			default: return false
		}
	}
	
	// ----------------------------------
	public final var name: String
	{
		switch kind
		{
			case .typeName, .variable, .valueParam, .referenceParam:
				return token.identifier
			
			case .procedureDeclaration, .moduleDeclaration:
				return children[0].srcStr
			
			default:
				assertionFailure(
					"Don't know how to extract name for ASTNode, \(self)"
				)
		}
		
		return ""
	}
	
	// ----------------------------------
	public final var value: Int
	{
		// ----------------------------------
		get
		{
			assert(self.isExpression)
			return token.value
		}
		// ----------------------------------
		set
		{
			assert(self.isExpression)
			token.value = newValue
		}
	}
	
	// ----------------------------------
	public final var parameters: [ASTNode]
	{
		assert(self.kind == .procedureDeclaration)
		return children[6].children
	}
	
	// ----------------------------------
	public final var constSection: ASTNode
	{
		assert(self.isScope)
		return children[1]
	}

	// ----------------------------------
	public final var typeSection: ASTNode
	{
		assert(self.isScope)
		return children[2]
	}
	
	// ----------------------------------
	public final var varSection: ASTNode
	{
		assert(self.isScope)
		return children[3]
	}
	
	// ----------------------------------
	public final var procedureList: [ASTNode]
	{
		assert(self.isScope)
		return children[4].children
	}
	
	// ----------------------------------
	public final var body: ASTNode
	{
		assert(self.isScope)
		return children[5]
	}
	
	// ----------------------------------
	public var arrayElementCount: ASTNode
	{
		assert(self.kind == .array)
		return children[0]
	}
	
	// ----------------------------------
	public var arrayElementType: ASTNode
	{
		assert(self.kind == .array)
		return children[1]
	}
	
	public var recordFields: [ASTNode]
	{
		assert(self.kind == .record)
		return children[0].children
	}
	
	// ----------------------------------
	public func type(is aType: TypeInfo) -> Bool {
		return typeInfo == aType
	}
	
	// MARK:- Initializers
	// ----------------------------------
	convenience init(token: Token, child: ASTNode? = nil)
	{
		self.init(token: token, kind: ASTNode.kind(from: token))
		if let child = child {
			self.addChild(child)
		}
	}
	
	// ----------------------------------
	convenience init(array: ASTNode, bracket: Token, index: ASTNode)
	{
		assert(array.isArrayIndexable)
		assert(bracket.symbol == .openBracket)
		assert(index.isExpression)
		
		self.init(token: bracket, kind: .arrayElement)
		
		self.children.reserveCapacity(2)
		self.addChild(array)
		self.addChild(index)
	}
	
	// ----------------------------------
	convenience init(record: ASTNode, dot: Token, field: ASTNode)
	{
		assert(record.isFieldSelectable)
		assert(dot.symbol == .period)
		assert(field.kind == .variable)
		
		self.init(token: dot, kind: .recordField)
		
		self.children.reserveCapacity(2)
		self.addChild(record)
		self.addChild(field)
	}
	
	// ----------------------------------
	convenience init(function: Token, parameters: [ASTNode])
	{
		assert(function.symbol == .identifier)
		self.init(token: function, kind: .function)
		self.addChildren(parameters)
	}
	
	// ----------------------------------
	convenience init(block: Token, statements: [ASTNode])
	{
		assert(
			block.symbol == .begin
			|| block.symbol == .then
			|| block.symbol == .else
			|| block.symbol == .do
		)
		self.init(token: block, kind: .codeBlock)
		self.addChildren(statements)
	}
	
	// ----------------------------------
	convenience init(assignment: Token, lvalue: ASTNode, rvalue: ASTNode)
	{
		assert(assignment.symbol == .becomes)
		self.init(token: assignment, kind: .assignment)
		self.addChildren([lvalue, rvalue])
	}
	
	// ----------------------------------
	convenience init(typeName: Token)
	{
		assert(typeName.symbol == .identifier)
		self.init(token: typeName, kind: .typeName)
	}
	
	// ----------------------------------
	convenience init(variable: Token, ofType typeSpec: ASTNode)
	{
		assert(variable.symbol == .identifier)
		assert(typeSpec.kind.isTypeSpec)
		
		self.init(token: variable, kind: .variableDeclaration)
		addChild(typeSpec.clone())
	}

	// ----------------------------------
	convenience init(variable: Token, sameTypeAs varDeclaration: ASTNode)
	{
		assert(variable.symbol == .identifier)
		assert(varDeclaration.kind == .variableDeclaration)
		assert(varDeclaration.children.count == 1)
		
		self.init(token: variable, kind: .variableDeclaration)
		addChild(varDeclaration.children.first!.clone())
	}
	
	// ----------------------------------
	convenience init(
		constantNamed identifier: ASTNode,
		equalsToken: Token,
		value: ASTNode)
	{
		assert(equalsToken.symbol == .isEqualTo)
		assert(identifier.kind == .variable && value.kind == .constant)
		
		self.init(token: equalsToken, kind: .constantDeclaration)
		addChildren([identifier, value])
	}
	
	// ----------------------------------
	convenience init(
		typeNamed identifier: ASTNode,
		equalsToken: Token,
		value: ASTNode)
	{
		assert(equalsToken.symbol == .isEqualTo)
		assert(identifier.kind == .typeName && value.kind.isTypeSpec)
		
		self.init(token: equalsToken, kind: .typeDeclaration)
		addChildren([identifier, value])
	}
	
	// ----------------------------------
	convenience init(
		procedure: Token,
		named name: Token,
		parameters: [ASTNode],
		constSection: ASTNode,
		typeSection: ASTNode,
		varSection: ASTNode,
		procedures: [ASTNode],
		body: ASTNode)
	{
		assert(procedure.symbol == .procedure)
		
		#if DEBUG
		for param in parameters {
			assert(param.kind == .valueParam || param.kind == .referenceParam)
		}
		#endif
		
		self.init(
			procedureOrModule: procedure,
			named: name,
			constSection: constSection,
			typeSection: typeSection,
			varSection: varSection,
			procedures: procedures,
			body: body
		)
		
		// The reason I add the parameters last is so I can use the same layout
		// for modules, which have all the same components, except for
		// parameters.
		self.addChild(ASTNode(listOf: parameters))
	}
	
	// ----------------------------------
	convenience init(
		module: Token,
		named name: Token,
		constSection: ASTNode,
		typeSection: ASTNode,
		varSection: ASTNode,
		procedures: [ASTNode],
		body: ASTNode)
	{
		assert(module.symbol == .module)
		
		self.init(
			procedureOrModule: module,
			named: name,
			constSection: constSection,
			typeSection: typeSection,
			varSection: varSection,
			procedures: procedures,
			body: body
		)
	}

	// ----------------------------------
	private convenience init(
		procedureOrModule marker: Token,
		named name: Token,
		constSection: ASTNode,
		typeSection: ASTNode,
		varSection: ASTNode,
		procedures: [ASTNode],
		body: ASTNode)
	{
		assert(marker.symbol == .procedure || marker.symbol == .module)
		assert(name.symbol == .identifier)
		assert(constSection.kind == .constSection)
		assert(typeSection.kind == .typeSection)
		assert(varSection.kind == .varSection)
		assert(body.kind == .codeBlock)
		
		#if DEBUG
		for proc in procedures {
			assert(proc.kind == .procedureDeclaration)
		}
		#endif
		
		self.init(
			token: marker,
			kind: marker.symbol == .procedure
				? .procedureDeclaration
				: .moduleDeclaration
		)
		
		let numChildren = marker.symbol == .procedure ? 7 : 6
		self.children.reserveCapacity(numChildren)
		
		self.addChild(ASTNode(token: name))
		self.addChild(constSection)
		self.addChild(typeSection)
		self.addChild(varSection)
		self.addChild(ASTNode(listOf: procedures))
		self.addChild(body)
	}
	
	// ----------------------------------
	convenience init(section: Token, contents: [ASTNode])
	{
		assert(TokenType.sectionTypes.contains(section.symbol))
		
		let sectionType: Kind
		switch section.symbol
		{
			case .begin:
				sectionType = .codeBlock
				#if DEBUG
				for statement in contents {
					assert(statement.isStatement)
				}
				#endif
			
			case .var:
				sectionType = .varSection
				#if DEBUG
				for declaration in contents {
					assert(declaration.kind == .variableDeclaration)
				}
				#endif
			
			case .const:
				sectionType = .constSection
				#if DEBUG
				for declaration in contents {
					assert(declaration.kind == .constantDeclaration)
				}
				#endif
			
			case .type:
				sectionType = .typeSection
				#if DEBUG
				for declaration in contents {
					assert(declaration.kind == .typeDeclaration)
				}
				#endif
			
			default:
				fatalError("Illegal sectionType, \(section.symbol)")
		}
		
		self.init(token: section, kind: sectionType)
		self.addChildren(contents)
	}
	
	// ----------------------------------
	convenience init(programModules: [ASTNode])
	{
		#if DEBUG
		for module in programModules {
			assert(module.kind == .moduleDeclaration)
		}
		#endif
		
		self.init(listOf: programModules)
		self.kind = .program
	}
	
	// ----------------------------------
	convenience init(
		if ifToken: Token,
		condition: ASTNode,
		thenBlock:ASTNode,
		elseBlock: ASTNode)
	{
		assert(ifToken.symbol == .if || ifToken.symbol == .elsif)
		assert(condition.isExpression)
		assert(thenBlock.kind == .codeBlock && thenBlock.symbol == .then)
		assert(
			(elseBlock.symbol == .else && elseBlock.kind == .codeBlock)
			|| (elseBlock.symbol == .elsif && elseBlock.kind == .ifStatement)
		)
		
		self.init(token: ifToken, kind: .ifStatement)
		
		self.children.reserveCapacity(3)
		addChild(condition)
		addChild(thenBlock)
		addChild(elseBlock)
	}
	
	// ----------------------------------
	convenience init(
		while whileToken: Token,
		condition: ASTNode,
		do block: ASTNode)
	{
		assert(whileToken.symbol == .while)
		assert(condition.isExpression)
		assert(block.kind == .codeBlock)
		
		self.init(token: whileToken, kind: .whileStatement)
		
		self.children.reserveCapacity(2)
		addChild(condition)
		addChild(block)
	}

	// ----------------------------------
	convenience init(listOf nodeList: [ASTNode])
	{
		self.init(token: Token.null(), kind: .nodeList)
		children.reserveCapacity(nodeList.count)
		self.addChildren(nodeList)
	}
	
	// ----------------------------------
	convenience init(
		array: Token,
		size: ASTNode,
		ofElementType elementType: ASTNode)
	{
		assert(array.symbol == .array)
		assert(size.isExpression)
		assert(elementType.kind.isTypeSpec)
		
		self.init(token: array, kind: .array)
		
		self.children.reserveCapacity(2)
		self.addChild(size)
		self.addChild(elementType)
	}
	
	// ----------------------------------
	convenience init(record: Token, fields: [ASTNode])
	{
		assert(record.symbol == .record)
		
		for field in fields
		{
			assert(field.kind == .variableDeclaration)
			field.kind = .fieldDeclaration
		}
		
		self.init(token: record, kind: .record)
		addChildren(fields)
	}
	
	// ----------------------------------
	public func replace(child: ASTNode, with newChild: ASTNode)
	{
		guard let childIndex = index(forChild: child) else
		{
			assertionFailure("child index not found")
			return
		}
		
		children[childIndex] = newChild
	}
	
	// ----------------------------------
	private func index(forChild node: ASTNode) -> Int?
	{
		for i in children.indices {
			if children[i] === node { return i }
		}
		
		return nil
	}

	// ----------------------------------
	private init(token: Token, kind: Kind)
	{
		self.token = token
		self.kind = kind
	}
	
	// ----------------------------------
	private func clone() -> ASTNode
	{
		let result = ASTNode(token: token, kind: kind)
		
		for child in children {
			result.addChild(child.clone())
		}
		
		return result
	}
	
	// ----------------------------------
	public final func addChild(_ child: ASTNode)
	{
		child.parent = self
		children.append(child)
	}
	
	// ----------------------------------
	public final func addChildren<S: Sequence>(_ nodes: S) where S.Element == ASTNode
	{
		for node in nodes {
			addChild(node)
		}
	}
	
	// ----------------------------------
	private static func kind(from token: Token) -> Kind
	{
		switch token.operatorGroup
		{
			case .binary: return .binaryOperator
			case .prefixUnary, .postfixUnary: return .unaryOperator
			default: break
		}
		
		switch token.symbol
		{
			case .identifier: return .variable
			case .number: return .constant
			default:
				fatalError(
					"Invalid token.symbol = \(token.symbol) to create ASTNode"
				)
		}
	}
	
	// ----------------------------------
	public static var empty: ASTNode {
		return ASTNode(token: Token.null(location: .none), kind: .empty)
	}
	
	// ----------------------------------
	public final var description: String
	{
		switch kind
		{
			case .empty: return "{}"
			case .variable, .constant, .typeName: return srcStr
			case .arrayElement: return "(\(children[0])[\(children[1])])"
			case .recordField: return "(\(children[0]).\(children[1]))"
			
			case .unaryOperator: return "\(srcStr)(\(children[0]))"
			case .binaryOperator:
				return "(\(children[0]) \(srcStr) \(children[1]))"
			
			case .function: return "\(srcStr)(\(childListDescription))"
			case .codeBlock: return "{\(childListDescription)}"
			case .assignment: return "\(children[0]) = \(children[1])"
			
			case .ifStatement, .whileStatement:
					return "\(srcStr){\(childListDescription)}"
			
			case .variableDeclaration: return "\(srcStr): \(children[0])"
			case .fieldDeclaration: return "\(srcStr): \(children[0])"

			case .constantDeclaration, .typeDeclaration:
				return "\(children[0]) is \(children[1])"
			
			case .procedureDeclaration, .moduleDeclaration:
				return "\(srcStr){\(childListDescription)}"

			case .program:
				return "PROGRAM{\(childListDescription)}"

			case .nodeList: return "\(childListDescription)"
			
			case .array: return "\(srcStr) \(children[0]) OF \(children[1])"
			case .record: return "\(srcStr){\(childListDescription)}"
			case .varSection: return "\(srcStr){\(childListDescription)}"
			case .constSection: return "\(srcStr){\(childListDescription)}"
			case .typeSection: return "\(srcStr){\(childListDescription)}"
			case .valueParam: return "val(\(srcStr): \(children[0]))"
			case .referenceParam: return "ref(\(srcStr): \(children[0]))"
		}
	}
	
	// ----------------------------------
	private var childListDescription: String
	{
		var result = ""
		
		if children.count > 0
		{
			for i in 0..<(children.count - 1) {
				result += "\(markUpListChild(children[i])), "
			}
			
			result += "\(markUpListChild(children.last!))"
		}
		
		return result
	}
	
	// ----------------------------------
	private func markUpListChild(_ node: ASTNode) -> String
	{
		guard node.kind == .nodeList else { return node.description }
		
		if self.kind == .procedureDeclaration || self.kind == .moduleDeclaration
		{
			return "[\(node.description)]"
		}
		
		return node.description
	}
}

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
class ASTNode: CustomStringConvertible
{
	// ----------------------------------
	public enum Kind
	{
		case empty
		case variable
		case constant
		case binaryOperator
		case unaryOperator
		case function
		case codeBlock
		case assignment
		case typeName
		case variableDeclaration
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
	public let token: Token
	public private(set) var children: [ASTNode] = []
	public weak var parent: ASTNode? = nil
	
	var srcStr: String { return token.srcString }
	
	// ----------------------------------
	var isStatement: Bool {
		return kind == .assignment || kind == .function
	}
	
	var isSection: Bool { return kind.isSection }
	
	// ----------------------------------
	var name: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[0]
	}
	
	// ----------------------------------
	var parameters: [ASTNode]
	{
		assert(self.kind == .procedureDeclaration)
		return children[6].children
	}
	
	// ----------------------------------
	var constSection: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[1]
	}

	// ----------------------------------
	var typeSection: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[2]
	}
	
	// ----------------------------------
	var varSection: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[3]
	}
	
	// ----------------------------------
	var procedureList: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[4]
	}
	
	// ----------------------------------
	var body: ASTNode
	{
		assert(self.kind == .procedureDeclaration)
		return children[5]
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
	convenience init(function: Token, parameters: [ASTNode])
	{
		assert(function.symbol == .identifier)
		self.init(token: function, kind: .function)
		self.addChildren(parameters)
	}
	
	// ----------------------------------
	convenience init(block: Token, statements: [ASTNode])
	{
		assert(block.symbol == .begin)
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
	convenience init(listOf nodeList: [ASTNode])
	{
		self.init(token: Token.null(), kind: .nodeList)
		self.addChildren(nodeList)
	}
	
	// ----------------------------------
	convenience init(
		array: Token,
		size: ASTNode,
		ofElementType elementType: ASTNode)
	{
		assert(array.symbol == .array)
		assert(elementType.kind.isTypeSpec)
		
		self.init(token: array, kind: .array)
		self.addChild(size)
		self.addChild(elementType)
	}
	
	// ----------------------------------
	convenience init(record: Token, fields: [ASTNode])
	{
		assert(record.symbol == .record)
		
		#if DEBUG
		for field in fields {
			assert(field.kind == .variableDeclaration)
		}
		#endif
		
		self.init(token: record, kind: .record)
		addChildren(fields)
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
	public func addChild(_ child: ASTNode)
	{
		child.parent = self
		children.append(child)
	}
	
	// ----------------------------------
	public func addChildren<S: Sequence>(_ nodes: S) where S.Element == ASTNode
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
	public var description: String
	{
		switch kind
		{
			case .empty: return "{}"
			case .variable, .constant, .typeName: return srcStr
			case .unaryOperator: return "\(srcStr)(\(children[0]))"
			
			case .binaryOperator:
				return "(\(children[0]) \(srcStr) \(children[1]))"
			
			case .function: return "\(srcStr)(\(childListDescription))"
			case .codeBlock: return "{\(childListDescription)}"
			case .assignment: return "\(children[0]) = \(children[1])"
			case .variableDeclaration: return "\(srcStr): \(children[0])"
			
			case .constantDeclaration, .typeDeclaration:
				return "\(children[0]) is \(children[1])"
			
			case .procedureDeclaration, .moduleDeclaration:
				return "\(srcStr){\(childListDescription)}"
			
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

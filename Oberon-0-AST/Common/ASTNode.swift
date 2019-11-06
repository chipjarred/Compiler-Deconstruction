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
	}
	
	public let kind: Kind
	public let token: Token
	public var children: [ASTNode] = []
	public weak var parent: ASTNode? = nil
	
	var srcStr: String { return token.srcString }
	
	// ----------------------------------
	convenience init(token: Token, child: ASTNode? = nil)
	{
		self.init(token: token, kind: ASTNode.kind(from: token))
		if let child = child {
			self.addChild(child)
		}
	}
	
	// ----------------------------------
	private init(token: Token, kind: Kind)
	{
		self.token = token
		self.kind = kind
	}
	
	// ----------------------------------
	public func addChild(_ child: ASTNode)
	{
		child.parent = self
		children.append(child)
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
			case .variable, .constant: return srcStr
			case .unaryOperator: return "\(srcStr)(\(children[0]))"
			case .binaryOperator:
				return "(\(children[0]) \(srcStr) \(children[1]))"
		}
	}
}

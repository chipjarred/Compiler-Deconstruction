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
public struct AbstractSyntaxTree: CustomStringConvertible
{
	private let root: ASTNode
	
	// ---------------------------------------------------
	public var description: String {
		return root.description
	}
	
	// ---------------------------------------------------
	public init(root: ASTNode) {
		self.root = root
	}
	
	// ---------------------------------------------------
	internal func findNode(
		kind: ASTNode.Kind,
		name: String? = nil) -> ASTNode?
	{
		return findNode(kind: kind, name: name, startingWith: root)
	}
	
	// ---------------------------------------------------
	public func traverse(with visitor: (_: ASTNode) -> Void) {
		traverse(startingAt: root, with: visitor)
	}
	
	// ---------------------------------------------------
	private func traverse(
		startingAt node: ASTNode,
		with visitor: (_: ASTNode) -> Void)
	{
		if traverseDepthFirst(in: node, with: visitor) {
			return
		}
		
		visitor(node)
				
		switch node.kind
		{
			case .moduleDeclaration, .procedureDeclaration:
				/*
				For modules and procedures, we visit in a specific order,
				because there are potential dependencies between the items in
				each sections.  The body depends on types, variables, constants
				and procedures defined in its scope.  Procedures depend on
				types, variables and constants (and on other procedures, but
				they have to be defined before use, so the order of definition
				will take care of that).  Variables depend on types.  Types
				depends on constants.  By processing constants first, then
				types, then variables, then procedures, and finally the body,
				we can ensure that everything is defined before use in one pass
				through the children.
				
				Except for procedures parameters, these are current stored in
				the right order, but we still explicitly get them by name in
				that order so that we can change the underlying storage, if
				needed, without having to change this code.
				*/
				traverse(startingAt: node.constSection, with: visitor)
				traverse(startingAt: node.typeSection, with: visitor)
				traverse(startingAt: node.varSection, with: visitor)
				for proc in node.procedureList {
					traverse(startingAt: proc, with: visitor)
				}
				traverse(startingAt: node.body, with: visitor)
			
			default:
				for child in node.children {
					traverse(startingAt: child, with: visitor)
				}
		}
	}
	
	// ---------------------------------------------------
	/**
	- Returns: `true` if traversal of the `node` should continue "in order" on return, or `false`
		otherwise.
	*/
	private func traverseDepthFirst(
		in node: ASTNode,
		with visitor: (_: ASTNode) -> Void) -> Bool
	{
		switch node.kind
		{
			/*
			For procedure declarations we need to traverse the parameter list
			first, because their `TypeInfo`s are used in constructing the
			symbol definition for the procedure.
			*/
			case .procedureDeclaration:
				traverse(startingAt: node.children.last!, with: visitor)
				return false
			
			case .moduleDeclaration:
				return false

			default:
				for child in node.children {
					traverse(startingAt: child, with: visitor)
				}
		}
		
		visitor(node)
		return true
	}
	
	// ---------------------------------------------------
	private func findNode(
		kind: ASTNode.Kind,
		name: String?,
		startingWith node: ASTNode) -> ASTNode?
	{
		if node.kind == kind
		{
			if name == nil { return node }
			
			switch node.kind
			{
				case .constantDeclaration, .typeDeclaration, .procedureDeclaration:
					if node.children[0].name == name { return node }
				
				default:
					if node.name == name { return node }
			}
		}
		
		for child in node.children
		{
			if let matchingNode =
				findNode(kind: kind, name: name, startingWith: child)
			{
				return matchingNode
			}
		}
		
		return nil
	}
}

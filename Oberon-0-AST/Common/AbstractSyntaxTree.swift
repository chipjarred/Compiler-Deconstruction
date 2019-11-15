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
			case .procedureDeclaration:
				/*
				For procedures we need to visit parameters first, which are
				actually at the end of the children array so that the rest of
				the layout can be the same as for modules.
				
				The parameters is an array of valueParam or referenceParam
				nodes.
				*/
				for param in node.parameters {
					traverse(startingAt: param, with: visitor)
				}
				fallthrough
			
			case .moduleDeclaration:
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
	- Returns: `true` this method did any traversal, or `false` otherwise
	*/
	private func traverseDepthFirst(
		in node: ASTNode,
		with visitor: (_: ASTNode) -> Void) -> Bool
	{
		switch node.kind
		{
			case .procedureDeclaration, .moduleDeclaration: return false

			default:
				for child in node.children {
					traverse(startingAt: child, with: visitor)
				}
		}
		
		visitor(node)
		return true
	}
}

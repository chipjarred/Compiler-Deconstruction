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
	internal func findNode(
		kind: ASTNode.Kind,
		skip: Int) -> ASTNode?
	{
		var skip = skip
		return findNode(kind: kind, skip: &skip, startingWith: root)
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
		switch node.kind
		{
			case .procedureDeclaration:
				/*
				For procedure declarations since the parameters are stored last
				in order to share a layout with module declarations, we can't
				just traverse the children in order.  The parameters define
				symbols that need to be seen by the statements in the body as
				local variables, so they need to be defined first, before the
				actual local variables and body.  So we traverse the parameter
				list first, then fall through to process the rest of the
				sections just as for modules.
				*/
				traverse(startingAt: node.children.last!, with: visitor)
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
				depend on constants.  By processing constants first, then
				types, then variables, then procedures, and finally the body,
				we can ensure that everything is defined before use in one pass
				through the children.
				
				Except for procedures parameters, these are currently stored in
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
		
		visitor(node)
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
				case .constantDeclaration,
					 .typeDeclaration,
					 .procedureDeclaration:
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
	
	// ---------------------------------------------------
	private func findNode(
		kind: ASTNode.Kind,
		skip: inout Int,
		startingWith node: ASTNode) -> ASTNode?
	{
		if node.kind == kind
		{
			if skip == 0 { return node }
			skip -= 1
		}
		
		for child in node.children
		{
			if let matchingNode =
				findNode(kind: kind, skip: &skip, startingWith: child)
			{
				return matchingNode
			}
		}
		
		return nil
	}
}

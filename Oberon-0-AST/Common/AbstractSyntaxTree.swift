// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
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
    public let root: ASTNode
    
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

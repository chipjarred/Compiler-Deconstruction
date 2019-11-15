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
Embodies a symbol scope for the compiler.
*/
public final class SymbolScope: Sequence
{
	enum Error: Swift.Error
	{
		case duplicateSymbolDefinition(_: SymbolInfo)
	}
	
	public typealias Element = SymbolInfo
	public typealias Iterator = Array<Element>.Iterator

	internal var parentScope: SymbolScope? = nil
	internal var symbols: [SymbolInfo] = []
	public var depth: Int
	
	// ---------------------------------------------------
	private init(parentScope: SymbolScope?)
	{
		self.parentScope = parentScope
		
		/*
		The depth property affects how the code generator allocates storage for
		the variables in the scope. If depth = 0, storage should be global, and
		for depth > 0, it should be on the stack.  This is the same as the "lev"
		field in the ObjDesc record in Wirth's original code.
		
		Global variables have to be handled differently from procedure local
		variables, but in Oberon-0, the only symbols that are syntacticaly
		defined in global scope are module names.  That means that top-level
		variables in modules are defined inside the module's own scope.  That's
		great for symbol look-up because it means that symbols defined in
		modules won't clash with each other, but the code generator still needs
		to allocate storage for top-level module variables as though they were
		global. The same would be true in object-oriented languages for static
		variables in classes or structs, or in C++ namespaces.
		
		So we set the depth to 0 for both global and module scopes, that is any
		scope whose parent is `nil` (global scope) or whose grandparent is `nil`
		(module scope), indicating global storage, and then increment from there
		for deeper level scopes.
		
		Thanks to Swift's optional chaining, we can do the parent/grandparent
		test as one test (parent?.parentScope will be `nil` if either
		parentScope is `nil` or `parentScope!.parentScope` is `nil`)
		*/
		self.depth = parentScope?.parentScope == nil
			? 0
			: parentScope!.depth + 1
	}
	
	// ---------------------------------------------------
	public final func makeIterator() -> Iterator {
		return symbols.makeIterator()
	}
	
	// ---------------------------------------------------
	public final func modifyEach(with block: (_:inout SymbolInfo) -> Void)
	{
		for i in symbols.indices {
			block(&symbols[i])
		}
	}
	
	// ---------------------------------------------------
	/**
	Obtain `SymbolInfo` for a `name` from the receiving `SymbolScope`
	
	The search is made *only* in the receiving `SymbolScope`.  If you wish to search the entire
	`SymbolScope` hierarchy, use `scope.heirarchy[name]`
	
	- Parameter name: `String` specifying the name of the symbol to search for.
	
	- Returns: The `SymbolInfo` associated with `name` defined in the receiving
		`SymbolScope`, or `nil` if `name` is not defined.
	*/
	public final subscript(name: String) -> SymbolInfo?
	{
		for symInfo in symbols
		{
			if symInfo.name == name {
				return symInfo
			}
		}
		
		return nil
	}
	
	// ---------------------------------------------------
	/**
	Define a symbol in the receiving `SymbolScope`
	
	- Parameters:
		- name: `String` containing the name of the symbol to be defined
		- kind: `SymbolInfo.Kind` specifying what kind of symbol to define
	
	- Returns: The newly created `SymbolInfo` that was defined in the receiving `SymbolScope`,
		or the existing `SymbolInfo` if it was already defined.
	
	- Note: currently this method only reports an attempt to define an existing symbol by emitting a
		message via `Oberon0Lexer.mark()`.  It does not report this error directly to the caller, which
		is less than ideal, but in keeping with the behavior of Wirth's original code.
	
	- Throws: an `SymbolScope.Error.duplicateSymbolDefinition` if `name` is already
		defined in the receiving `SymbolScope`.
	*/
	public final func defineSymbol(
		named name: String,
		kind: SymbolInfo.Kind) throws -> SymbolInfo
	{
		if let symInfo = self[name] {
			throw Error.duplicateSymbolDefinition(symInfo)
		}
		
		let symInfo = SymbolInfo(name: name, kind: kind)
		append(symInfo)
		return symInfo
	}
	
	// ---------------------------------------------------
	internal final func append(_ symbolInfo: SymbolInfo)
	{
		assert(self[symbolInfo.name] == nil)
		symbols.append(symbolInfo)
		symbols.last?.owningScope = self
	}
	
	// ---------------------------------------------------
	internal final func append<S: Sequence>(_ symbolInfos: S)
		where S.Element == SymbolInfo
	{
		for element in symbolInfos {
			append(element)
		}
	}
	
	// ---------------------------------------------------
	public final func openScope() -> SymbolScope {
		return SymbolScope(parentScope: self)
	}
	
	// ---------------------------------------------------
	public final func closeScope() -> SymbolScope
	{
		assert(self.parentScope != nil)
		
		let parent = self.parentScope
		self.parentScope = nil
		return parent!
	}
	
	// ---------------------------------------------------
	public static func makeGlobalScope() -> SymbolScope
	{
		// ---------------------------------------------------
		func enter(
			_ kind: SymbolInfo.Kind,
			_ value: Int,
			_ name: String,
			_ type: TypeInfo?,
			in scope: SymbolScope)
		{
			scope.append(
				SymbolInfo(name: name, kind: kind, type: type, value: value)
			)
		}
		
		let scope = SymbolScope(parentScope: nil)
		
		enter(.type, 1, "Bool", RISCCodeGenerator.boolType, in: scope)
		enter(.type, 2, "Int", RISCCodeGenerator.intType, in: scope)
		enter(.constant, 1, "TRUE", RISCCodeGenerator.boolType, in: scope)
		enter(.constant, 0, "FALSE", RISCCodeGenerator.boolType, in: scope)
		enter(.standardProcedure, 1, "Read", nil, in: scope)
		enter(.standardProcedure, 2, "Write", nil, in: scope)
		enter(.standardProcedure, 3, "WriteHex", nil, in: scope)
		enter(.standardProcedure, 4, "WriteLn", nil, in: scope)
		
		return scope
	}
}

// MARK:- SymbolScope.Hierarchy implementation
// ---------------------------------------------------
public extension SymbolScope
{
	// ---------------------------------------------------
	/**
	A `SymbolScope.Hierarchy` anchored at the receiving `SymbolScope` object
	*/
	final var hierarchy: Hierarchy {
		return Hierarchy(self)
	}
	
	// ---------------------------------------------------
	/**
	Represents the whole hierarchy anchored at the scope from which  the `Hierarchy` object is obtained
	extending all the way up to and including global scope.  It provides a means of searching and iterating
	over symbols visible from the scope that generated it.
	*/
	struct Hierarchy: Sequence
	{
		public typealias Element = SymbolScope.Element
		
		private var startingScope: SymbolScope
		
		// ---------------------------------------------------
		/**
		Iterator for accessing `SymbolInfo` objects in the `Hierarchy` from which the `Iterator`
		is obtained.
		*/
		public struct Iterator: IteratorProtocol
		{
			public typealias Element = Hierarchy.Element
			
			private var currentScope: SymbolScope?
			private var currentScopeIterator: SymbolScope.Iterator
			
			// ---------------------------------------------------
			fileprivate init(_ currentScope: SymbolScope)
			{
				self.currentScope =  currentScope
				self.currentScopeIterator = currentScope.makeIterator()
			}
			
			// ---------------------------------------------------
			/**
			Obtain the next symbol in hierarchy, first exhausting the current scope before progressing to
			its parent scope, until the global scope is exhausted.
			
			- Returns: the next `SymbolInfo` in the `Hierarchy` or `nil` if there are no more.
			*/
			public mutating func next() -> Element?
			{
				while currentScope != nil
				{
					if let result = currentScopeIterator.next() {
						return result
					}
					
					currentScope = currentScope?.parentScope
					if let curScope = currentScope {
						currentScopeIterator = curScope.makeIterator()
					}
				}
				
				return nil
			}
		}
		
		// ---------------------------------------------------
		/**
		Initialize a scope Hierarchy anchored at the specified `SymbolScope`
		- Parameter startingScope: `SymbolScope` to use as the anchor of the `Hierarchy`
		*/
		fileprivate init(_ startingScope: SymbolScope) {
			self.startingScope = startingScope
		}
		
		// ---------------------------------------------------
		/**
		Obtain `SymbolInfo` for a `name` from `SymbolScope` hierarchy, starting with the receiving
		`SymbolScope`
		
		The search is performed starting with the receiving `SymbolScope` then successively through parent
		`SymbolScope`s with the first match being returned.
		
		- Parameter name: `String` specifying the name of the symbol to search for.
		
		- Returns: The `SymbolInfo` associated with `name` defined the `SymbolScope` hierarchy,
			or `nil` if `name` is not defined.
		*/
		public subscript(name: String) -> SymbolInfo?
		{
			for symbolInfo in self
			{
				if symbolInfo.name == name {
					return symbolInfo
				}
			}
			
			return nil
		}
		
		// ---------------------------------------------------
		public func makeIterator() -> SymbolScope.Hierarchy.Iterator {
			return Iterator(startingScope)
		}
	}
}

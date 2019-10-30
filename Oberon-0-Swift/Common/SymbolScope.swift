//
//  SymbolScope.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/30/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public class SymbolScope: Sequence
{
	public typealias Element = SymbolInfo
	public typealias Iterator = Array<Element>.Iterator

	internal var parentScope: SymbolScope? = nil
	internal var symbols: [SymbolInfo] = []
	
	// ---------------------------------------------------
	private init(parentScope: SymbolScope?) {
		self.parentScope = parentScope
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
	`SymbolScope` heirarchy, use `findInHeirarchy(name:)`
	
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
	*/
	public final func defineSymbol(
		named name: String,
		kind: SymbolInfo.Kind) -> SymbolInfo
	{
		if let symInfo = self[name]
		{
			Oberon0Lexer.mark("symbol, \(name), is already defined")
			return symInfo
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
	/**
	Obtain `SymbolInfo` for a `name` from `SymbolScope` heirarchy, starting with the receiving
	`SymbolScope`
	
	The search is performed starting with the receiving `SymbolScope` then successively through parent
	`SymbolScope`s with the first match being returned.
	
	- Parameter name: `String` specifying the name of the symbol to search for.
	
	- Returns: The `SymbolInfo` associated with `name` defined the `SymbolScope` heirarchy,
		or `nil` if `name` is not defined.
	*/
	public final func findInHeirarchy(name: String) -> SymbolInfo?
	{
		var nextScope:SymbolScope? = self
		
		while let curScope = nextScope
		{
			if let symInfo = curScope[name] {
				return symInfo
			}
			nextScope = curScope.parentScope
		}
		
		return nil
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

//
//  SymbolTable.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/29/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
internal struct SymbolTable
{
	typealias Object = RISCCodeGenerator.Object
	typealias ObjDesc = RISCCodeGenerator.ObjDesc
	typealias SymbolInfo = RISCCodeGenerator.SymbolInfo
	
	internal static var (topScope, universe) = makeTopScope()
	internal static var sentinel: RISCCodeGenerator.Object = makeSentinel()

	// ---------------------------------------------------
	public static func newObj(named name: String, kind: SymbolInfo.Kind) -> Object
	{
		var x = topScope
		sentinel!.symbolInfo.name = name
		while x!.next!.symbolInfo.name != name {
			x = x!.next
		}
		if x!.next == sentinel
		{
			let new = ObjDesc()
			new.symbolInfo = SymbolInfo(name: name, kind: kind)
			
			new.next = sentinel
			x!.next = new
			return new
		}
		else
		{
			Oberon0Lexer.mark("multiple definition of \(name)")
			return x!.next
		}
	}

	// ---------------------------------------------------
	public static func find(name: String) -> Object
	{
		var s = topScope
		sentinel!.symbolInfo.name = name
		while true
		{
			var x = s!.next
			while x!.symbolInfo.name != name {
				x = x!.next
			}
			if x !== sentinel
			{
				return x
			}
			if s === universe
			{
				Oberon0Lexer.mark("undefined identifier: \(name)")
				return x
			}
			s = s!.dsc
		}
	}
	
	// ---------------------------------------------------
	@discardableResult
	public static func openScope() -> Object
	{
		topScope = openScope(topScope)
		return topScope
	}
	
	// ---------------------------------------------------
	private static func openScope(_ topScope: Object) -> Object
	{
		let scope:RISCCodeGenerator.Object = ObjDesc()
		scope!.symbolInfo = SymbolInfo(kind: .head)
		scope!.dsc = topScope
		scope!.next = sentinel
		return scope
	}
	
	// ---------------------------------------------------
	@discardableResult
	public static func closeScope() -> Object
	{
		topScope = topScope!.dsc
		return topScope
	}

	// ---------------------------------------------------
	public static func enter(
		_ kind: SymbolInfo.Kind,
		_ value: Int,
		_ name: String,
		_ type: RISCCodeGenerator.`Type`,
		in topScope: inout Object)
	{
		let obj = ObjDesc()
		obj.symbolInfo = SymbolInfo(
			name: name,
			kind: kind,
			type: type,
			value: value
		)
		obj.dsc = nil
		obj.next = topScope!.next
		topScope!.next = obj
	}
	
	// ---------------------------------------------------
	fileprivate static func makeSentinel() -> Object
	{
		let sentinel = ObjDesc()
		sentinel.symbolInfo = SymbolInfo(
			kind: .variable,
			type: RISCCodeGenerator.intType,
			value: 0
		)
		
		return sentinel
	}
	
	// ---------------------------------------------------
	fileprivate static func makeTopScope() -> (Object, Object)
	{
		var topScope = openScope(nil)
		let universe = topScope
		
		enter(.type, 1, "Bool", RISCCodeGenerator.boolType, in: &topScope)
		enter(.type, 2, "Int", RISCCodeGenerator.intType, in: &topScope)
		enter(.constant, 1, "TRUE", RISCCodeGenerator.boolType, in: &topScope)
		enter(.constant, 0, "FALSE", RISCCodeGenerator.boolType, in: &topScope)
		enter(.standardProcedure, 1, "Read", nil, in: &topScope)
		enter(.standardProcedure, 2, "Write", nil, in: &topScope)
		enter(.standardProcedure, 3, "WriteHex", nil, in: &topScope)
		enter(.standardProcedure, 4, "WriteLn", nil, in: &topScope)
		
		return (topScope, universe)
	}
}

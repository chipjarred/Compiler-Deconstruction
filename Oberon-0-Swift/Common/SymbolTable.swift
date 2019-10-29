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
	internal static var sentinel: RISCCodeGenerator.Object = makeGuard()

	// ---------------------------------------------------
	public static func find(name: String) -> Object
	{
		var s = Oberon0Parser.topScope
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
			if s === Oberon0Parser.universe
			{
				Oberon0Lexer.mark("undefined identifier: \(name)")
				return x
			}
			s = s!.dsc
		}
	}
	
	// ---------------------------------------------------
	public static func openScope(_ topScope: Object) -> Object
	{
		let scope:RISCCodeGenerator.Object = ObjDesc()
		scope!.symbolInfo = SymbolInfo(kind: RISCCodeGenerator.Head)
		scope!.dsc = topScope
		scope!.next = sentinel
		return scope
	}

	// ---------------------------------------------------
	public static func closeScope(_ topScope: inout Object) {
		topScope = topScope!.dsc
	}

	// ---------------------------------------------------
	public static func enter(
		_ kind: Int,
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
	fileprivate static func makeGuard() -> RISCCodeGenerator.Object
	{
		let sentinel = RISCCodeGenerator.ObjDesc()
		sentinel.symbolInfo = SymbolInfo(
			kind: RISCCodeGenerator.Var,
			type: RISCCodeGenerator.intType,
			value: 0
		)
		
		return sentinel
	}
}

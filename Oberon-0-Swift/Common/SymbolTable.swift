//
//  SymbolTable.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/29/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct SymbolTable
{
	// ---------------------------------------------------
	public class ListNode: Equatable
	{
		public var symbolInfo = SymbolInfo()
		
		public var next: ListNode? = nil
		public var parentScope: ListNode? = nil
		
		public required init() { }
		
		public static func == (left: ListNode, right: ListNode) -> Bool {
			return left.symbolInfo == right.symbolInfo
		}
	}

	internal static var (topScope, universe) = makeTopScope()
	internal static var sentinel: SymbolTable.ListNode? = makeSentinel()

	// ---------------------------------------------------
	public static func newNode(
		named name: String,
		kind: SymbolInfo.Kind) -> ListNode?
	{
		var x = topScope
		sentinel!.symbolInfo.name = name
		while x!.next!.symbolInfo.name != name {
			x = x!.next
		}
		if x!.next == sentinel
		{
			let new = ListNode()
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
	public static func insert(
		named name: String,
		kind: SymbolInfo.Kind) -> SymbolInfo?
	{
		return newNode(named: name, kind: kind)?.symbolInfo
	}

	// ---------------------------------------------------
	public static func find(name: String) -> ListNode?
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
			s = s!.parentScope
		}
	}
	
	// ---------------------------------------------------
	@discardableResult
	public static func openScope() -> ListNode?
	{
		topScope = openScope(topScope)
		return topScope
	}
	
	// ---------------------------------------------------
	private static func openScope(_ topScope: ListNode?) -> ListNode?
	{
		let scope:SymbolTable.ListNode? = ListNode()
		scope!.symbolInfo = SymbolInfo(kind: .head)
		scope!.parentScope = topScope
		scope!.next = sentinel
		return scope
	}
	
	// ---------------------------------------------------
	@discardableResult
	public static func closeScope() -> ListNode?
	{
		topScope = topScope!.parentScope
		return topScope
	}

	// ---------------------------------------------------
	public static func enter(
		_ kind: SymbolInfo.Kind,
		_ value: Int,
		_ name: String,
		_ type: TypeDesc?,
		in topScope: inout ListNode?)
	{
		let obj = ListNode()
		obj.symbolInfo = SymbolInfo(
			name: name,
			kind: kind,
			type: type,
			value: value
		)
		obj.parentScope = nil
		obj.next = topScope!.next
		topScope!.next = obj
	}
	
	// ---------------------------------------------------
	fileprivate static func makeSentinel() -> ListNode?
	{
		let sentinel = ListNode()
		sentinel.symbolInfo = SymbolInfo(
			kind: .variable,
			type: RISCCodeGenerator.intType,
			value: 0
		)
		
		return sentinel
	}
	
	// ---------------------------------------------------
	fileprivate static func makeTopScope() -> (ListNode?, ListNode?)
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

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
	// ---------------------------------------------------
	public static func find(name: String) -> RISCCodeGenerator.Object
	{
		var s = Oberon0Parser.topScope
		Oberon0Parser.`guard`!.symbolInfo.name = name
		while true
		{
			var x = s!.next
			while x!.symbolInfo.name != name {
				x = x!.next
			}
			if x !== Oberon0Parser.`guard`
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
}

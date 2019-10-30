//
//  SymbolInfo.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/29/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

public typealias TypeDesc = RISCCodeGenerator.TypeInfo

// ---------------------------------------------------
public class SymbolInfo: Equatable
{
	// ---------------------------------------------------
	public enum Kind: Int
	{
		case head = 0
		case variable = 1
		case parameter = 2
		case constant = 3
		case field = 4
		case type = 5
		case procedure = 6
		case standardProcedure = 7
		
		case register = 10
		case condition = 11
	}

	public var kind: Kind = .head
	public var level: Int = 0
	public var type: TypeDesc? = nil
	public var name = ""
	public var value: Int = 0
	
	// ---------------------------------------------------
	final var isParameter: Bool {
		return (kind == .parameter) || kind == .variable && value > 0
	}
	
	// ---------------------------------------------------
	init(
		name: String = "",
		kind: Kind = .head,
		level: Int = 0,
		type: TypeDesc? = nil,
		value: Int = 0)
	{
		self.name = name
		self.kind = kind
		self.level = level
		self.type = type
		self.value = value
	}
	
	// ---------------------------------------------------
	public static func == (left: SymbolInfo, right: SymbolInfo) -> Bool
	{
		return left.kind == right.kind
			&& left.level == right.level
			&& left.name == right.name
			&& left.value == right.value
			&& left.type == right.type
	}
}

//
//  Token.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/31/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct Token: CustomStringConvertible
{
	public static let null = Token(.null)
	
	public private(set) var symbol: TokenType
	public private(set) var identifier: String
	public private(set) var value: Int
	
	// ---------------------------------------------------
	public init(_ symbol: TokenType, identifier: String = "", value: Int = 0)
	{
		self.symbol = symbol
		self.identifier = identifier
		self.value = value
	}
	
	// ---------------------------------------------------
	public var description: String
	{
		var result = "\(symbol)(rawValue: \(symbol.rawValue)"
		
		switch symbol
		{
			case .number: result += ", value: \(value)"
			case .ident: result += ", identifier: \(identifier)"
			default: break
		}
		
		return result + ")"
	}
}

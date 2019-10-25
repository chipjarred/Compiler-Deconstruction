//
//  OberonSymbol.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/25/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public enum OberonSymbol: Int, Comparable
{
	case null = 0
	case times = 1
	case div = 3
	case mod = 4
	case and = 5
	case plus = 6
	case minus = 7
	case or = 8
	case eql = 9
	case neq = 10
	case lss = 11
	case geq = 12
	case leq = 13
	case gtr = 14
	case period = 18
	case comma = 19
	case colon = 20
	case rparen = 22
	case rbrak = 23
	case of = 25
	case then = 26
	case `do` = 27
	case lparen = 29
	case lbrak = 30
	case not = 32
	case becomes = 33
	case number = 34
	case ident = 37
	case semicolon = 38
	case end = 40
	case `else` = 41
	case elsif = 42
	case `if` = 44
	case `while` = 46
	case array = 54
	case record = 55
	case const = 57
	case type = 58
	case `var` = 59
	case procedure = 60
	case begin = 61
	case module = 63
	case eof = 64
	
	// ---------------------------------------------------
	public static func < (lhs: OberonSymbol, rhs: OberonSymbol) -> Bool {
		return lhs.rawValue < rhs.rawValue
	}
}

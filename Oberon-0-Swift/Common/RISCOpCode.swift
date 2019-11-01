//
//  RISCOpCode.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 10/31/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct RISCOpCode:
	ExpressibleByIntegerLiteral,
	Comparable,
	Hashable,
	CustomStringConvertible
{
	public typealias IntegerLiteralType = UInt32
	
	public let code: UInt32
	
	// ---------------------------------------------------
	public init(integerLiteral: IntegerLiteralType) {
		self.code = integerLiteral
	}
	
	// ---------------------------------------------------
	public init(_ code: UInt32) { self.init(integerLiteral: code) }
	
	// ---------------------------------------------------
	public static func == (left: RISCOpCode, right: RISCOpCode) -> Bool {
		return left.code == right.code
	}
	
	// ---------------------------------------------------
	public static func < (left: RISCOpCode, right: RISCOpCode) -> Bool {
		return left.code < right.code
	}

	// ---------------------------------------------------
	public static func +(left: RISCOpCode, right: Int) -> RISCOpCode
	{
		if right < 0 {
			return RISCOpCode(left.code - UInt32(-right))
		}
		
		return RISCOpCode(left.code + UInt32(right))
	}
	
	// ---------------------------------------------------
	public static func -(left: RISCOpCode, right: Int) -> RISCOpCode {
		return left + (-right)
	}
	
	public static let MOV: RISCOpCode = 0
	public static let MVN: RISCOpCode = 1
	public static let ADD: RISCOpCode = 2
	public static let SUB: RISCOpCode = 3
	public static let MUL: RISCOpCode = 4
	public static let Div: RISCOpCode = 5
	public static let Mod: RISCOpCode = 6
	public static let CMP: RISCOpCode = 7
	public static let MOVI: RISCOpCode = 16
	public static let MVNI: RISCOpCode = 17
	public static let ADDI: RISCOpCode = 18
	public static let SUBI: RISCOpCode = 19
	public static let MULI: RISCOpCode = 20
	public static let DIVI: RISCOpCode = 21
	public static let MODI: RISCOpCode = 22
	public static let CMPI: RISCOpCode = 23
	public static let CHKI: RISCOpCode = 24
	public static let LDW: RISCOpCode = 32
	public static let LDB: RISCOpCode = 33
	public static let POP: RISCOpCode = 34
	public static let STW: RISCOpCode = 36
	public static let STB: RISCOpCode = 37
	public static let PSH: RISCOpCode = 38
	public static let RD: RISCOpCode = 40
	public static let WRD: RISCOpCode = 41
	public static let WRH: RISCOpCode = 42
	public static let WRL: RISCOpCode = 43
	public static let BEQ: RISCOpCode = 48
	public static let BNE: RISCOpCode = 49
	public static let BLT: RISCOpCode = 50
	public static let BGE: RISCOpCode = 51
	public static let BLE: RISCOpCode = 52
	public static let BGT: RISCOpCode = 53
	public static let BR: RISCOpCode = 56
	public static let BSR: RISCOpCode = 57
	public static let RET: RISCOpCode = 58

	// ---------------------------------------------------
	fileprivate static var mneumonics = makeMneumonics()
	fileprivate static func makeMneumonics() -> [RISCOpCode: String]
	{
		var mneumonics = [RISCOpCode: String]()

		mneumonics[.MOV] = "MOV "
		mneumonics[.MVN] = "MVN "
		mneumonics[.ADD] = "ADD "
		mneumonics[.SUB] = "SUB "
		mneumonics[.MUL] = "MUL "
		mneumonics[.Div] = "DIV "
		mneumonics[.Mod] = "MOD "
		mneumonics[.CMP] = "CMP "
		mneumonics[.MOVI] = "MOVI"
		mneumonics[.MVNI] = "MVNI"
		mneumonics[.ADDI] = "ADDI"
		mneumonics[.SUBI] = "SUBI"
		mneumonics[.MULI] = "MULI"
		mneumonics[.DIVI] = "DIVI"
		mneumonics[.MODI] = "MODI"
		mneumonics[.CMPI] = "CMPI"
		mneumonics[.CHKI] = "CHKI"
		mneumonics[.LDW] = "LDW "
		mneumonics[.LDB] = "LDB "
		mneumonics[.POP] = "POP "
		mneumonics[.STW] = "STW "
		mneumonics[.STB] = "STB "
		mneumonics[.PSH] = "PSH "
		mneumonics[.BEQ] = "BEQ "
		mneumonics[.BNE] = "BNE "
		mneumonics[.BLT] = "BLT "
		mneumonics[.BGE] = "BGE "
		mneumonics[.BLE] = "BLE "
		mneumonics[.BGT] = "BGT "
		mneumonics[.BR] = "BR "
		mneumonics[.BSR] = "BSR "
		mneumonics[.RET] = "RET "
		mneumonics[.RD] = "READ"
		mneumonics[.WRD] = "WRD "
		mneumonics[.WRH] = "WRH "
		mneumonics[.WRL] = "WRL "
		
		return mneumonics
	}
	
	// ---------------------------------------------------
	public var description: String {
		return RISCOpCode.mneumonics[self] ?? "UNKNOWN\(code)"
	}
}

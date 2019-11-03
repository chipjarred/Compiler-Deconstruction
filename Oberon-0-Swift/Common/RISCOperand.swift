//
//  RISCOperand.swift
//  Oberon-0-Swift
//
//  Created by Chip Jarred on 11/3/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
/**
`RISCOperand` is essentially Wirth's `Item` record, with a better name.  It represents an value to be
used as an operand for a RISC instruction.  It is attributed with information such as the addressing `mode`
the associated Oberon-0 type, RISC register allocation, etc...
*/
public struct RISCOperand
{
	public typealias Mode = SymbolInfo.Kind
	
	public var mode: Mode = .head
	public var lev: Int = 0
	public var type: TypeInfo? = nil
	public var a: Int = 0
	internal var b: Int = 0
	internal var c: Int = 0
	internal var r: Int = 0
	
	// ---------------------------------------------------
	public init() { }
	
	// ---------------------------------------------------
	// x.a
	public mutating func setFieldInfo(from symbolInfo: SymbolInfo)
	{
		a += symbolInfo.value
		type = symbolInfo.type
	}
	
	// ---------------------------------------------------
	// x := x[index]
	public mutating func index(
		at index: RISCOperand,
		for codegenerator: inout RISCCodeGenerator) throws
	{
		if index.type != RISCCodeGenerator.intType {
			throw CodeGenError.indexNotInteger
		}
		if index.mode == .constant
		{
			if (index.a < 0) || (index.a >= type!.len) {
				throw CodeGenError.indexOutOfRange(
					index: index.a,
					range: 0...type!.len
				)
			}
			self.a += index.a * Int(type!.base!.size)
		}
		else
		{
			var y = index
			if y.mode != .register {
				y = try codegenerator.load(y)
			}
			codegenerator.put(.CHKI, y.r, 0, type!.len)
			codegenerator.put(.MULI, y.r, y.r, type!.base!.size)
			codegenerator.put(.ADD, y.r, r, y.r)
			codegenerator.regs.remove(r)
			r = y.r
		}
		type = type!.base
	}
}

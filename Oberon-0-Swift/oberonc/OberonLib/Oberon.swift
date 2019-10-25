//
//  Oberon.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/15/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

// ---------------------------------------------------
public func CAP(_ c: CHAR) -> CHAR
{
	if c >= "a" && c <= "z" {
		return (c - CHAR("a")) + CHAR("A")
	}
	
	return c
}

// ---------------------------------------------------
public func ORD(_ c: CHAR) -> Int {
	return Int(c.ascii)
}

// ---------------------------------------------------
public func CHR<T: FixedWidthInteger>(_ c: T) -> CHAR {
	return CHAR(integerLiteral: UInt8(c))
}

// ---------------------------------------------------
public func COPY(_ src: ARRAY<CHAR>, _ dst: inout ARRAY<CHAR>)
{
	let charsToCopy = max(src.count, dst.count)
	for i in 0..<charsToCopy
	{
		dst[i] = src[i]
		if dst[i] == 0 { return }
	}
}

// ---------------------------------------------------
/**
Arithmetic Shift (left)
- Parameters:
	- x: value to shift
	- n:  number of bits to shift
- Returns: x * 2^n

- Note: Performs left shfit for positive values of `n`.  For negative values, it shifts right `-n` bits.
*/
public func ASH<T, U>(_ x: T, _ n: U) -> T
	where T: FixedWidthInteger, U: FixedWidthInteger, U:SignedInteger
{
	guard n > 0 else { return x >> Int(-n) }
	return x << Int(n)
}


// ---------------------------------------------------
public func ODD<T: FixedWidthInteger>(_ x: T) -> Bool {
	return x % 2 == 1
}

// ---------------------------------------------------
public func LEN(_ s: ARRAY<CHAR>) -> Int {
	return Int(s.count)
}

// ---------------------------------------------------
public func SHORT(_ x: Int) -> Int {
	return Int(x)
}

// ---------------------------------------------------
public func SHORT(_ x: Int) -> Int16 {
	return Int16(x)
}

// ---------------------------------------------------
public func SHORT(_ x: Double) -> Float {
	return Float(x)
}

// ---------------------------------------------------
public func LONG(_ x: Int16) -> Int {
	return Int(x)
}

// ---------------------------------------------------
public func LONG(_ x: Int) -> Int {
	return Int(x)
}

// ---------------------------------------------------
public func LONG(_ x: Float) -> Double {
	return Double(x)
}

// ---------------------------------------------------
public func NEW<T: DefaultInitializable>(_ p: inout T?) {
	p = T()
}

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
public func ORD(_ c: CHAR) -> LONGINT {
	return LONGINT(c.ascii)
}

// ---------------------------------------------------
public func CHR<T: FixedWidthInteger>(_ c: T) -> CHAR {
	return CHAR(integerLiteral: UInt8(c))
}

// ---------------------------------------------------
public func MAX<T: FixedWidthInteger>(_: T.Type) -> T {
	return T.max
}

// ---------------------------------------------------
public func MAX(_: LONGREAL.Type) -> LONGREAL {
	return LONGREAL.greatestFiniteMagnitude
}

// ---------------------------------------------------
public func MAX(_: REAL.Type) -> REAL {
	return REAL.greatestFiniteMagnitude
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
public func INCL(_ set: inout Set<INTEGER>, _ i: INTEGER) {
	set.insert(i)
}

// ---------------------------------------------------
public func INCL(_ set: inout Set<INTEGER>, _ i: LONGINT) {
	INCL(&set, INTEGER(i))
}

// ---------------------------------------------------
public func EXCL(_ set: inout Set<INTEGER>, _ i: INTEGER) {
	set.remove(i)
}

// ---------------------------------------------------
public func EXCL(_ set: inout Set<INTEGER>, _ i: LONGINT) {
	EXCL(&set, INTEGER(i))
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
public func ODD(_ x: LONGINT) -> Bool {
	return x % 2 == 1
}

// ---------------------------------------------------
public func LEN(_ s: ARRAY<CHAR>) -> LONGINT {
	return LONGINT(s.count)
}

// ---------------------------------------------------
public func SHORT(_ x: LONGINT) -> INTEGER {
	return INTEGER(x)
}

// ---------------------------------------------------
public func SHORT(_ x: INTEGER) -> SHORTINT {
	return SHORTINT(x)
}

// ---------------------------------------------------
public func SHORT(_ x: LONGREAL) -> REAL {
	return REAL(x)
}

// ---------------------------------------------------
public func LONG(_ x: SHORTINT) -> INTEGER {
	return INTEGER(x)
}

// ---------------------------------------------------
public func LONG(_ x: INTEGER) -> LONGINT {
	return LONGINT(x)
}

// ---------------------------------------------------
public func LONG(_ x: REAL) -> LONGREAL {
	return LONGREAL(x)
}

// ---------------------------------------------------
public func NEW<T: DefaultInitializable>(_ p: inout T?) {
	p = T()
}

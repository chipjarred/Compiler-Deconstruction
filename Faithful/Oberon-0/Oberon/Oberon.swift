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
public func MAX<T: FixedWidthInteger>(_: T.Type) -> T {
	return T.max
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

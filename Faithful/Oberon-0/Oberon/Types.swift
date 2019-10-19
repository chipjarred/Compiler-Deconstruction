//
//  Types.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/16/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

public typealias LONGINT = Int32
public typealias SHORTINT = Int16
public typealias INTEGER = Int
public typealias BOOLEAN = Bool

public typealias REAL = Float
public typealias LONGREAL = Double

extension LONGINT: DefaultInitializable { }
extension SHORTINT: DefaultInitializable { }
extension INTEGER: DefaultInitializable { }
extension BOOLEAN: DefaultInitializable { }

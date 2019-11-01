//
//  RISCEmulator.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

public enum RISCOpCode
{
}

// ---------------------------------------------------
public struct RISCEmulator
{
	// ---------------------------------------------------
	public struct OpCode:
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
		public static func == (left: OpCode, right: OpCode) -> Bool {
			return left.code == right.code
		}
		
		// ---------------------------------------------------
		public static func < (left: OpCode, right: OpCode) -> Bool {
			return left.code < right.code
		}

		// ---------------------------------------------------
		public static func +(left: OpCode, right: Int) -> OpCode
		{
			if right < 0 {
				return OpCode(left.code - UInt32(-right))
			}
			
			return OpCode(left.code + UInt32(right))
		}
		
		// ---------------------------------------------------
		public static func -(left: OpCode, right: Int) -> OpCode {
			return left + (-right)
		}
		
		public static let MOV: OpCode = 0
		public static let MVN: OpCode = 1
		public static let ADD: OpCode = 2
		public static let SUB: OpCode = 3
		public static let MUL: OpCode = 4
		public static let Div: OpCode = 5
		public static let Mod: OpCode = 6
		public static let CMP: OpCode = 7
		public static let MOVI: OpCode = 16
		public static let MVNI: OpCode = 17
		public static let ADDI: OpCode = 18
		public static let SUBI: OpCode = 19
		public static let MULI: OpCode = 20
		public static let DIVI: OpCode = 21
		public static let MODI: OpCode = 22
		public static let CMPI: OpCode = 23
		public static let CHKI: OpCode = 24
		public static let LDW: OpCode = 32
		public static let LDB: OpCode = 33
		public static let POP: OpCode = 34
		public static let STW: OpCode = 36
		public static let STB: OpCode = 37
		public static let PSH: OpCode = 38
		public static let RD: OpCode = 40
		public static let WRD: OpCode = 41
		public static let WRH: OpCode = 42
		public static let WRL: OpCode = 43
		public static let BEQ: OpCode = 48
		public static let BNE: OpCode = 49
		public static let BLT: OpCode = 50
		public static let BGE: OpCode = 51
		public static let BLE: OpCode = 52
		public static let BGT: OpCode = 53
		public static let BR: OpCode = 56
		public static let BSR: OpCode = 57
		public static let RET: OpCode = 58

		// ---------------------------------------------------
		fileprivate static var mneumonics = makeMneumonics()
		fileprivate static func makeMneumonics() -> [OpCode: String]
		{
			var mneumonics = [OpCode: String]()

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
			return OpCode.mneumonics[self] ?? "UNKNOWN\(code)"
		}
	}
	
	// in bytes
	public static let MemSize: Int = 4096
	internal static let memoryWords = MemSize / MemoryLayout<Int32>.size
	internal static let ProgOrg: Int = 2048
	internal static let programOriginIndex = ProgOrg / MemoryLayout<Int32>.size
	
	internal var IR: UInt32 = 0
	internal var N: Bool = false
	internal var Z: Bool = false
	public var R = [Int32](repeating: 0, count: 16)
	public var M = [Int32](repeating: 0, count: memoryWords)

	/**
	Tuple to hold a decoded RISC instruction.
	- Note: This type is NOT part of the original code
	*/
	public typealias DecodedInstruction = (
		opCode: OpCode,
		a: Int32,
		b: Int32,
		c: Int32
	)

	/**
	Decode a RISC instruction into it's opcode and parameters.
	- Note: This function is NOT part of the original code
	*/
	public func decode(instruction: UInt32) -> DecodedInstruction
	{
		enum InstructionFormat: UInt32
		{
			case format0 = 0x0000_0000
			case format1 = 0x4000_0000
			case format2 = 0x8000_0000
			case format3 = 0xc000_0000
		}
		let formatMask: UInt32 = InstructionFormat.format3.rawValue
		var instruct = instruction

		let a: Int32
		let b: Int32
		var c: Int32
		switch InstructionFormat(rawValue: instruct & formatMask)!
		{
			case .format0:
				c = Int32(instruct & 0x0f)
				instruct = instruct >> 18
				b = Int32(instruct & 0x0f)
				instruct = instruct >> 4
				a = Int32(instruct & 0x0f)
				instruct = instruct >> 4

			case .format1, .format2:
				let tempC = Int32(instruct & 0x0003_ffff)
				c = tempC >= 0x0002_0000 ? tempC - 0x0004_0000 : tempC
				instruct = instruct >> 18
				b = Int32(instruct & 0x0f)
				instruct = instruct >> 4
				a = Int32(instruct & 0x0f)
				instruct = instruct >> 4

			case .format3:
				a = 0
				b = 0
				let tempC = Int32(instruct & 0x03ff_ffff)
				c = tempC >= 0x0200_0000 ? tempC - 0x0400_0000 : tempC
				instruct = instruct >> 26
		}
		
		let opCode = OpCode(instruction / 0x4000000 % 0x40)

		return (opCode: opCode, a: a, b: b, c: c)
	}
	
	// ---------------------------------------------------
	fileprivate func printRegisters()
	{
		// ---------------------------------------------------
		func regStr(_ i: Int) -> String {
			"R\(String(format: "%08x", i)) = 0x\(hex:R[i]))"
		}
		
		var str = ""
		for i in 0..<(R.count / 4)
		{
			str = regStr(i) + "\t"
			str += regStr(i + 4) + "\t"
			str += regStr(i + 8) + "\t"
			str += regStr(i + 12)
			print(str)
		}
	}
	
	// ---------------------------------------------------
	fileprivate func traceExecution()
	{
		print(
			"Executing: \(R[15])\t"
			+ "\(disassemble(instruction: UInt32(M[R[15] / 4])))"
		)
	}

	// ---------------------------------------------------
	/**
	Execute the program, writing any output from RISC output instructions to `stdout`
	*/
	public mutating func execute(
		_ start: UInt32,
		input inputScanner: inout RISCInputScanner,
		debug: Bool = false)
	{
		var outStream =
			FileHandle.standardOutput.textOutputStream(encoding: .utf8)!
		execute(
			start,
			input: &inputScanner,
			output: &outStream,
			debug: debug
		)
	}

	// ---------------------------------------------------
	/**
	Execute the program, writing any output from RISC output instructions to `out`
	*/
	// ---------------------------------------------------
	public mutating func execute<OutStream: TextOutputStream>(
		_ start: UInt32,
		input inputScanner: inout RISCInputScanner,
		output outStream: inout OutStream,
		debug: Bool = false)
	{
		R[14] = 0
		R[15] = Int32(start) + Int32(RISCEmulator.ProgOrg)
		
		if debug
		{
			printRegisters()
			traceExecution()
		}

		while execute(
			instruction: UInt32(bitPattern: M[R[15] / 4]),
			input: &inputScanner,
			output: &outStream,
			debug: debug)
		{
			if debug
			{
				printRegisters()
				traceExecution()
			}
		}
		
		if debug
		{
			printRegisters()
			traceExecution()
		}
	}
	
	// ---------------------------------------------------
	private func getInteger(
		from inputScanner: inout RISCInputScanner) -> Int32
	{
		while let token = inputScanner.scan()
		{
			switch token
			{
				case let .integer(value):
					return Int32(value)
				
				default:
					print(
						"Expected integer input but got: \(token.description). "
						+ "Trying again..."
					)
			}
		}
	
		print("Expected integer input, but encountered end of input.  Using 0.")
		return 0
	}
	
	// ---------------------------------------------------
	/**
	Execute the program, writing any output from RISC output instructions to `out`

	- Returns: `true` of the program should continue executing, or `false`, if it should halt.
	*/
	private mutating func execute<OutStream: TextOutputStream>(
		instruction: UInt32,
		input inputScanner: inout RISCInputScanner,
		output outStream: inout OutStream,
		debug: Bool = false) -> Bool
	{

		var nextInstruction = R[15] + 4
		IR = instruction
		let (opc, a, b, c) = decode(instruction: IR)
		opCodeSwitch: switch opc
		{
			case .MOV, .MOVI: R[a] = c << b
			case .MVN, .MVNI: R[a] = -(c << b)
			case .ADD, .ADDI: R[a] = R[b] + c
			case .SUB, .SUBI: R[a] = R[b] - c
			case .MUL, .MULI: R[a] = R[b] * c
			case .Div, .DIVI: R[a] = R[b] / c
			case .Mod, .MODI: R[a] = R[b] % c
			case .CMP, .CMPI:
				Z = R[b] == c
				N = R[b] < c
			case .CHKI: if (R[a] < 0) || (R[a] >= c) { R[a] = 0 }
			case .LDW: R[a] = M[(R[b] + c) / 4]
			case .LDB: break
			case .POP:
				R[a] = M[R[b] / 4]
				R[b] += c
			case .STW:
				M[(R[b] + c) / 4] = R[a]
			case .STB: break
			case .PSH:
				R[b] -= c
				M[R[b] / 4] = R[a]
			case .RD: R[a] = getInteger(from: &inputScanner)
			case .WRD: print(" \(R[c])", terminator: "", to: &outStream)
			case .WRH: print("\(hex: R[c])", terminator: "", to: &outStream)
			case .WRL: print("\n", terminator: "", to: &outStream)
			case .BEQ: if Z { nextInstruction = R[15] + c*4 }
			case .BNE: if !Z { nextInstruction = R[15] + c*4 }
			case .BLT: if N { nextInstruction = R[15] + c*4 }
			case .BGE: if !N { nextInstruction = R[15] + c*4 }
			case .BLE: if Z || N { nextInstruction = R[15] + c*4 }
			case .BGT: if !Z && !N { nextInstruction = R[15] + c*4 }
			case .BR:
				nextInstruction = R[15] + c * 4
			case .BSR:
				nextInstruction = R[15] + c * 4
				R[14] = R[15] + 4
			case .RET:
				nextInstruction = R[c % 0x10]
				if nextInstruction == 0 { return false }
			default:
				print(
					"Illegal instruction at \(R[15])\n"
					+ "instruction: 0x\(hex: IR)\n"
					+ "Disassembly: "
					+ "\(disassemble(instruction: IR, debug: debug))\n"
					+ "Halting RISC processor"
				)
				return false
		}
		
		R[15] = nextInstruction
		
		return true
	}

	// ---------------------------------------------------
	public mutating func load(_ code: [UInt32], _ len: Int)
	{
		for i in 0..<len {
			M[i + Int(RISCEmulator.ProgOrg / 4)] = Int32(bitPattern: code[i])
		}
	}

	// ---------------------------------------------------
	public func disassemble(
		instruction: UInt32,
		debug: Bool = false) -> String
	{
		let w = instruction
		let (opCode, a, b, c) = decode(instruction: w)
		var output = opCode.description
		if opCode < .BEQ {
			output += "\t\(a), \(b), \(c)"
		}
		else {
			output += "\t\(c)"
		}
		output += "\tmachine code: (0x\(hex: w))"
		
		if debug
		{
			let format = (w >> 30) & 0x3
			output += "\nformat = \(format)"
			output += "\nopCode = 0x\(hex: opCode.code) "
			output += "\(opCode)  \(opCode.description)"
			output += "\n     a = 0x\(hex: a) \(a)"
			output += "\n     b = 0x\(hex: b) \(b)"
			output += "\n     c = 0x\(hex: c) \(c)"
		}

		return output
	}
	
	// ---------------------------------------------------
}

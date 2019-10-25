//
//  RISC.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//

import Foundation

// ---------------------------------------------------
public struct RISC
{
	// ---------------------------------------------------
	public struct OpCode: ExpressibleByIntegerLiteral, Comparable
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
	}
	
	/*in bytes*/
	public static let MemSize: Int = 4096
	internal static let memoryWords =
		MemSize / MemoryLayout<Int32>.size
	internal static let ProgOrg: Int = 2048
	internal static let programOriginIndex =
		ProgOrg / MemoryLayout<Int32>.size
	internal static let MOV: OpCode = 0
	internal static let MVN: OpCode = 1
	internal static let ADD: OpCode = 2
	internal static let SUB: OpCode = 3
	internal static let MUL: OpCode = 4
	internal static let Div: OpCode = 5
	internal static let Mod: OpCode = 6
	internal static let CMP: OpCode = 7
	internal static let MOVI: OpCode = 16
	internal static let MVNI: OpCode = 17
	internal static let ADDI: OpCode = 18
	internal static let SUBI: OpCode = 19
	internal static let MULI: OpCode = 20
	internal static let DIVI: OpCode = 21
	internal static let MODI: OpCode = 22
	internal static let CMPI: OpCode = 23
	internal static let CHKI: OpCode = 24
	internal static let LDW: OpCode = 32
	internal static let LDB: OpCode = 33
	internal static let POP: OpCode = 34
	internal static let STW: OpCode = 36
	internal static let STB: OpCode = 37
	internal static let PSH: OpCode = 38
	internal static let RD: OpCode = 40
	internal static let WRD: OpCode = 41
	internal static let WRH: OpCode = 42
	internal static let WRL: OpCode = 43
	internal static let BEQ: OpCode = 48
	internal static let BNE: OpCode = 49
	internal static let BLT: OpCode = 50
	internal static let BGE: OpCode = 51
	internal static let BLE: OpCode = 52
	internal static let BGT: OpCode = 53
	internal static let BR: OpCode = 56
	internal static let BSR: OpCode = 57
	internal static let RET: OpCode = 58

	internal static var IR: UInt32 = 0
	internal static var N: Bool = false
	internal static var Z: Bool = false
	public static var R = [Int32](repeating: 0, count: 16)
	public static var M = [Int32](repeating: 0, count: memoryWords)
	internal static var W = Texts.Writer()

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
	public static func decode(instruction: UInt32) -> DecodedInstruction
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
	public static func Execute(
		_ start: UInt32,
		_ in: inout Texts.Scanner,
		_ out: Texts.Text? = nil)
	{
		// Helper functions - not part of ORIGINAL CODE
		// These are needed because Swift doesn't allow implicit integer
		// conversions, and does bounds-checked.  We could use the
		// unchecked versions (ie. &+, &%, etc...) but just constructing
		// the right type is less error prone, which is the whole reason
		// Swift does that in the first place
		func Unsigned(_ x: Int32) -> UInt32 { return UInt32(bitPattern: x) }
		func Signed(_ x: UInt32) -> Int32 { return Int32(bitPattern: x) }
		
		R[14] = 0
		R[15] = Int32(start) + Int32(ProgOrg)
		let debugInstructionLimit = -1
		var debugInstructionCounter = 0
		loop: while true
		{
			if debugInstructionCounter == debugInstructionLimit { break }
			debugInstructionCounter += 1
			var nextInstruction = R[15] + 4
			IR = UInt32(bitPattern: M[R[15] / 4])
			let opc = OpCode(IR / 0x4000000 % 0x40)
			let a = Signed(IR / 0x400000 % 0x10)
			let b = Signed(IR / 0x40000 % 0x10)
			var c = Signed(IR % 0x40000)
			if opc < MOVI
			{
				/*F0*/
				c = R[Signed(IR % 0x10)]
			}
			else if opc < BEQ
			{
				/*F1, F2*/
				c = Signed(IR % 0x40000)
				if c >= 0x20000 {
					c -= 0x40000
				}
			}
			else
			{
				/*F3*/
				c = Signed(IR % 0x4000000)
				if c >= 0x2000000 {
					c -= 0x4000000
				}
			}
			switch opc
			{
				case MOV, MOVI: R[a] = c << b
				case MVN, MVNI: R[a] = -(c << b)
				case ADD, ADDI: R[a] = R[b] + c
				case SUB, SUBI: R[a] = R[b] - c
				case MUL, MULI: R[a] = R[b] * c
				case Div, DIVI: R[a] = R[b] / c
				case Mod, MODI: R[a] = R[b] % c
				case CMP, CMPI:
					Z = R[b] == c
					N = R[b] < c
				case CHKI: if (R[a] < 0) || (R[a] >= c) { R[a] = 0 }
				case LDW: R[a] = M[Unsigned((R[b] + c)) / 4]
				case LDB: break
				case POP:
					R[a] = M[Unsigned(R[b]) / 4]
					R[b] += c
				case STW:
					M[Unsigned((R[b] + c)) / 4] = R[a]
				case STB: break
				case PSH:
					R[b] -= c
					M[Unsigned(R[b]) / 4] = R[a]
				case RD:
					Texts.Scan(&`in`)
					R[a] = Int32(`in`.i)
				case WRD:
					// MODIFICATION: If out is not provided, write to stdout
					if out != nil
					{
						Texts.Write(&W, " ");
						Texts.WriteInt(&W, Int(R[c]), 1)
					}
					else { print(" \(R[c])") }
				case WRH:
					// MODIFICATION: If out is not provided, write to stdout
					if out != nil { Texts.WriteHex(&W, Int(R[c])) }
					else { print("\(hex: R[c])") }
				case WRL:
					// MODIFICATION: If out is not provided, write to stdout
					if let out = out
					{
						Texts.WriteLn(&W)
						Texts.Append(out, &W.buf)
					}
					else { print("\n") }
				case BEQ: if Z { nextInstruction = R[15] + c*4 }
				case BNE: if !Z { nextInstruction = R[15] + c*4 }
				case BLT: if N { nextInstruction = R[15] + c*4 }
				case BGE: if !N { nextInstruction = R[15] + c*4 }
				case BLE: if Z || N { nextInstruction = R[15] + c*4 }
				case BGT: if !Z && !N { nextInstruction = R[15] + c*4 }
				case BR:
					nextInstruction = R[15] + c * 4
				case BSR:
					nextInstruction = R[15] + c * 4
					R[14] = R[15] + 4
				case RET:
					nextInstruction = R[c % 0x10]
					if nextInstruction == 0 {
						break loop
					}
				default:
					print("Illegal instruction at \(R[15])")
					print("instruction: 0x\(hex: IR)")
					print("\(#function): \(#line)")
					print("Disassembly: \(disassemble(instruction:IR, debug: true))")
					print("\(#function): \(#line)")
					print("Halting RISC processor")
					return
			}
			R[15] = nextInstruction
		}
	}

	// ---------------------------------------------------
	public static func ModifiedExecute(
		_ start: UInt32,
		_ in: inout Texts.Scanner,
		_ out: Texts.Text?,
		debug: Bool = false)
	{
		// ---------------------------------------------------
		// Helper functions - not part of ORIGINAL CODE
		func Unsigned(_ x: Int32) -> UInt32 { return UInt32(bitPattern: x) }
		func Signed(_ x: UInt32) -> Int32 { return Int32(bitPattern: x) }
		
		// ---------------------------------------------------
		func toHex(_ x: UInt32) -> String { return String(format: "%08x", x) }
		func toDec(_ x: Int) -> String { return String(format: "%02d", x) }
		func regStr(_ i: Int) -> String {
			"R\(toDec(i)) = 0x\(toHex(Unsigned(R[i])))"
		}
		
		// ---------------------------------------------------
		func printRegisters()
		{
			guard debug else { return }
			
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
		func trace(
			_ message: @autoclosure () -> String = "",
			function: StaticString = #function,
			line: UInt = #line)
		{
			if debug {
				print("\(function): line: \(line): \(message())")
			}
		}
		
		R[14] = 0
		R[15] = Int32(start) + Int32(ProgOrg)
		printRegisters()
		let debugInstructionLimit = -1
		var debugInstructionCounter = 0
		loop: while true
		{
			if debugInstructionCounter == debugInstructionLimit { break }
			debugInstructionCounter += 1
			trace("\nExecuting: \(R[15])\t\(disassemble(instruction: UInt32(M[R[15] / 4])))")
			var nextInstruction = R[15] + 4
			IR = UInt32(bitPattern: M[R[15] / 4])
			let (opc, a, b, c) = decode(instruction: IR)
			switch opc
			{
				case MOV, MOVI: R[a] = c << b
				case MVN, MVNI: R[a] = -(c << b)
				case ADD, ADDI: R[a] = R[b] + c
				case SUB, SUBI: R[a] = R[b] - c
				case MUL, MULI: R[a] = R[b] * c
				case Div, DIVI: R[a] = R[b] / c
				case Mod, MODI: R[a] = R[b] % c
				case CMP, CMPI:
					Z = R[b] == c
					N = R[b] < c
				case CHKI: if (R[a] < 0) || (R[a] >= c) { R[a] = 0 }
				case LDW: R[a] = M[Unsigned((R[b] + c)) / 4]
				case LDB: break
				case POP:
					R[a] = M[Unsigned(R[b]) / 4]
					R[b] += c
				case STW:
					M[Unsigned((R[b] + c)) / 4] = R[a]
				case STB: break
				case PSH:
					R[b] -= c
					M[Unsigned(R[b]) / 4] = R[a]
				case RD:
					Texts.Scan(&`in`)
					R[a] = Int32(`in`.i)
				case WRD:
					// MODIFICATION: If out is not provided, write to stdout
					if out != nil
					{
						Texts.Write(&W, " ");
						Texts.WriteInt(&W, Int(R[c]), 1)
					}
					else { print(" \(R[c])") }
				case WRH:
					// MODIFICATION: If out is not provided, write to stdout
					if out != nil { Texts.WriteHex(&W, Int(R[c])) }
					else { print("\(hex: R[c])") }
				case WRL:
					// MODIFICATION: If out is not provided, write to stdout
					if let out = out
					{
						Texts.WriteLn(&W)
						Texts.Append(out, &W.buf)
					}
					else { print("\n") }
				case BEQ: if Z { nextInstruction = R[15] + c*4 }
				case BNE: if !Z { nextInstruction = R[15] + c*4 }
				case BLT: if N { nextInstruction = R[15] + c*4 }
				case BGE: if !N { nextInstruction = R[15] + c*4 }
				case BLE: if Z || N { nextInstruction = R[15] + c*4 }
				case BGT: if !Z && !N { nextInstruction = R[15] + c*4 }
				case BR:
					nextInstruction = R[15] + c * 4
				case BSR:
					nextInstruction = R[15] + c * 4
					R[14] = R[15] + 4
				case RET:
					nextInstruction = R[c % 0x10]
					if nextInstruction == 0 {
						break loop
					}
				default:
					print("Illegal instruction at \(R[15])")
					print("instruction: 0x\(hex: IR)")
					print("\(#function): \(#line)")
					print("Disassembly: \(disassemble(instruction: IR, debug: true))")
					print("\(#function): \(#line)")
					print("Halting RISC processor")
					return
			}
			R[15] = nextInstruction
			printRegisters()
		}
	}

	// ---------------------------------------------------
	public static func Load(_ code: [UInt32], _ len: Int)
	{
		for i in 0..<len {
			M[i + Int(ProgOrg / 4)] = Int32(bitPattern: code[i])
		}
	}

	// ---------------------------------------------------
	fileprivate static func _moduleInit() {
		Texts.OpenWriter(&W)
	}


	// ---------------------------------------------------
	public static func disassemble(
		instruction: UInt32,
		debug: Bool = false) -> String
	{
		let w = instruction
		let op = OpCode(w / 0x4000000 % 0x40)
		var output = mnemo[op.code].description
		guard output != "" else { return "0x\(hex: w)" }

		let (opCode, a, b, c) = decode(instruction: w)
		if op < BEQ {
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
			output += "\n    op = 0x\(hex: op.code) \(op)  \(mnemo[op.code].description)"
			output += "\nopCode = 0x\(hex: opCode.code) \(opCode)  \(mnemo[opCode.code].description)"
			output += "\n     a = 0x\(hex: a) \(a)"
			output += "\n     b = 0x\(hex: b) \(b)"
			output += "\n     c = 0x\(hex: c) \(c)"
		}

		return output
	}
	fileprivate static var mnemo = makeMneumonics()
	fileprivate static func makeMneumonics() -> [ARRAY<CHAR>]
	{
		var mnemo = [ARRAY<CHAR>](
			repeating: ARRAY<CHAR>(count: 5),
			count: 64
		)

		mnemo[MOV.code] = "MOV "
		mnemo[MVN.code] = "MVN "
		mnemo[ADD.code] = "ADD "
		mnemo[SUB.code] = "SUB "
		mnemo[MUL.code] = "MUL "
		mnemo[Div.code] = "DIV "
		mnemo[Mod.code] = "MOD "
		mnemo[CMP.code] = "CMP "
		mnemo[MOVI.code] = "MOVI"
		mnemo[MVNI.code] = "MVNI"
		mnemo[ADDI.code] = "ADDI"
		mnemo[SUBI.code] = "SUBI"
		mnemo[MULI.code] = "MULI"
		mnemo[DIVI.code] = "DIVI"
		mnemo[MODI.code] = "MODI"
		mnemo[CMPI.code] = "CMPI"
		mnemo[CHKI.code] = "CHKI"
		mnemo[LDW.code] = "LDW "
		mnemo[LDB.code] = "LDB "
		mnemo[POP.code] = "POP "
		mnemo[STW.code] = "STW "
		mnemo[STB.code] = "STB "
		mnemo[PSH.code] = "PSH "
		mnemo[BEQ.code] = "BEQ "
		mnemo[BNE.code] = "BNE "
		mnemo[BLT.code] = "BLT "
		mnemo[BGE.code] = "BGE "
		mnemo[BLE.code] = "BLE "
		mnemo[BGT.code] = "BGT "
		mnemo[BR.code] = "BR "
		mnemo[BSR.code] = "BSR "
		mnemo[RET.code] = "RET "
		mnemo[RD.code] = "READ"
		mnemo[WRD.code] = "WRD "
		mnemo[WRH.code] = "WRH "
		mnemo[WRL.code] = "WRL "
		
		return mnemo
	}
}


extension String.StringInterpolation
{
	mutating func appendInterpolation<T:FixedWidthInteger>(hex value: T)
	{
		let hexDigits = [Character]("0123456789abcedf")
		var hexStr = ""
		
		var value = value
		for _ in (0..<MemoryLayout<UInt32>.size * 2)
		{
			let nibble = Int(value & 0x0f)
			hexStr.append(hexDigits[nibble])
			value >>= 4
		}
		
		appendLiteral(String(hexStr.reversed()))
    }
}

//
//  RISC.swift
//  Oberon-0
//
//  Created by Chip Jarred on 10/17/19.
//  Copyright © 2019 Chip Jarred. All rights reserved.
//
import Texts
import Oberon

/*in bytes*/
public let MemSize: LONGINT = 4096
internal let ProgOrg: LONGINT = 2048
internal let MOV: LONGINT = 0
internal let MVN: LONGINT = 1
internal let ADD: LONGINT = 2
internal let SUB: LONGINT = 3
internal let MUL: LONGINT = 4
internal let Div: LONGINT = 5
internal let Mod: LONGINT = 6
internal let CMP: LONGINT = 7
internal let MOVI: LONGINT = 16
internal let MVNI: LONGINT = 17
internal let ADDI: LONGINT = 18
internal let SUBI: LONGINT = 19
internal let MULI: LONGINT = 20
internal let DIVI: LONGINT = 21
internal let MODI: LONGINT = 22
internal let CMPI: LONGINT = 23
internal let CHKI: LONGINT = 24
internal let LDW: LONGINT = 32
internal let LDB: LONGINT = 33
internal let POP: LONGINT = 34
internal let STW: LONGINT = 36
internal let STB: LONGINT = 37
internal let PSH: LONGINT = 38
internal let RD: LONGINT = 40
internal let WRD: LONGINT = 41
internal let WRH: LONGINT = 42
internal let WRL: LONGINT = 43
internal let BEQ: LONGINT = 48
internal let BNE: LONGINT = 49
internal let BLT: LONGINT = 50
internal let BGE: LONGINT = 51
internal let BLE: LONGINT = 52
internal let BGT: LONGINT = 53
internal let BR: LONGINT = 56
internal let BSR: LONGINT = 57
internal let RET: LONGINT = 58

internal var IR: UInt32 = 0
internal var N: BOOLEAN = false
internal var Z: BOOLEAN = false
public var R = ARRAY<LONGINT>(count: 16)
public var M = ARRAY<LONGINT>(count:
	Int(MemSize / LONGINT(MemoryLayout<LONGINT>.stride))
)
internal var W = Texts.Writer()

/**
Tuple to hold a decoded RISC instruction.
- Note: This type is NOT part of the original code
*/
public typealias DecodedInstruction = (
	opCode: LONGINT,
	a: LONGINT,
	b: LONGINT,
	c: LONGINT
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

	let a: LONGINT
	let b: LONGINT
	var c: LONGINT
	switch InstructionFormat(rawValue: instruct & formatMask)!
	{
		case .format0:
			c = LONGINT(instruct & 0x0f)
			instruct = instruct >> 18
			b = LONGINT(instruct & 0x0f)
			instruct = instruct >> 4
			a = LONGINT(instruct & 0x0f)
			instruct = instruct >> 4

		case .format1, .format2:
			let tempC = instruct & 0x0003_ffff
			c =  tempC >= 0x0002_0000
				? LONGINT(bitPattern: tempC) - 0x0004_0000
				: LONGINT(bitPattern: tempC)
			instruct = instruct >> 18
			b = LONGINT(instruct & 0x0f)
			instruct = instruct >> 4
			a = LONGINT(instruct & 0x0f)
			instruct = instruct >> 4

		case .format3:
			a = 0
			b = 0
			let tempC = instruct & 0x03ff_ffff
			c =  tempC >= 0x0200_0000
				? LONGINT(bitPattern: tempC) - 0x0400_0000
				: LONGINT(bitPattern: tempC)
			instruct = instruct >> 26
	}
	
//	let opCode = LONGINT(instruct & 0xf)
	let opCode = LONGINT(bitPattern: instruction / 0x4000000 % 0x40)

	return (opCode: opCode, a: a, b: b, c: c)
}

public func Execute(
	_ start: LONGINT,
	_ in: inout Texts.Scanner,
	_ out: Texts.Text? = nil)
{
	// Helper functions - not part of ORIGINAL CODE
	// These are needed because Swift doesn't allow implicit integer
	// conversions, and does bounds-checked.  We could use the
	// unchecked versions (ie. &+, &%, etc...) but just constructing
	// the right type is less error prone, which is the whole reason
	// Swift does that in the first place
	func Unsigned(_ x: LONGINT) -> UInt32 { return UInt32(bitPattern: x) }
	func Signed(_ x: UInt32) -> LONGINT { return LONGINT(bitPattern: x) }
	
	var opc, a, b, c, nxt: LONGINT

	R[14] = 0
	R[15] = start + ProgOrg
	let debugInstructionLimit = -1
	var debugInstructionCounter = 0
	loop: while true
	{
		if debugInstructionCounter == debugInstructionLimit { break }
		debugInstructionCounter += 1
		nxt = R[15] + 4
		IR = UInt32(bitPattern: M[R[15] / 4])
		opc = Signed(IR / 0x4000000 % 0x40)
		a = Signed(IR / 0x400000 % 0x10)
		b = Signed(IR / 0x40000 % 0x10)
		c = Signed(IR % 0x40000)
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
			case MOV, MOVI: R[a] = ASH(c, b)
			case MVN, MVNI: R[a] = -ASH(c, b)
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
				R[a] = `in`.i
			case WRD:
				// MODIFICATION: If out is not provided, write to stdout
				if out != nil
				{
					Texts.Write(&W, " ");
					Texts.WriteInt(&W, R[c], 1)
				}
				else { print(" \(R[c])") }
			case WRH:
				// MODIFICATION: If out is not provided, write to stdout
				if out != nil { Texts.WriteHex(&W, R[c]) }
				else { print("\(hex: R[c])") }
			case WRL:
				// MODIFICATION: If out is not provided, write to stdout
				if let out = out
				{
					Texts.WriteLn(&W)
					Texts.Append(out, &W.buf)
				}
				else { print("\n") }
			case BEQ: if Z { nxt = R[15] + c*4 }
			case BNE: if !Z { nxt = R[15] + c*4 }
			case BLT: if N { nxt = R[15] + c*4 }
			case BGE: if !N { nxt = R[15] + c*4 }
			case BLE: if Z || N { nxt = R[15] + c*4 }
			case BGT: if !Z && !N { nxt = R[15] + c*4 }
			case BR:
				nxt = R[15] + c * 4
			case BSR:
				nxt = R[15] + c * 4
				R[14] = R[15] + 4
			case RET:
				nxt = R[c % 0x10]
				if nxt == 0 {
					break loop
				}
			default:
				print("Illegal instruction at \(R[15])")
				print("instruction: 0x\(hex: IR)")
				print("\(#function): \(#line)")
				print("Disassembly: \(disassemble(instruction: LONGINT(bitPattern: IR), debug: true))")
				print("\(#function): \(#line)")
				print("Halting RISC processor")
				return
		}
		R[15] = nxt
	}
}

// ---------------------------------------------------
public func ModifiedExecute(
	_ start: LONGINT,
	_ in: inout Texts.Scanner,
	_ out: Texts.Text?,
	debug: Bool = false)
{
	// ---------------------------------------------------
	// Helper functions - not part of ORIGINAL CODE
	func Unsigned(_ x: LONGINT) -> UInt32 { return UInt32(bitPattern: x) }
	func Signed(_ x: UInt32) -> LONGINT { return LONGINT(bitPattern: x) }
	
	// ---------------------------------------------------
	func toHex(_ x: UInt32) -> String { return String(format: "%08x", x) }
	func toDec(_ x: INTEGER) -> String { return String(format: "%02d", x) }
	func regStr(_ i: INTEGER) -> String {
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
	
	var opc, a, b, c, nxt: LONGINT

	R[14] = 0
	R[15] = start + ProgOrg
	printRegisters()
	let debugInstructionLimit = -1
	var debugInstructionCounter = 0
	loop: while true
	{
		if debugInstructionCounter == debugInstructionLimit { break }
		debugInstructionCounter += 1
		trace("\nExecuting: \(R[15])\t\(disassemble(instruction: M[R[15] / 4]))")
		nxt = R[15] + 4
		IR = UInt32(bitPattern: M[R[15] / 4])
		(opc, a, b, c) = decode(instruction: IR)
		switch opc
		{
			case MOV, MOVI: R[a] = ASH(c, b)
			case MVN, MVNI: R[a] = -ASH(c, b)
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
				R[a] = `in`.i
			case WRD:
				// MODIFICATION: If out is not provided, write to stdout
				if out != nil
				{
					Texts.Write(&W, " ");
					Texts.WriteInt(&W, R[c], 1)
				}
				else { print(" \(R[c])") }
			case WRH:
				// MODIFICATION: If out is not provided, write to stdout
				if out != nil { Texts.WriteHex(&W, R[c]) }
				else { print("\(hex: R[c])") }
			case WRL:
				// MODIFICATION: If out is not provided, write to stdout
				if let out = out
				{
					Texts.WriteLn(&W)
					Texts.Append(out, &W.buf)
				}
				else { print("\n") }
			case BEQ: if Z { nxt = R[15] + c*4 }
			case BNE: if !Z { nxt = R[15] + c*4 }
			case BLT: if N { nxt = R[15] + c*4 }
			case BGE: if !N { nxt = R[15] + c*4 }
			case BLE: if Z || N { nxt = R[15] + c*4 }
			case BGT: if !Z && !N { nxt = R[15] + c*4 }
			case BR:
				nxt = R[15] + c * 4
			case BSR:
				nxt = R[15] + c * 4
				R[14] = R[15] + 4
			case RET:
				nxt = R[c % 0x10]
				if nxt == 0 {
					break loop
				}
			default:
				print("Illegal instruction at \(R[15])")
				print("instruction: 0x\(hex: IR)")
				print("\(#function): \(#line)")
				print("Disassembly: \(disassemble(instruction: LONGINT(bitPattern: IR), debug: true))")
				print("\(#function): \(#line)")
				print("Halting RISC processor")
				return
		}
		R[15] = nxt
		printRegisters()
	}
}

public func Load(_ code: ARRAY<LONGINT>, _ len: LONGINT)
{
	var i: LONGINT
	
	i = 0
	while i < len
	{
		M[i + ProgOrg / 4] = code[i]
		i += 1
	}
}

fileprivate func _moduleInit() {
	Texts.OpenWriter(&W)
}


fileprivate func disassemble(
	instruction: LONGINT,
	debug: Bool = false) -> String
{
	let w = UInt32(bitPattern: instruction)
	let op = LONGINT(w / 0x4000000 % 0x40)
	var output = mnemo[op].description
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
		output += "\n    op = 0x\(hex: op) \(op)  \(mnemo[op].description)"
		output += "\nopCode = 0x\(hex: opCode) \(opCode)  \(mnemo[opCode].description)"
		output += "\n     a = 0x\(hex: a) \(a)"
		output += "\n     b = 0x\(hex: b) \(b)"
		output += "\n     c = 0x\(hex: c) \(c)"
	}

	return output
}
fileprivate var mnemo = makeMneumonics()
fileprivate func makeMneumonics() -> [ARRAY<CHAR>]
{
	var mnemo = [ARRAY<CHAR>](
		repeating: ARRAY<CHAR>(count: 5),
		count: 64
	)

	mnemo[MOV] = "MOV "
	mnemo[MVN] = "MVN "
	mnemo[ADD] = "ADD "
	mnemo[SUB] = "SUB "
	mnemo[MUL] = "MUL "
	mnemo[Div] = "DIV "
	mnemo[Mod] = "MOD "
	mnemo[CMP] = "CMP "
	mnemo[MOVI] = "MOVI"
	mnemo[MVNI] = "MVNI"
	mnemo[ADDI] = "ADDI"
	mnemo[SUBI] = "SUBI"
	mnemo[MULI] = "MULI"
	mnemo[DIVI] = "DIVI"
	mnemo[MODI] = "MODI"
	mnemo[CMPI] = "CMPI"
	mnemo[CHKI] = "CHKI"
	mnemo[LDW] = "LDW "
	mnemo[LDB] = "LDB "
	mnemo[POP] = "POP "
	mnemo[STW] = "STW "
	mnemo[STB] = "STB "
	mnemo[PSH] = "PSH "
	mnemo[BEQ] = "BEQ "
	mnemo[BNE] = "BNE "
	mnemo[BLT] = "BLT "
	mnemo[BGE] = "BGE "
	mnemo[BLE] = "BLE "
	mnemo[BGT] = "BGT "
	mnemo[BR] = "BR "
	mnemo[BSR] = "BSR "
	mnemo[RET] = "RET "
	mnemo[RD] = "READ"
	mnemo[WRD] = "WRD "
	mnemo[WRH] = "WRH "
	mnemo[WRL] = "WRL "
	
	return mnemo
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

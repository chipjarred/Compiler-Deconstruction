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

internal var IR: LONGINT = 0
internal var N: BOOLEAN = false
internal var Z: BOOLEAN = false
public var R = ARRAY<LONGINT>(count: 16)
public var M = ARRAY<LONGINT>(count:
	Int(MemSize / LONGINT(MemoryLayout<LONGINT>.stride))
)
internal var W = Texts.Writer()

public func Execute(
	_ start: LONGINT,
	_ in: inout Texts.Scanner,
	_ out: Texts.Text)
{
	var opc, a, b, c, nxt: LONGINT

	R[14] = 0
	R[15] = start + ProgOrg
	loop: while true
	{
		nxt = R[15] + 4
		IR = M[R[15] / 4]
		opc = IR / 0x4000000 % 0x40
		a = IR / 0x400000 % 0x10
		b = IR / 0x40000 % 0x10
		c = IR % 0x40000
		if opc < MOVI
		{
			/*F0*/
			c = R[IR % 0x10]
		}
		else if opc < BEQ
		{
			/*F1, F2*/
			c = IR % 0x40000
			if c >= 0x20000 {
				c -= 0x40000
			}
		}
		else
		{
			/*F3*/
			c = IR % 0x4000000
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
			case LDW: R[a] = M[(R[b] + c) / 4]
			case LDB: break
			case POP:
				R[a] = M[(R[b]) / 4]
				R[b] += c
			case STW: M[(R[b] + c) / 4] = R[a]
			case STB: break
			case PSH:
				R[b] -= c
				M[(R[b]) / 4] = R[a]
			case RD:
				Texts.Scan(&`in`)
				R[a] = `in`.i
			case WRD:
				Texts.Write(&W, " ");
				Texts.WriteInt(&W, R[c], 1)
			case WRH:
				Texts.WriteHex(&W, R[c])
			case WRL:
				Texts.WriteLn(&W)
				Texts.Append(out, &W.buf)
			case BEQ: if Z { nxt = R[15] + c*4 }
			case BNE: if !Z { nxt = R[15] + c*4 }
			case BLT: if N { nxt = R[15] + c*4 }
			case BGE: if !N { nxt = R[15] + c*4 }
			case BLE: if Z || N { nxt = R[15] + c*4 }
			case BGT: if !Z && !N { nxt = R[15] + c*4 }
			case BR: nxt = R[15] + c*4
			case BSR:
				nxt = R[15] + c*4
				R[14] = R[15] + 4
			case RET:
				nxt = R[c % 0x10]
				if nxt == 0 {
					break loop
				}
			default: break // Probably should fatalError("Illegal Instruction \(opc)")
		}
		R[15] = nxt
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

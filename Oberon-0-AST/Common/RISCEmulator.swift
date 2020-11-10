// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Foundation

// ---------------------------------------------------
public struct RISCEmulator
{
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

    // ---------------------------------------------------
    /**
     Tuple to hold a decoded RISC instruction.
    */
    public typealias DecodedInstruction = (
        opCode: RISCOpCode,
        a: Int32,
        b: Int32,
        c: Int32
    )

    // ---------------------------------------------------
    /**
     Decode a RISC instruction into it's opcode and parameters.
    
     The RISC instruction format is described  in *Compiler Construction*.  This
     method uses that description to decode an instruction into a usable form.
    
     - Parameter instruction: `UInt32` containing the RISC instruction to be
        decoded
    
     - Returns; `DecodedInstruction` tuple containing the decode components of
        the instruction
     */
    public func decode(instruction: UInt32) -> DecodedInstruction
    {
        /*
        The most significant 2 bits specify the format.  We just test for them
        where they are instead of trying to extract the four instruction
        formats as actual 0, 1, 2 and 3.  ie. no shifting needed to get the
        format, just a simple AND with a mask, and conveniently we can use our
        format-3 indicator as the mask.
        */
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
                /*
                Format-0 instructions specify 3 registers
                2 operands and 1 destination: a = op(b, c)
                
                The 4 least significant bits are the c operand register number
                The next 12 bits are don't care bits.
                The next 4 bits are the b operand register number
                The next 4 bits are the a operand register number
                The next 4 bits are the opCode
                The next 2 bits are the format bits
                */
                c = Int32(instruct & 0x0f)
                instruct = instruct >> 18    // shift 4 c-bits + 12 don't care
                b = Int32(instruct & 0x0f)
                instruct = instruct >> 4    // shift 4 b-bits
                a = Int32(instruct & 0x0f)
                instruct = instruct >> 4    // shift 4 a-bits

            case .format1, .format2:
                /*
                Format-1 and Format-2 share the exact same layout.  Only the
                interpretation is different.  Format-1 encodes an immediate
                value operand into the instruct, where as format-2 encodes an
                offset (displacement) for register-indirect addressing.  The
                format-1 immediate value and format-2 offset occupy the exact
                same bits in the instruction, so we decode them both exactly the
                same way.
                
                The least 18 bits are the immediate value or offset
                The next 4 bits are the b operand register number
                The next 4 bits are the a operand register number
                The next 4 bits are the opCode
                The next 2 bits are the format bits
                */
                let tempC = Int32(instruct & 0x0003_ffff)
                c = tempC >= 0x0002_0000 ? tempC - 0x0004_0000 : tempC
                instruct = instruct >> 18 // shift 18 imm value/offset bits
                b = Int32(instruct & 0x0f)
                instruct = instruct >> 4  // shift 4 b-bits
                a = Int32(instruct & 0x0f)
                instruct = instruct >> 4  // shift 4 a-bits

            case .format3:
                /*
                Format-3 encodes PC-relative branch instructions.
                
                The least 26 bits are the PC-relative destination address
                The next 4 bits are the opCode
                The next 2 bits are the format bits
                */
                a = 0
                b = 0
                let tempC = Int32(instruct & 0x03ff_ffff)
                c = tempC >= 0x0200_0000 ? tempC - 0x0400_0000 : tempC
                instruct = instruct >> 26
        }
        
        let opCode = RISCOpCode(instruction / 0x4000000 % 0x40)

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
     Execute the program, writing any output from RISC output instructions to
     `stdout`
     */
    public mutating func execute(
        _ start: UInt32,
        input inputScanner: inout RISCInputScanner,
        debug: Bool = false)
    {
        var outStream =
            FileHandle.standardOutput.textOutputStream!
        execute(
            start,
            input: &inputScanner,
            output: &outStream,
            debug: debug
        )
    }

    // ---------------------------------------------------
    /**
     Execute the program, writing any output from RISC output instructions to
     `outStream`
     */
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
     Execute one instruction of the  program, writing any output from RISC
     output instructions to `outStream`

     - Returns: `true` if the emulator should continue executing, or `false`,
        if it should halt.
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

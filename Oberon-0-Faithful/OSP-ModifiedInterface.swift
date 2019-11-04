// Copyright 2019 Chip Jarred
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is furnished
// to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// ---------------------------------------------------
fileprivate func printLog()
{
	print(
		"\n\n ---- Oberon Log ----\n"
		+ "\(OberonLog?.description ?? "-- EMPTY LOG --")"
	)
	OberonLog?.clear()
}

// ---------------------------------------------------
public extension OSP
{
	static var magic: LONGINT {
		return LONGINT(bitPattern: 0x656e7472) // "entr"
	}
	
	// ---------------------------------------------------
	static var program: ARRAY<LONGINT>
	{
		let objCode = OSG.getObjectCode()
		var program = [LONGINT]()
		program.reserveCapacity(objCode.count + 2)
		program.append(OSP.magic)
		program.append(OSG.entry * 4)
		program.append(contentsOf: objCode)
		return ARRAY(program)
	}

	// ---------------------------------------------------
	/**
	Compile Oberon-0 code from a `String`
	*/
	static func Compile(source: String)
	{
		defer { printLog() }
		let sourceCode: Text = TextDesc(source)
		var S = Texts.Scanner()

		Texts.OpenScanner(&S, sourceCode, 0)
		OSS.Init(sourceCode, 0)
		OSS.Get(&sym)
		Module(&S)
	}

	// ---------------------------------------------------
	static func Decode() -> String
	{
		defer { printLog() }
		var result: Text = TextDesc()
		OSG.Decode(&result)
		let r = result?.description ?? "!!!!! NO OUTPUT !!!!!"
		return r
	}
}


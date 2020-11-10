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

// ----------------------------------
class CompilerPhase
{
    private let errorReporter: ErrorReporter
    
    public var errorCount: Int { return errorReporter.errorCount }
    
    // ---------------------------------------------------
    init(errorsTo reporter: ErrorReporter) {
        self.errorReporter = reporter
    }
    
    // MARK:- Diagnostic reporting
    // ---------------------------------------------------
    internal final func emitError(_ message: String) {
        errorReporter.mark(message)
    }
    
    // ---------------------------------------------------
    internal final func emitError(
        _ message: String,
        at location: SourceLocation?,
        annotatedWith annotation: String? = nil,
        at noteLocation: SourceLocation? = nil)
    {
        errorReporter.mark(
            message,
            at: location,
            annotatedWith: annotation,
            at: noteLocation
        )
    }
    
    // ---------------------------------------------------
    internal final func unexpectedError(_ error: Error) -> Never
    {
        let message =
            "Unexpected error: \(error): \(error.localizedDescription)"
        
        emitError(message)
        fatalError(message)
    }
}

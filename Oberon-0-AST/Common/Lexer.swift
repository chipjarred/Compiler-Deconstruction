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

fileprivate let nullCharacter = Character(ascii: 0)

fileprivate let lowerAlphabetStr = "abcdefghijklmnopqrstuvwxyz"
fileprivate let digitStr = "0123456789"
fileprivate let lowerAlphabet =
    CharacterSet(charactersIn: lowerAlphabetStr)
fileprivate let upperAlphabet =
    CharacterSet(charactersIn: lowerAlphabetStr.uppercased())
fileprivate let alphabet = lowerAlphabet.union(upperAlphabet)
fileprivate let numeric = CharacterSet(charactersIn: digitStr)
fileprivate let alphaNumeric = alphabet.union(numeric)

// ---------------------------------------------------
public class Lexer
{
    private static let IdLen: Int = 256

    /*
    We could now move identifier to be a local variable in identiferToken(),
    but it's actually a bit more efficient to keep it here, allowing
    identiferToken() to just re-use the existing storage for it rather than
    having to reallocate it for every call.
    */
    private var identifier = ""
    private var ch = Character(ascii: 0)
    private var sourceReader = UTF8CharacterReader()
    public let errorReporter: ErrorReporter
    
    internal private(set) var tokenLocation = SourceLocation.none
    private var characterLocation = SourceLocation.none
    
    // ---------------------------------------------------
    private func readCharacter(into c: inout Character) -> Bool
    {
        characterLocation = sourceReader.position
        return sourceReader.readCharacter(into: &c)
    }
    
    // ---------------------------------------------------
    private func readCharacter() -> Character?
    {
        characterLocation = sourceReader.position
        return sourceReader.readCharacter()
    }
    
    // ---------------------------------------------------
    private func mark(_ msg: String) {
        errorReporter.mark(msg, at: sourceReader.position)
    }
    
    // ---------------------------------------------------
    private func identifierToken() -> Token
    {
        var i = 0
        identifier.removeAll(keepingCapacity: true)
        repeat
        {
            if i < Lexer.IdLen
            {
                identifier.append(ch)
                i += 1
            }
        } while readCharacter(into: &ch)
            && alphaNumeric.contains(ch)

        let tokenRange = tokenLocation.rangeTo(characterLocation)
        if let keywordSymbol = TokenType.keywordTokenType(for: identifier) {
            return Token(keywordSymbol, sourceRange: tokenRange)
        }
        return Token(
            .identifier,
            identifier: identifier,
            sourceRange: tokenRange
        )
    }
    
    // ---------------------------------------------------
    private func numberToken() -> Token
    {
        var value = 0
        repeat
        {
            let zeroAscii = Character("0").asciiValue!
            let digitValue = Int(ch.asciiValue! - zeroAscii)
            if value <= (Int.max - digitValue) / 10 {
                value = 10 * value + digitValue
            }
            else
            {
                mark("number too large")
                value = 0
                break
            }
        }
        while readCharacter(into: &ch) && numeric.contains(ch)
        
        return Token(
            .number,
            value: value,
            sourceRange: tokenLocation.rangeTo(characterLocation)
        )
    }

    // ---------------------------------------------------
    private func discardComment()
    {
        guard readCharacter(into: &ch) else {
            mark("comment not terminated")
            return
        }
        
        outer: while true
        {
            inner: while true
            {
                while ch == "("
                {
                    guard readCharacter(into: &ch) else {
                        break inner
                    }
                    if ch == "*" { discardComment() }
                }
                if ch == "*"
                {
                    guard readCharacter(into: &ch) else {
                        break inner
                    }
                    break
                }
                
                guard readCharacter(into: &ch) else {
                    break inner
                }
            }

            if ch == ")"
            {
                let _ = readCharacter(into: &ch)
                break
            }
            
            if sourceReader.endOfInput
            {
                mark("comment not terminated")
                break
            }
        }
    }
    
    // ---------------------------------------------------
    internal var eofToken: Token {
        return Token(.eof, location: tokenLocation)
    }

    // ---------------------------------------------------
    /**
     Unconditionally gets  the next token from the token stream.  This is the
     original method of obtaining a token, but since it doesn't allow a means
     of peeking at the next token without consuming it, it is private. Use
     `nextToken()` or `peekToken()` instead.
    
     - Returns: the next token in the token stream, or  `.eof`, if all tokens
        have been read.
    */
    private func getToken() -> Token
    {
        defer { tokenLocation = characterLocation }
        
        while !sourceReader.endOfInput,
            ch <= " ",
            readCharacter(into: &ch)
        {
            tokenLocation = characterLocation
        }
        
        if sourceReader.endOfInput {
            return Token(.eof, location: tokenLocation)
        }
        else
        {
            switch ch
            {
                case "0"..."9": return numberToken()
                case "A"..."Z", "a"..."z": return identifierToken()
                default: break
            }
            
            var c = nullCharacter
            let _ = readCharacter(into: &c)
            
            var sym: TokenType
            switch ch
            {
                case "&": sym = .and
                case "*": sym = .times
                case "+": sym = .plus
                case "-": sym = .minus
                case "=": sym = .isEqualTo
                case "#": sym = .isNotEqualTo
                case "<":
                    if c == "="
                    {
                        c = readCharacter() ?? nullCharacter
                        sym = .lessThanOrEqualTo
                        
                    } else {
                        sym = .lessThan
                    }
                case ">":
                    if c == "="
                    {
                        c = readCharacter() ?? nullCharacter
                        sym = .greaterThanOrEqualTo
                    }
                    else {
                        sym = .greaterThan
                    }
                case ";": sym = .semicolon
                case ",": sym = .comma
                case ":":
                    if c == "="
                    {
                        c = readCharacter() ?? nullCharacter
                        sym = .becomes
                    }
                    else { sym = .colon }
                case ".": sym = .period
                case "(":
                    if c == "*" {
                        discardComment()
                        
                        return getToken()
                    }
                    else {
                        sym = .openParen
                    }
                case ")": sym = .closeParen
                case "[": sym = .openBracket
                case "]": sym = .closeBracket
                case "~": sym = .not
                default: sym = .null
            }
            
            ch = c
            
            return Token(
                sym,
                sourceRange: tokenLocation.rangeTo(characterLocation)
            )
        }
    }
    
    // ---------------------------------------------------
    /**
     - Returns: `token` unless `token.symbol` is `.eof`, in which case it
        returns `nil`
     */
    private func optionalToken(_ token: Token) -> Token? {
        return token.symbol == .eof ? nil : token
    }
    
    // ---------------------------------------------------
    /**
     Get the next token in the token stream, consuming it.
    
     - Note: this implementation works with a `cachedToken`, which if it's not
        `nil` is returned, and sets `cachedToken` to `nil` so that subsequent
        calls to `nextToken()` or `peekToken()` do not continue to return the
        same token returned by this call.  If `cachedToken` is `nil`, we
        actually get the next token from the stream.
     */
    public func nextToken() -> Token?
    {
        if let token = cachedToken
        {
            cachedToken = nil
            return optionalToken(token)
        }
        
        return optionalToken(getToken())
    }
    
    private var cachedToken: Token? = nil
        
    // ---------------------------------------------------
    /**
     Return the current token (the one that would be returned by a call to
     `nextToken()`), without consuming it, so that a subsequent call to
     `nextToken()` will still return it.
     */
    public func peekToken() -> Token?
    {
        cachedToken = cachedToken ?? getToken()
        
        return optionalToken(cachedToken!)
    }
    
    // ---------------------------------------------------
    /**
     Consume the current token without returning it.
    
     - Note: if `cachedToken` is `nil`, then we're either positioned just
        before the first token, or we have just called `nextToken()`.  Either
        way we have to actually get a token to advance past it.
        
     On the other hand, if `cachedToken` is not `nil` then it holds the token
        that would be returned by `nextToken()`, so all we have to do is set
        `cachedToken` to `nil`, which is fast, and the next call to either
        `peekToken()` or `nextToken()` will actually do the work of getting the
        token.
     */
    public func advance()
    {
        guard cachedToken?.symbol != .eof else { return }
        
        if cachedToken == nil { let _ = getToken() }
        cachedToken = nil
    }
    
    // ---------------------------------------------------
    /**
     Advance the lexer to the next token of type `target`, optionally consuming
     it.
    
     - Parameters:
        - target: `TokenType` to advance to.
        - consuming: `Bool` specifying whether or not to consme the target
            token.  `true` consumes it, while `false` leaves it as the current
            token.
     */
    public func advance(to target: TokenType, consuming: Bool) {
        advance(toOneOf: [target], consuming: consuming)
    }
    
    // ---------------------------------------------------
    /**
     Advance the lexer to the next token that matches any of the `TokenType`s
     listed in `targets`, optionally consuming it.
    
     - Parameters:
        - targets: `Array` of `TokenType` listing symbol types to advance to.
        - consuming: `Bool` specifying whether or not to consme the target
            token.  `true` consumes it, while `false` leaves it as the current
            token.
     */
    public func advance(toOneOf targets: [TokenType], consuming: Bool)
    {
        while let token = peekToken(), !targets.contains(token.symbol) {
            advance()
        }
        
        if consuming { advance() }
    }
    
    // ---------------------------------------------------
    public init(
        sourceStream: InputStream,
        sourceName: String,
        errorsTo reporter: ErrorReporter)
    {
        self.errorReporter = reporter
        self.sourceReader = UTF8CharacterReader(
            inputStream: sourceStream,
            name: sourceName
        )
        let curPosition = sourceReader.position
        self.tokenLocation = curPosition
        self.characterLocation = curPosition
        if !sourceReader.readCharacter(into: &ch) {
            ch = nullCharacter
        }
    }
}


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
class CodeGenerator: CompilerPhase
{
    private var codeGenImpl: RISCCodeGenerator
    public var entry: UInt32 { return UInt32(codeGenImpl.entry) * 4 }

    // ---------------------------------------------------
    public override init(errorsTo reporter: ErrorReporter)
    {
        self.codeGenImpl = RISCCodeGenerator()
        super.init(errorsTo: reporter)
    }

    // ---------------------------------------------------
    public final func generate(from ast: AbstractSyntaxTree?) -> [UInt32]?
    {
        guard let ast = ast else { return nil }
        generateProgram(ast.root)
        
        return errorCount == 0 ? codeGenImpl.getObjectCode() : nil
    }
    
    // ---------------------------------------------------
    /**
    Disassemble a compiled Oberon-0 program to a `String`
    */
    public final func disassemble() -> String
    {
        var result = ""
        codeGenImpl.decode(to: &result)
        return result
    }

    // ---------------------------------------------------
    private func generateProgram(_ node: ASTNode)
    {
        assert(node.kind == .program)
        
        for module in node.children {
            generateModule(for: module)
        }
    }
    
    // ---------------------------------------------------
    private func generateModule(for node: ASTNode)
    {
        assert(node.kind == .moduleDeclaration)
        
        codeGenImpl.open()
        
        let declarationSize = allocateGlobalVarSection(for: node.varSection)
        generateProcedures(node.procedureList)

        codeGenImpl.header(declarationSize)
        let _ = generateCodeBlock(for: node.body)
        
        codeGenImpl.close()
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Number of bytes used by global variable declarations
    */
    private func allocateGlobalVarSection(for node: ASTNode) -> Int
    {
        assert(node.kind == .varSection)
        assert(node.parent!.kind == .moduleDeclaration)
        assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
        
        var declarationsSize = 0
        
        for varDeclaration in node.children
        {
            declarationsSize +=
                allocateGlobalVariableDeclaration(for: varDeclaration)
            varDeclaration.symbolInfo.value  = -declarationsSize
        }
        
        return declarationsSize
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Number of bytes used by the global variable declaration
    */
    private func allocateGlobalVariableDeclaration(for node: ASTNode) -> Int
    {
        assert(node.kind == .variableDeclaration)
        assert(node.scope.depth == 0, "Scope depth is \(node.scope.depth)")
        
        node.symbolInfo.level = node.scope.depth
        return node.symbolInfo.type!.size
    }
    
    // ---------------------------------------------------
    private func generateProcedures(_ procedures: [ASTNode])
    {
        for procedure in procedures
        {
            assert(procedure.kind == .procedureDeclaration)
            generateProcedure(procedure)
        }
    }
    
    // ---------------------------------------------------
    private func generateProcedure(_ procedure: ASTNode)
    {
        assert(procedure.kind == .procedureDeclaration)
        
        codeGenImpl.IncLevel(1)

        let procInfo = procedure.children[0].symbolInfo!
        
        let paramSize = allocateFormalParameters(
            procInfo.type!.fields,
            for: procedure
        )
        let localVarSize = allocateLocalVarSection(for: procedure.varSection)
        generateProcedures(procedure.procedureList)
        
        procInfo.value = codeGenImpl.pc
        codeGenImpl.enter(localVarSize)
        
        let _ = generateCodeBlock(for: procedure.body)
        
        codeGenImpl.procedureReturn(paramSize)
        codeGenImpl.IncLevel(-1)
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Number of bytes used by formal parameters
    */
    private func allocateFormalParameters(
        _ parameters: [SymbolInfo],
        for procedure: ASTNode) -> Int
    {
        let parameterBlockStartOffset = RISCCodeGenerator.wordSize * 2
        var parametersSize = 0
        
        for parameter in parameters
        {
            parameter.level = procedure.scope.depth
            parametersSize += parameter.type!.size
        }
        
        var parameterOffset = parametersSize + parameterBlockStartOffset
        for parameter in parameters
        {
            parameterOffset -= (parameter.kind == .parameter)
                ? RISCCodeGenerator.wordSize
                : parameter.type!.size
            parameter.value = parameterOffset
        }
        
        return parametersSize
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Number of bytes used by local variables
    */
    private func allocateLocalVarSection(for node: ASTNode) -> Int
    {
        assert(node.kind == .varSection)
        assert(node.parent!.kind == .procedureDeclaration)
        assert(node.scope.depth > 0)
        
        var declarationsSize = 0
        
        for varDeclaration in node.children
        {
            declarationsSize +=
                allocateLocalVariableDeclaration(for: varDeclaration)
            varDeclaration.symbolInfo.value  = -declarationsSize
        }
        
        return declarationsSize
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Number of bytes used by the local variable declaration
    */
    private func allocateLocalVariableDeclaration(for node: ASTNode) -> Int
    {
        assert(node.kind == .variableDeclaration)
        assert(node.scope.depth > 0, "Scope depth is \(node.scope.depth)")
        
        node.symbolInfo.level = node.scope.depth
        return node.symbolInfo.type!.size
    }
    
    // ---------------------------------------------------
    /**
    - Returns: Bytes used code generated by the code block
    */
    private func generateCodeBlock(for node: ASTNode) -> Int
    {
        assert(node.kind == .codeBlock)
        
        let startIndex = codeGenImpl.pc
        
        for statement in node.children {
            generateStatement(statement)
        }
        
        let endIndex = codeGenImpl.pc
        return (endIndex - startIndex) * MemoryLayout<UInt32>.stride
    }
    
    // ---------------------------------------------------
    private func generateStatement(_ statement: ASTNode)
    {
        assert(statement.isStatement)
        
        switch statement.kind
        {
            case .assignment:
                generateAssignment(statement)
            
            case .ifStatement:
                generateIfStatement(statement)
            
            case .whileStatement:
                generateWhileStatement(statement)
            
            case .functionCall:
                generateProcedureCall(statement)
            
            default:
                emitError(
                    "Cannot generate code for \(statement.srcStr)",
                    at: statement.sourceLocation
                )
        }
    }
    
    // ---------------------------------------------------
    private func generateProcedureCall(_ procCall: ASTNode)
    {
        assert(procCall.kind == .functionCall)
        
        guard procCall.typeInfo == TypeInfo.void else
        {
            emitError(
                "Return value for function procedure, \(procCall.name), "
                + "must be used assigned to a variable or used in an "
                + "expression",
                at: procCall.sourceLocation
            )
            return
        }
        
        guard let procInfo = generateParameterPassing(for: procCall) else {
            return
        }
        
        generateCall(to: procCall, procInfo: procInfo)
    }
    
    // ---------------------------------------------------
    private func generateCall(to procedureCall: ASTNode, procInfo: SymbolInfo)
    {
        var callOperand = makeOperand(from: procInfo)
        if procInfo.kind == .standardProcedure
        {
            var parameter = procInfo.value <= 3
                ? generateExpression(procedureCall.children[0])
                : codeGenImpl.makeDefaultOperand()
            emitErrorOnThrow
            {
                try codeGenImpl.call(
                    standardProcedure: &callOperand,
                    with: &parameter
                )
            }
        }
        else {
            codeGenImpl.call(procedure: &callOperand)
        }
    }
    
    // ---------------------------------------------------
    private func generateParameterPassing(
        for procedureCall: ASTNode) -> SymbolInfo?
    {
        guard let procInfo = procedureCall.scope.hierarchy[procedureCall.name]
        else
        {
            emitError(
                "Undefined procedure, \(procedureCall.name)",
                at: procedureCall.sourceLocation
            )
            return nil
        }
        
        if procInfo.kind == .standardProcedure { return procInfo }
        
        guard procInfo.kind == .procedure else
        {
            emitError(
                "Attempt to call non-procedure, \(procInfo.name)",
                at: procedureCall.sourceLocation
            )
            return nil
        }
        
        assert(
            procedureCall.children.count == procInfo.type!.fields.count,
            "wrong number of parameters should be caught by type checking pass"
        )

        for (actualParameter, formalParamInfo) in
            zip(procedureCall.children, procInfo.type!.fields)
        {
            generatePassOneParameter(actualParameter, for: formalParamInfo)
        }
        
        return procInfo
    }
    
    // ---------------------------------------------------
    private func generatePassOneParameter(
        _ actualParameter: ASTNode, for formalParamInfo: SymbolInfo)
    {
        var paramOperand = generateExpression(actualParameter)
        emitErrorOnThrow {
            try codeGenImpl.parameter(&paramOperand, formalParamInfo)
        }
    }
    
    // ---------------------------------------------------
    private func generateWhileStatement(_ whileStatement: ASTNode)
    {
        assert(whileStatement.kind == .whileStatement)
        
        let jumpLocation = codeGenImpl.pc
        let condition = generateControlFlowCondition(for: whileStatement)
        
        generateDoBlock(for: whileStatement, using: condition, at: jumpLocation)
    }
    
    // ---------------------------------------------------
    private func generateDoBlock(
        for whileStatement: ASTNode,
        using condition: RISCOperand,
        at conditionLocation: Int)
    {
        assert(whileStatement.kind == .whileStatement)
        
        let _ = generateCodeBlock(for: whileStatement.children[1])
        codeGenImpl.jumpBack(conditionLocation)
        codeGenImpl.fixLink(condition.a)
    }
    
    // ---------------------------------------------------
    private func generateIfStatement(_ ifStatement: ASTNode)
    {
        assert(ifStatement.kind == .ifStatement)
        
        var jumpLocation = 0
        var condition = generateControlFlowCondition(for: ifStatement)
        
        generateThenBlock(of: ifStatement)
        
        let elseBlock = generateElseIfChain(
            of: ifStatement,
            reusing: &condition,
            and: &jumpLocation
        )
        
        generateElseBlock(elseBlock, using: &condition, and: &jumpLocation)
    }
    
    // ---------------------------------------------------
    private func generateControlFlowCondition(
        for controlStatement: ASTNode) -> RISCOperand
    {
        assert(
            controlStatement.kind == .ifStatement
            || controlStatement.kind == .whileStatement
        )
        
        var condition = generateExpression(controlStatement.children[0])
        
        emitErrorOnThrow {
            try codeGenImpl.conditionalJump(&condition)
        }
        
        return condition
    }
    
    // ---------------------------------------------------
    private func generateThenBlock(of ifStatement: ASTNode) {
        let _ = generateCodeBlock(for: ifStatement.children[1])
    }
    
    // ---------------------------------------------------
    /**
    Generate instructions for consecutive ELSIF blocks.
    
    - Returns: ELSE block that is not an IF statement (ie... not ELSIF).
    */
    private func generateElseIfChain(
        of ifStatement: ASTNode,
        reusing condition: inout RISCOperand,
        and jumpLocation: inout Int) -> ASTNode
    {
        var elseIf = ifStatement.children[2]
        while elseIf.kind == .ifStatement
        {
            generateElseIfCondition(
                for: elseIf,
                reusing: &condition,
                and: &jumpLocation
            )
            
            generateThenBlock(of: elseIf)
            
            elseIf = elseIf.children[2]
        }
        
        return elseIf
    }
    
    // ---------------------------------------------------
    private func generateElseIfCondition(
        for elseIf: ASTNode,
        reusing condition: inout RISCOperand,
        and jumpLocation: inout Int)
    {
        codeGenImpl.jumpForward(&jumpLocation)
        codeGenImpl.fixLink(condition.a)
        condition = generateExpression(elseIf.children[0])
        emitErrorOnThrow { try codeGenImpl.conditionalJump(&condition) }
    }
    
    // ---------------------------------------------------
    private func generateElseBlock(
        _ elseBlock: ASTNode,
        using condition: inout RISCOperand,
        and jumpLocation: inout Int)
    {
        if elseBlock.kind == .codeBlock && elseBlock.children.count > 0
        {
            codeGenImpl.jumpForward(&jumpLocation)
            codeGenImpl.fixLink(condition.a)
            let _ = generateCodeBlock(for: elseBlock)
        }
        else {
            codeGenImpl.fixLink(condition.a)
        }
        
        codeGenImpl.fixLink(jumpLocation)
    }
    
    // ---------------------------------------------------
    private func generateAssignment(_ assignment: ASTNode)
    {
        assert(assignment.kind == .assignment)
        
        let destination = assignment.children[0]
        assert(destination.isAssignable)
        
        let source = assignment.children[1]
        assert(source.isExpression)
        
        guard var left = makeOperand(from: destination) else {
            return
        }
        
        var right = generateExpression(source)
        
        emitErrorOnThrow {
            try codeGenImpl.emitAssignment(into: &left, from: &right)
        }
    }
    
    // ---------------------------------------------------
    private func generateExpression(_ expression: ASTNode) -> RISCOperand
    {
        switch expression.kind
        {
            case .unaryOperator:
                return generateUnaryOperation(expression)
            
            case .binaryOperator:
                return generateBinaryOperation(expression)
            
            case .functionCall:
                emitError(
                    "Function procedures are not currently supported in "
                    + "expressions",
                    at: expression.sourceLocation
                )
            
            default:
                if let operand = makeOperand(from: expression) {
                    return operand
                }
                assertionFailure("Illegal expression")
                emitError("Illegal expression.", at: expression.sourceLocation)
        }
        
        return codeGenImpl.makeDefaultOperand()
    }
    
    // ---------------------------------------------------
    private func generateUnaryOperation(_ operation: ASTNode) -> RISCOperand
    {
        assert(operation.kind == .unaryOperator)
        
        var operand = generateExpression(operation.children[0])
        
        switch operation.symbol
        {
            case .unaryPlus, .unaryMinus, .not:
                emitErrorOnThrow
                {
                    try codeGenImpl.emitUnaryExpression(
                        operation.symbol,
                        &operand
                    )
                }
            
            default:
                emitError(
                    "Cannot generate code for unary operator, "
                    + "\"\(operation.srcStr)\".",
                    at: operation.sourceLocation
                )
        }

        return operand
    }
    
    // ---------------------------------------------------
    private func generateBinaryOperation(_ operation: ASTNode) -> RISCOperand
    {
        assert(operation.kind == .binaryOperator)
        
        var left = generateExpression(operation.children[0])

        let opSymbol = operation.symbol
        if opSymbol == .and || opSymbol == .or
        {
            emitErrorOnThrow
            {
                try codeGenImpl.emitLogicShortCircuit(
                    for: opSymbol,
                    operand: &left
                )
            }
        }
        
        switch opSymbol
        {
            case .and, .or,
                 .plus, .minus, .times, .div, .mod:
                
                var right = generateExpression(operation.children[1])
                emitErrorOnThrow
                {
                    try codeGenImpl.emitBinaryExpression(
                        opSymbol,
                        &left,
                        &right
                    )
                }
            
            case .isEqualTo, .isNotEqualTo,
                 .lessThan, .lessThanOrEqualTo,
                 .greaterThan, .greaterThanOrEqualTo:
                
                var right = generateExpression(operation.children[1])
                emitErrorOnThrow {
                    try codeGenImpl.emitComparison(opSymbol, &left, &right)
                }

            default:
                emitError(
                    "Cannot generate code for binary operator, "
                    + "\"\(operation.srcStr)\".",
                    at: operation.sourceLocation
                )
        }
        
        return left
    }
    
    // ---------------------------------------------------
    private func makeOperand(from node: ASTNode) -> RISCOperand?
    {
        switch node.kind
        {
            case .constant:
                return codeGenImpl.makeConstItem(
                    node.symbolInfo.type,
                    node.value
                )
                
            case .variable:
                return makeOperand(from: node.symbolInfo)
                
            case .arrayElement:
                guard let arrayOp = makeOperand(from: node.children[0]) else {
                    break
                }
                
                let indexOp = generateExpression(node.children[1])
                return makeArrayElementOperand(arrayOp, index: indexOp)
            
            case .recordField:
                guard var recordField = makeOperand(from: node.children[0])
                else { break }
                
                recordField.setFieldInfo(from: node.children[1].symbolInfo)
                return recordField
            
            case .functionCall:
                guard let procInfo = node.scope.hierarchy[node.name] else
                {
                    emitError(
                        "Undefined procedure, \(node.name)",
                        at: node.sourceLocation
                    )
                    break
                }
                guard procInfo.kind == .procedure else
                {
                    emitError(
                        "Attempt to call non-procedure, \(procInfo.name)",
                        at: node.sourceLocation
                    )
                    break
                }
                return makeOperand(from: procInfo)
                
            default:
                emitError(
                    "Unable to generate operand from \(node.kind)",
                    at: node.sourceLocation
                )
        }
        
        return nil
    }
    
    // ---------------------------------------------------
    private func makeArrayElementOperand(
        _ array: RISCOperand,
        index: RISCOperand) -> RISCOperand
    {
        assert(array.type!.form == .array)
        
        var array = array
        
        emitErrorOnThrow {
            try array.index(at: index, for: &codeGenImpl)
        }
        
        return array
    }
    
    // ---------------------------------------------------
    private func makeOperand(from symbolInfo: SymbolInfo) -> RISCOperand
    {
        do
        {
            switch symbolInfo.kind
            {
                case .constant:
                    return codeGenImpl.makeConstItem(
                        symbolInfo.type!,
                        symbolInfo.value
                    )
                
                case .variable, .procedure, .standardProcedure, .parameter:
                    return try codeGenImpl.makeOperand(symbolInfo)
                
                default:
                    emitError(
                        "Unable to generate code",
                        at: symbolInfo.sourceLocation
                    )
            }
        }
        catch { emitErrorOnThrow { throw error } }
        
        return codeGenImpl.makeDefaultOperand()
    }
    
    // ---------------------------------------------------
    private func emitErrorOnThrow(for block: () throws -> Void)
    {
        do { return try block() }
        catch {
            emitError(error.localizedDescription)
        }
    }
}

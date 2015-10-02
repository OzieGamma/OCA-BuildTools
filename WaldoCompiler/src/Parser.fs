// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Parser.fs" company="Oswald Maskens">
//   Copyright 2014 Oswald Maskens
//   
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//   
//       http://www.apache.org/licenses/LICENSE-2.0
//   
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
// </copyright>
// --------------------------------------------------------------------------------------------------------------------
module OCA.WaldoCompiler.Parser

open OCA.AsmLib
open OCA.WaldoCompiler
open OFuncLib

let (|T|_|) target v = 
    match v with
    | Positioned(value, _) when value = target -> Some()
    | _ -> None

type Tree = List<Declaration>

and Declaration = 
    | Global of attributes : List<Positioned<string>> * name : Positioned<string> * tpe : Positioned<WaldoType> * value : Expr
    | AsmFunction of attributes : List<Positioned<string>> * name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | AsmOperator of attributes : List<Positioned<string>> * name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | DefFunction of attributes : List<Positioned<string>> * name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    | DefOperator of attributes : List<Positioned<string>> * name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    member this.position = 
        match this with
        | Global(_, Positioned(_, p), _, _) -> p
        | AsmFunction(_, Positioned(_, p), _, _, _) -> p
        | AsmOperator(_, Positioned(_, p), _, _, _) -> p
        | DefFunction(_, Positioned(_, p), _, _, _) -> p
        | DefOperator(_, Positioned(_, p), _, _, _) -> p
    member this.name = 
        match this with
        | Global(_, Positioned(name, _), _, _) -> name
        | AsmFunction(_, Positioned(name, _), _, _, _) -> name
        | AsmOperator(_, Positioned(name, _), _, _, _) -> name
        | DefFunction(_, Positioned(name, _), _, _, _) -> name
        | DefOperator(_, Positioned(name, _), _, _, _) -> name

and StatementBlock = List<Statement>

and Statement = 
    | VarDeclaration of name : Positioned<string> * tpe : Positioned<WaldoType> * value : Expr
    | MethodCallStatement of name : Positioned<string> * args : List<Expr>
    member this.position = 
        match this with
        | VarDeclaration(Positioned(_, p), _, _) -> p
        | MethodCallStatement(Positioned(_, p), _) -> p

and Expr = 
    | Variable of name : Positioned<string>
    | OperatorCall of left : Expr * op : Positioned<string> * right : Expr
    | MethodCall of name : Positioned<string> * args : List<Expr>
    | ConstExpr of ConstExpr
    member this.position = 
        match this with
        | Variable(Positioned(_, p)) -> p
        | OperatorCall(left, _, _) -> left.position
        | MethodCall(Positioned(_, p), _) -> p
        | ConstExpr expr -> expr.position

and ConstExpr = 
    | ConstInt of Positioned<bigint>
    | ConstUInt of Positioned<bigint>
    | ConstStr of Positioned<string>
    | ConstChar of Positioned<char>
    | ConstBool of Positioned<bool>
    member this.position = 
        match this with
        | ConstInt(Positioned(_, p)) -> p
        | ConstUInt(Positioned(_, p)) -> p
        | ConstStr(Positioned(_, p)) -> p
        | ConstChar(Positioned(_, p)) -> p
        | ConstBool(Positioned(_, p)) -> p

// Parses the file
let parseFile (fileName : string) (source : List<Positioned<Token>>) : GenericAttempt<Tree, Positioned<string>> = 
    let fail msg pos = Fail [ Positioned(msg, pos) ]
    let failUnexpected expected (head : Positioned<'T>) = 
        fail (sprintf "Expected %s but got %A instead" expected head.value) head.position
    
    let failUnexpectedEOF phase = 
        let pos = Position(0u, 0u, fileName)
        fail (sprintf "Unexpected end of file while trying to parse %s" phase) pos
    
    let failIfTailNotEmpry message (node, tail) = 
        match tail with
        | [] -> node
        | head :: _ -> failUnexpected message head
    
    let blockInsides source = 
        let rec tillBlockEnd n acc source = 
            match source with
            | T RightBrace :: tail when n = 1 -> Ok acc, tail
            | T RightBrace as head :: tail -> tillBlockEnd (n - 1) (head :: acc) tail
            | T LeftBrace as head :: tail -> tillBlockEnd (n + 1) (head :: acc) tail
            | head :: tail -> tillBlockEnd n (head :: acc) tail
            | [] -> failUnexpectedEOF "block end", []
        match source with
        | T LeftBrace :: tail -> 
            let body, tail = tillBlockEnd 1 [] tail
            body |> Attempt.map List.rev, tail
        | head :: tail -> head |> failUnexpected "block begin", tail
        | [] -> failUnexpectedEOF "block begin", []
    
    let parseAttributes source: PositionedListAttempt<string> * List<Positioned<Token>> =
        let rec parseAttrList acc source =
            match source with
            | T RightBracket :: tail -> Ok (acc |> List.rev), tail
            | T Comma :: Positioned(Id attr, pos) :: tail ->
                parseAttrList [Positioned(attr, pos)] tail
            | head :: tail -> head |> failUnexpected ", or ] in attribute", tail
            | [] -> failUnexpectedEOF ", or ] in attribute", []

        match source with
        | T LeftBracket :: Positioned(Id attr, pos) :: tail ->
            parseAttrList [Positioned(attr, pos)] tail
        | _ -> Ok [], source

    let rec parseType source : PositionedAttempt<WaldoType> * List<Positioned<Token>> = 
        let parseTypeList source = 
            let rec inner acc source = 
                let tpe, tail = parseType source
                match tail with
                | T Comma :: tail -> inner (tpe :: acc) tail
                | _ -> (tpe :: acc), tail
            inner [] source
        match source with
        | Positioned(Id "Void", pos) :: tail -> Ok(Positioned(Void, pos)), tail
        | Positioned(Id name, pos) :: tail -> Ok(Positioned(Identifier name, pos)), tail
        | Positioned(LeftBracket, pos) :: tail -> 
            let fromTpe, fromTail = parseTypeList tail
            match fromTail with
            | T(Operator "=>") :: tail -> 
                let toTpe, toTail = parseType tail
                match toTail with
                | T RightBracket :: tail -> 
                    (fromTpe |> Attempt.liftList, toTpe)
                    |> Attempt.lift2
                    |> Attempt.map (fun (f, t) -> Positioned(FuncType(f |> List.map Position.remove, t.value), pos)), tail
                | head :: tail -> head |> failUnexpected "] in func type", tail
                | [] -> failUnexpectedEOF "] in func type", []
            | head :: tail -> head |> failUnexpected "=> in func type", tail
            | [] -> failUnexpectedEOF "=> in func type", []
        | head :: tail -> head |> failUnexpected "type", tail
        | [] -> failUnexpectedEOF "type", []
    
    let parseArgumentList source = 
        let rec inner acc source =
            match source with
            | T RightParen :: _ -> acc, source
            | _ ->
                let tpe, tail = parseType source
                match tail with
                | Positioned(Id name, pos) :: tail -> 
                    let newAcc = (Positioned(name, pos), tpe) :: acc
                    match tail with
                    | T Comma :: tail -> inner newAcc tail
                    | _ -> newAcc, tail
                | _ -> acc, source
        match source with
        | T LeftParen :: tail -> 
            let argsList, tail = inner [] tail
            match tail with
            | T RightParen :: tail -> 
                argsList
                |> List.rev
                |> List.map (fun (name, tpe) -> tpe |> Attempt.map (fun tpe -> (name, tpe)))
                |> Attempt.liftList, tail
            | head :: tail -> head |> failUnexpected "argument list end ')'", tail
            | [] -> failUnexpectedEOF "argument list end ')'", []
        | head :: tail -> head |> failUnexpected "argument list", tail
        | [] -> failUnexpectedEOF "argument list", []
    
    let parseConstExpr source = 
        match source with
        | Positioned(IntLit lit, pos) :: tail -> Ok(ConstInt(Positioned(lit, pos))), tail
        | Positioned(UIntLit lit, pos) :: tail -> Ok(ConstUInt(Positioned(lit, pos))), tail
        | Positioned(CharLit lit, pos) :: tail -> Ok(ConstChar(Positioned(lit, pos))), tail
        | Positioned(StringLit lit, pos) :: tail -> Ok(ConstStr(Positioned(lit, pos))), tail
        | Positioned(BoolLit lit, pos) :: tail -> Ok(ConstBool(Positioned(lit, pos))), tail
        | head :: tail -> head |> failUnexpected "const expressions", tail
        | [] -> failUnexpectedEOF "const expression", []
    
    //Parses an asm function WITH the left paren.
    let parseAsm attr name retType node source = 
        let args, argsTail = parseArgumentList source
        let body, bodyTail = blockInsides argsTail
        (args, body |> Attempt.bind Transform.tokensToInstr) 
        |> Attempt.lift2curriedMap (fun args body -> node (attr, name, args, retType, body)), bodyTail
    
    // Parses an Expression
    let rec parseExpr source = 
        let parseMethodArgs source = 
            let rec inner acc source = 
                let expr, tail = parseExpr source
                match tail with
                | T Comma :: tail -> inner (expr :: acc) tail
                | T RightParen :: tail -> 
                    (expr :: acc)
                    |> List.rev
                    |> Attempt.liftList, tail
                | head :: tail -> failUnexpected ", or ) in argument list of function call" head, tail
                | [] -> failUnexpectedEOF "name after type in var decl", tail
            match source with
            | T RightParen :: tail -> Ok [], tail
            | _ -> inner [] source
        
        let rec parseSimpleExpr source = 
            match source with
            | Positioned(Id funcName, pos) :: T LeftParen :: tail -> 
                let args, tail = parseMethodArgs tail
                args |> Attempt.map (fun args -> MethodCall(Positioned(funcName, pos), args)), tail
            | Positioned(Id varName, pos) :: tail -> Ok(Variable(Positioned(varName, pos))), tail
            | other -> 
                let constExpr, tail = parseConstExpr other
                (constExpr |> Attempt.map ConstExpr), tail
        
        let left, tail = parseSimpleExpr source
        match tail with
        | Positioned(Operator op, pos) :: tail -> 
            let right, tail = parseSimpleExpr tail
            (left, right) |> Attempt.lift2curriedMap (fun left right -> OperatorCall(left, Positioned(op, pos), right)), 
            tail
        | _ -> left, tail
    
    let rec parseStatementBlock source = 
        let rec parseStatementBlockBody source = 
            (*
            let parseForLoop tail = 
                match tail with
                | Positioned(Id name, pos) :: T Equals :: tail -> 
                    let startExpr, tail = parseExpr tail
                    match tail with
                    | T Upto :: tail -> 
                        let endExpr, tail = parseExpr tail
                        let body, tail = parseStatementBlock tail
                        let statement = 
                            (startExpr, endExpr, body) 
                            |> Attempt.lift3curriedMap (fun startExpr endExpr body -> ForLoop(Positioned(name, pos), startExpr, endExpr, body))
                        statement :: parseStatementBlockBody tail
                    | head :: tail -> failUnexpected "Upto in for-loop statement" head :: parseStatementBlockBody tail
                    | [] -> failUnexpectedEOF "Upto in for-loop statement" :: []
                | head :: tail -> failUnexpected "Name and equals in for-loop statement" head :: parseStatementBlockBody tail
                | [] -> failUnexpectedEOF "Name and equals in for-loop statement" :: []
            
            let parseWhileLoop tail = 
                let expr, tail = parseExpr tail
                let body, tail = parseStatementBlock tail
                let statement = (expr, body) |> Attempt.lift2curriedMap (fun expr body -> WhileLoop(expr, body))
                statement :: parseStatementBlockBody tail
            
            *)
            let parseMethodCallStatment source = 
                let expr, tail = parseExpr source
                match expr with
                | Ok(MethodCall(name, args)) -> Ok(MethodCallStatement(name, args)) :: parseStatementBlockBody tail
                | Ok expr -> 
                    Fail [ Positioned("Only method calls can be used as statements and expressions", expr.position) ] 
                    :: parseStatementBlockBody tail
                | Fail msg -> Fail msg :: parseStatementBlockBody tail
            match source with
            | Positioned(Id _, _) :: T LeftParen :: _ -> parseMethodCallStatment source
            | head :: tail -> 
                let tpe, tail = parseType source
                match tail with
                | Positioned(Id name, pos) :: T Equals :: tail -> 
                    let expr, tail = parseExpr tail
                    let statement = 
                        (tpe, expr) 
                        |> Attempt.lift2curriedMap (fun tpe expr -> VarDeclaration(Positioned(name, pos), tpe, expr))
                    statement :: parseStatementBlockBody tail
                | head :: tail -> failUnexpected "name after type in var decl" head :: parseStatementBlockBody tail
                | [] -> failUnexpectedEOF "name after type in var decl" :: []
            | [] -> []
        
        let body, tail = blockInsides source
        body |> Attempt.bind (fun body -> parseStatementBlockBody body |> Attempt.liftList), tail
    
    //Parses a function WITH the left paren.
    let parseDef attr name retType node source = 
        let args, argsTail = parseArgumentList source
        let body, bodyTail = parseStatementBlock argsTail
        (args, body) |> Attempt.lift2curriedMap (fun args body -> node (attr, name, args, retType, body)), bodyTail
    
    // Parses a global AFTER the equals sign
    let parseGlobal attr name tpe source = 
        let expr, tail = source |> parseExpr
        expr |> Attempt.map (fun body -> Global(attr, name, tpe, body)), tail
    
    let rec parseDeclaration acc source : GenericAttempt<List<Declaration>, Positioned<string>> = 
        if source |> List.isEmpty then acc |> Attempt.liftList
        else
            // All declarations can have attributes
            let attributes, attrTail = parseAttributes source
            // All declarations start with a type
            let tpeAttempt, tail = parseType attrTail
            match (attributes, tpeAttempt) |> Attempt.lift2 with
            | Ok(attr, tpe) ->
                let asm = attr |> List.exists (fun attrName -> attrName |> Position.remove = "Asm")

                if asm then
                    match tail with
                    | Positioned(Id name, pos) :: T LeftParen :: _ -> 
                        let posName = Positioned(name, pos)
                        // WITH left paren, WITHOUT name
                        let newDecl, declTail = parseAsm attr posName tpe AsmFunction tail.Tail
                        parseDeclaration (newDecl :: acc) declTail
                    | Positioned(Id name, pos) :: T Equals :: tail -> Fail [Positioned("Globals can't have the Asm attribute", pos)]
                    | Positioned(Operator name, pos) :: tail -> 
                        let operator, operatorTail = parseAsm attr (Positioned(name, pos)) tpe AsmOperator tail
                        parseDeclaration (operator :: acc) operatorTail
                    | head :: _ -> head |> failUnexpected "asm declaration name"
                    | [] -> failUnexpectedEOF "asm declaration name"
                else
                    match tail with
                    // Since its an ascii name, it could be a function or a global 
                    | Positioned(Id name, pos) :: T LeftParen :: _  -> 
                        let posName = Positioned(name, pos)
                        // WITH left paren, WITHOUT name
                        let newDecl, declTail = parseDef attr posName tpe DefFunction tail.Tail
                        parseDeclaration (newDecl :: acc) declTail
                    | Positioned(Id name, pos) :: T Equals :: afterEquals -> 
                        let posName = Positioned(name, pos)
                        let newDecl, declTail = parseGlobal attr posName tpe afterEquals
                        parseDeclaration (newDecl :: acc) declTail
                    | Positioned(Operator name, pos) :: tail -> 
                        let operator, operatorTail = parseDef attr (Positioned(name, pos)) tpe DefOperator tail
                        parseDeclaration (operator :: acc) operatorTail
                    | head :: _ -> head |> failUnexpected "declaration name"
                    | [] -> failUnexpectedEOF "declaration name"
            | Fail msg -> // Make sure to get other errors
                match parseDeclaration acc tail with
                | Ok _ -> Fail msg
                | Fail msg2 -> Fail([ msg; msg2 ] |> List.concat)
    
    parseDeclaration [] source |> Attempt.map (fun list -> list |> List.rev)

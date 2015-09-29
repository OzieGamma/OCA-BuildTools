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
    | Global of name : Positioned<string> * expr : Expr
    | AsmFunction of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | AsmOperator of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | DefFunction of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    | DefOperator of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    member this.position = 
        match this with
        | Global(Positioned(_, p), _) -> p
        | AsmFunction(Positioned(_, p), _, _, _) -> p
        | AsmOperator(Positioned(_, p), _, _, _) -> p
        | DefFunction(Positioned(_, p), _, _, _) -> p
        | DefOperator(Positioned(_, p), _, _, _) -> p

and StatementBlock = List<Statement>

and Statement = 
    | VarDeclaration of var : Positioned<string> * tpe : Positioned<WaldoType> * value : Expr
    member this.position = 
        match this with
        | VarDeclaration(Positioned(p, _), _, _) -> p

and Expr = 
    | Variable of name : Positioned<string>
    | OperatorCall of left : Expr * op : Positioned<string> * right : Expr
    | ConstExpr of ConstExpr
    member this.position = 
        match this with
        | Variable(Positioned(_, p)) -> p
        | OperatorCall(left, _, _) -> left.position
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
    
    let rec parseType source : PositionedAttempt<WaldoType> * List<Positioned<Token>> = 
        let rec parseSimpleType source = 
            let parseTypeList source = 
                let rec inner acc source = 
                    let tpe, tail = parseSimpleType source
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
                    let toTpe, toTail = parseSimpleType tail
                    match toTail with
                    | T RightBracket :: tail -> 
                        (fromTpe |> Attempt.liftList, toTpe)
                        |> Attempt.lift2
                        |> Attempt.map (fun (f, t) -> Positioned(FuncType(f |> List.map Position.remove, t.value), pos)), 
                        tail
                    | head :: tail -> head |> failUnexpected "] in func type", tail
                    | [] -> failUnexpectedEOF "] in func type", []
                | head :: tail -> head |> failUnexpected "=> in func type", tail
                | [] -> failUnexpectedEOF "=> in func type", []
            | head :: tail -> head |> failUnexpected "type", tail
            | [] -> failUnexpectedEOF "type", []
        match source with
        | T Colon :: tail -> parseSimpleType tail
        | Positioned(_, pos) :: tail -> Ok(Positioned(Void, pos)), source
        | [] -> failUnexpectedEOF "type", []
    
    let parseArgumentList source = 
        let rec inner acc source = 
            match source with
            | Positioned(Id name, pos) :: tail -> 
                let tpe, tail = parseType tail
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
    
    let parseVarValDeclList source = 
        match source with
        | Positioned(Id name, pos) :: tail -> 
            let tpe, tail = parseType tail
            (tpe |> Attempt.map (fun tpe -> (Positioned(name, pos), tpe) :: [])), tail
        | other -> parseArgumentList other
    
    let parseConstExpr source = 
        match source with
        | Positioned(IntLit lit, pos) :: tail -> Ok(ConstInt(Positioned(lit, pos))), tail
        | Positioned(UIntLit lit, pos) :: tail -> Ok(ConstUInt(Positioned(lit, pos))), tail
        | Positioned(CharLit lit, pos) :: tail -> Ok(ConstChar(Positioned(lit, pos))), tail
        | Positioned(StringLit lit, pos) :: tail -> Ok(ConstStr(Positioned(lit, pos))), tail
        | Positioned(BoolLit lit, pos) :: tail -> Ok(ConstBool(Positioned(lit, pos))), tail
        | head :: tail -> head |> failUnexpected "const expressions", tail
        | [] -> failUnexpectedEOF "const expression", []
    
    let rec parseNamespaceName acc source = 
        match source with
        | Positioned(Id str, _) :: T Colon :: tail -> parseNamespaceName (acc + str + ";") tail
        | Positioned(Id str, pos) :: tail -> Ok((acc + str) |> Position.add pos), tail
        | head :: tail -> head |> failUnexpected "Id as namespace name", tail
        | [] -> failUnexpectedEOF "namespace name", []
    
    let parseAsmfunc source = 
        let parseGeneric node name pos tail = 
            let args, argsTail = parseArgumentList tail
            let retType, retTail = parseType argsTail
            
            let body = 
                blockInsides retTail
                |> failIfTailNotEmpry "asm function end"
                |> Attempt.bind Transform.tokensToInstr
            (args, retType, body)
            |> Attempt.lift3
            |> Attempt.map (fun (args, retType, body) -> node (name |> Position.add pos, args, retType, body))
        match source with
        | Positioned(Id name, pos) :: tail -> parseGeneric AsmFunction name pos tail
        | Positioned(Operator name, pos) :: tail -> parseGeneric AsmOperator name pos tail
        | head :: _ -> head |> failUnexpected "asm function name"
        | [] -> failUnexpectedEOF "asm function name"
    
    let rec parseExpr source = 
        let rec parseSimpleExpr source = 
            match source with
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
            
            let parseMethodCallStatment source = 
                let expr, tail = parseExpr source
                match expr with
                | Ok(MethodCall(name, this, args)) -> Ok(MethodCallStatement(name, this, args)) :: parseStatementBlockBody tail
                | Ok expr -> Fail [ Positioned("Only method calls can be used as statements and expressions", expr.position) ] :: parseStatementBlockBody tail
                | Fail msg -> Fail msg :: parseStatementBlockBody tail

            *)
            match source with
            | head :: tail -> 
                let tpe, tail = parseType source
                match tail with
                | Positioned(Id name, pos) :: T Equals :: tail -> 
                    let expr, tail = parseExpr tail
                    let statement = 
                        (tpe, expr) 
                        |> Attempt.lift2curriedMap (fun tpe expr -> VarDeclaration(Positioned(name, pos), tpe, expr))
                    statement :: parseStatementBlockBody tail
                | head :: tail -> failUnexpected "Name after type in var decl" head :: parseStatementBlockBody tail
                | [] -> failUnexpectedEOF "Name after type in var decl" :: []
            | [] -> []
        
        let body, tail = blockInsides source
        body |> Attempt.bind (fun body -> parseStatementBlockBody body |> Attempt.liftList), tail
    
    //|> Attempt.map List.rev, tail
    let parseDefFunc source = 
        let parseGeneric node name pos tail = 
            let args, argsTail = parseArgumentList tail
            let retType, retTail = parseType argsTail
            let body = parseStatementBlock retTail |> failIfTailNotEmpry "def function end"
            (args, retType, body)
            |> Attempt.lift3
            |> Attempt.map (fun (args, retType, body) -> node (name |> Position.add pos, args, retType, body))
        match source with
        | Positioned(Id name, pos) :: tail -> parseGeneric DefFunction name pos tail
        | Positioned(Operator name, pos) :: tail -> parseGeneric DefOperator name pos tail
        | head :: _ -> head |> failUnexpected "def function name"
        | [] -> failUnexpectedEOF "def function name"
    
    let parseConst source = 
        match source with
        | Positioned(Id name, pos) :: T Equals :: tail -> 
            tail
            |> parseConstExpr
            |> failIfTailNotEmpry "End of const expression"
            |> Attempt.map (fun body -> Global(Positioned(name, pos), ConstExpr(body)))
        | head :: _ -> head |> failUnexpected "constant identifier"
        | [] -> failUnexpectedEOF "constant identifier"
    
    let parseDeclaration source : GenericAttempt<Declaration, Positioned<string>> = 
        match source with
        | T Asm :: tail -> parseAsmfunc tail
        | T Def :: tail -> parseDefFunc tail
        | T Const :: tail -> parseConst tail
        | head :: _ -> head |> failUnexpected "\"using\" or \"asm\" or \"def\" or \"const\""
        | [] -> failUnexpectedEOF "declaration"
    
    source |> SeqExt.split (fun x -> x.value = Asm || x.value = Def || x.value = Const)
    |> List.map parseDeclaration
    |> Attempt.liftList

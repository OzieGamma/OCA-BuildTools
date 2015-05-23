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

type Tree = List<NamespaceDecl>

and NamespaceDecl = 
    | NamespaceDecl of _name : Positioned<string> * _elements : List<NamespaceElement>
    
    member this.position = 
        match this with
        | NamespaceDecl(Positioned(_, pos), _) -> pos
    
    member this.name = 
        match this with
        | NamespaceDecl(name, _) -> name
    
    member this.elements = 
        match this with
        | NamespaceDecl(_, elements) -> elements

and NamespaceElement = 
    | UsingStatement of name : Positioned<string>
    | ConstStatement of name : Positioned<string> * expr : ConstExpr
    | AsmFunction of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | AsmOperator of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : List<Positioned<Instr>>
    | DefFunction of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    | DefOperator of name : Positioned<string> * arguments : List<Positioned<string> * Positioned<WaldoType>> * returnType : Positioned<WaldoType> * body : StatementBlock
    member this.position = 
        match this with
        | UsingStatement(Positioned(_, p)) -> p
        | ConstStatement(Positioned(_, p), _) -> p
        | AsmFunction(Positioned(_, p), _, _, _) -> p
        | AsmOperator(Positioned(_, p), _, _, _) -> p
        | DefFunction(Positioned(_, p), _, _, _) -> p
        | DefOperator(Positioned(_, p), _, _, _) -> p

and StatementBlock = List<Statement>

and Statement = 
    | VarDeclaration of pos : Position * vars : List<Positioned<string> * Positioned<WaldoType>> * value : Expr
    | ValDeclaration of pos : Position * vals : List<Positioned<string> * Positioned<WaldoType>> * value : Expr
    | Assignment of pos : Position * names : List<string> * value : Expr
    | MethodCallStatement of name : Positioned<string> * this : Option<Expr> * args : Expr
    | If of expr : Expr * body : StatementBlock
    | IfElse of expr : Expr * thn : StatementBlock * els : StatementBlock
    | WhileLoop of expr : Expr * body : StatementBlock
    | ForLoop of varName : Positioned<string> * startExpr : Expr * endExpr : Expr * body : StatementBlock
    | Return of Expr
    member this.position = 
        match this with
        | VarDeclaration(p, _, _) -> p
        | ValDeclaration(p, _, _) -> p
        | Assignment(p, _, _) -> p
        | MethodCallStatement(Positioned(_, p), _, _) -> p
        | If(expr, _) -> expr.position
        | IfElse(expr, _, _) -> expr.position
        | WhileLoop(expr, _) -> expr.position
        | ForLoop(Positioned(_, p), _, _, _) -> p
        | Return expr -> expr.position

and Expr = 
    | Variable of name : Positioned<string>
    | OperatorCall of left : Expr * op : Positioned<string> * right : Expr
    | UnaryOperatorCall of op : Positioned<string> * expr : Expr
    | MethodCall of name : Positioned<string> * this : Option<Expr> * args : Expr
    | TupleExpr of pos : Position * List<Expr>
    | ConstExpr of ConstExpr
    member this.position = 
        match this with
        | Variable(Positioned(_, p)) -> p
        | OperatorCall(left, _, _) -> left.position
        | UnaryOperatorCall(Positioned(_, p), _) -> p
        | MethodCall(Positioned(_, p), _, _) -> p
        | TupleExpr(p, _) -> p
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
    let failUnexpected expected (head : Positioned<'T>) = fail (sprintf "Expected %s but got %A instead" expected head.value) head.position
    
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
        let rec inner source = 
            let rec innerInner acc source = 
                let tpe, tail = inner source
                match tail with
                | T Comma :: tail -> innerInner (tpe :: acc) tail
                | _ -> (tpe :: acc), tail
            match source with
            | Positioned(Id "Unit", pos) :: tail -> Ok(Positioned(Tuple [], pos)), tail
            | Positioned(LeftParen, pos) :: T RightParen :: tail -> Ok(Positioned(Tuple [], pos)), tail
            | Positioned(Id name, pos) :: tail -> Ok(Positioned(Identifier name, pos)), tail
            | Positioned(LeftParen, pos) :: tail -> 
                let tpes, tail = innerInner [] tail
                match tail with
                | T RightParen :: tail -> 
                    tpes
                    |> Attempt.liftList
                    |> Attempt.map List.rev
                    |> Attempt.map (List.map Position.remove)
                    |> Attempt.bind (fun tpes -> 
                           if tpes.Length = 1 then Ok tpes.Head
                           else Ok(Tuple tpes))
                    |> Attempt.map (Position.add pos), tail
                | head :: tail -> head |> failUnexpected "type end ')'", tail
                | [] -> failUnexpectedEOF "type end ')'", []
            | Positioned(LeftBracket, pos) :: tail -> 
                let fromTpe, fromTail = inner tail
                match fromTail with
                | T(Operator "=>") :: tail -> 
                    let toTpe, toTail = inner tail
                    match toTail with
                    | T RightBracket :: tail -> 
                        (fromTpe, toTpe)
                        |> Attempt.lift2
                        |> Attempt.map (fun (f, t) -> Positioned(FuncType(f.value, t.value), pos)), tail
                    | head :: tail -> head |> failUnexpected "] in func type", tail
                    | [] -> failUnexpectedEOF "] in func type", []
                | head :: tail -> head |> failUnexpected "=> in func type", tail
                | [] -> failUnexpectedEOF "=> in func type", []
            | head :: tail -> head |> failUnexpected "type", tail
            | [] -> failUnexpectedEOF "type", []
        match source with
        | T Colon :: tail -> inner tail
        | Positioned(_, pos) :: tail -> Ok(Positioned(Unknown, pos)), source
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
    
    let parseUsing (source : List<Positioned<Token>>) = 
        let name, nameTail = parseNamespaceName "" source
        match nameTail with
        | head :: _ -> head |> failUnexpected "Using statement should only contain a namespace name"
        | [] -> name |> Attempt.map UsingStatement
    
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
        let rec parseTupleExprEnd pos acc source = 
            let entry, tail = parseExpr source
            match tail with
            | T Comma :: tail -> parseTupleExprEnd pos (entry :: acc) tail
            | T RightParen :: tail -> 
                let expr = 
                    (entry :: acc)
                    |> List.rev
                    |> Attempt.liftList
                    |> Attempt.map (fun exprs -> TupleExpr(pos, exprs))
                expr, tail
            | head :: _ -> head |> failUnexpected ") or , in tuple expr", tail
            | [] -> failUnexpectedEOF ") or , in tuple expr", tail
        
        let rec parseSimpleExpr source = 
            match source with
            | Positioned(LeftParen, pos) :: T RightParen :: tail -> Ok(TupleExpr(pos, [])), tail
            | Positioned(LeftParen, pos) :: tail -> 
                let left, tail = parseExpr tail
                match tail with
                | T Comma :: tail -> parseTupleExprEnd pos [ left ] tail
                | T RightParen :: tail -> left, tail
                | head :: _ -> head |> failUnexpected ") in expr", tail
                | [] -> failUnexpectedEOF ") in expr", tail
            | Positioned(Id funcName, pos) :: T LeftParen :: tail -> 
                let args, tail = parseExpr source.Tail
                let expr = args |> Attempt.map (fun args -> MethodCall(Positioned(funcName, pos), None, args))
                expr, tail
            | Positioned(Id _, _) :: T Colon :: _ -> 
                let name, tail = parseNamespaceName "" source
                let args, tail = parseExpr tail
                let expr = (name, args) |> Attempt.lift2curriedMap (fun name args -> MethodCall(name, None, args))
                expr, tail
            | Positioned(Operator op, pos) :: tail -> 
                let expr, tail = parseExpr tail
                let expr = expr |> Attempt.map (fun expr -> UnaryOperatorCall(Positioned(op, pos), expr))
                expr, tail
            | Positioned(Id varName, pos) :: tail -> Ok(Variable(Positioned(varName, pos))), tail
            | other -> 
                let constExpr, tail = parseConstExpr other
                (constExpr |> Attempt.map ConstExpr), tail
        
        let left, tail = parseSimpleExpr source
        match tail with
        | Positioned(Operator op, pos) :: tail -> 
            let right, tail = parseSimpleExpr tail
            (left, right) |> Attempt.lift2curriedMap (fun left right -> OperatorCall(left, Positioned(op, pos), right)), tail
        | Positioned(Dot, pos) :: tail -> 
            let name, tail = parseNamespaceName "" tail
            let args, tail = parseExpr tail
            let expr = (left, name, args) |> Attempt.lift3curriedMap (fun this name args -> MethodCall(name, Some(this), args))
            expr, tail
        | _ -> left, tail
    
    let rec parseStatementBlock source = 
        let rec parseStatementBlockBody source = 
            let parseVarOrVal node pos tail = 
                let vals, tail = parseVarValDeclList tail
                match tail with
                | T Equals :: tail -> 
                    let expr, tail = parseExpr tail
                    let statement = (vals, expr) |> Attempt.lift2curriedMap (fun vals expr -> node (pos, vals, expr))
                    statement :: parseStatementBlockBody tail
                | head :: tail -> failUnexpected "Equals in val or var statement" head :: parseStatementBlockBody tail
                | [] -> failUnexpectedEOF "Equals in val or var statement" :: []
            
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
            
            match source with
            | Positioned(Val, pos) :: tail -> parseVarOrVal ValDeclaration pos tail
            | Positioned(Var, pos) :: tail -> parseVarOrVal VarDeclaration pos tail
            | T For :: tail -> parseForLoop tail
            | T While :: tail -> parseWhileLoop tail
            | head :: tail -> parseMethodCallStatment source
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
            |> Attempt.map (fun body -> ConstStatement(Positioned(name, pos), body))
        | head :: _ -> head |> failUnexpected "constant identifier"
        | [] -> failUnexpectedEOF "constant identifier"
    
    let parseNamespaceElement source : GenericAttempt<NamespaceElement, Positioned<string>> = 
        match source with
        | T Using :: tail -> parseUsing tail
        | T Asm :: tail -> parseAsmfunc tail
        | T Def :: tail -> parseDefFunc tail
        | T Const :: tail -> parseConst tail
        | head :: _ -> head |> failUnexpected "\"using\" or \"asm\" or \"def\" or \"const\""
        | [] -> failUnexpectedEOF "namespace member"
    
    let parseNamespace source : GenericAttempt<NamespaceDecl, Positioned<string>> = 
        let parseNamespaceBody source = 
            source
            |> blockInsides
            |> failIfTailNotEmpry "namespace block end"
            |> Attempt.map (SeqExt.split (fun x -> x.value = Asm || x.value = Def || x.value = Const))
            |> Attempt.bind (fun l -> 
                   l
                   |> List.map parseNamespaceElement
                   |> Attempt.liftList)
        match source with
        | Positioned(Namespace, _) :: tail -> 
            let name, nameTail = parseNamespaceName "" tail
            let elements = parseNamespaceBody nameTail
            (name, elements)
            |> Attempt.lift2
            |> Attempt.map NamespaceDecl
        | head :: _ -> head |> failUnexpected "namespace"
        | [] -> failUnexpectedEOF "namespace"
    
    let nameSpacesSource = source |> SeqExt.split (fun x -> x.value = Namespace)
    nameSpacesSource
    |> List.map parseNamespace
    |> Attempt.liftList

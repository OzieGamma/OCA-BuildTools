// --------------------------------------------------------------------------------------------------------------------
// <copyright file="ParserTest.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.ParserTest

open NUnit.Framework
open OCA.AsmLib
open OCA.WaldoCompiler
open OCA.WaldoCompiler.Parser
open OFuncLib

let pos = Position.addZero
let p = Position.zero

let parse source = 
    source
    |> Lexer.tokenizeFile "f"
    |> Attempt.map (List.map Position.remove)
    |> Attempt.map (List.map Position.addZero)
    |> Attempt.bind (Parser.parseFile "f")

let makeNamespace name elements = NamespaceDecl(pos name, elements)

[<Test>]
let ``Parser should be able to parse an empty namespace``() = 
    let test1 = "namespace System {}", [ makeNamespace "System" [] ]
    let test2 = "namespace System:Collections:Generic {}", [ makeNamespace "System;Collections;Generic" [] ]
    
    let test3 = 
        "namespace System:Collections:Generic {} namespace System {}", 
        [ makeNamespace "System;Collections;Generic" []
          makeNamespace "System" [] ]
    [ test1; test2; test3 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with a using clause``() = 
    let test1 = "namespace System { using System:Do:not:Try }", [ makeNamespace "System" [ UsingStatement(pos "System;Do;not;Try") ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with a const clause``() = 
    let test1 = "namespace System { const TIME = 2 }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstInt(pos 2I)) ] ]
    let test2 = "namespace System { const TIME = 2u }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstUInt(pos 2I)) ] ]
    let test3 = "namespace System { const TIME = 'T' }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstChar(pos 'T')) ] ]
    let test4 = "namespace System { const TIME = \"T\" }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstStr(pos "T")) ] ]
    let test5 = "namespace System { const TIME = true }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstBool(pos true)) ] ]
    let test6 = "namespace System { const TIME = false }", [ makeNamespace "System" [ ConstStatement(pos "TIME", ConstBool(pos false)) ] ]
    [ test1; test2; test3; test4; test5; test6 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with an asm function``() = 
    let test1 = 
        "namespace System { asm Add():Unit { add s4 s4 s4 }}", 
        [ makeNamespace "System" [ AsmFunction(pos "Add", [], pos (Tuple []), [ pos (Add(SReg 4us, SReg 4us, SReg 4us)) ]) ] ]
    let test2 = 
        "namespace System {asm Add(a: Bool):(Int, Int) {ret}}", 
        [ makeNamespace "System" 
              [ AsmFunction(pos "Add", [ pos "a", pos (Identifier "Bool") ], pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])), [ pos Ret ]) ] ]
    
    let test3 = 
        "namespace System {asm Add(a: Int, b: (Int, Int)):(Int, Int) {ret}}", 
        [ makeNamespace "System" 
              [ AsmFunction
                    (pos "Add", 
                     [ pos "a", pos (Identifier "Int")
                       pos "b", pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])) ], pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])), [ pos Ret ]) ] ]
    [ test1; test2; test3 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with an asm operator``() = 
    let test1 = 
        "namespace System { asm +*():Unit { add s4 s4 s4 }}", 
        [ makeNamespace "System" [ AsmOperator(pos "+*", [], pos (Tuple []), [ pos (Add(SReg 4us, SReg 4us, SReg 4us)) ]) ] ]
    let test2 = 
        "namespace System {asm +*(a: Bool):(Int, Int) {ret}}", 
        [ makeNamespace "System" 
              [ AsmOperator(pos "+*", [ pos "a", pos (Identifier "Bool") ], pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])), [ pos Ret ]) ] ]
    
    let test3 = 
        "namespace System {asm +*(a: Int, b: (Int, Int)):(Int, Int) {ret}}", 
        [ makeNamespace "System" 
              [ AsmOperator
                    (pos "+*", 
                     [ pos "a", pos (Identifier "Int")
                       pos "b", pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])) ], pos (Tuple(Identifier "Int" :: Identifier "Int" :: [])), [ pos Ret ]) ] ]
    [ test1; test2; test3 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with a def function``() = 
    let test1 = "namespace System { def Add():Unit {}}", [ makeNamespace "System" [ DefFunction(pos "Add", [], pos (Tuple []), []) ] ]
    let test2 = 
        "namespace System { def Add():Unit { var a : Int = 0 }}", 
        [ makeNamespace "System" 
              [ DefFunction(pos "Add", [], pos (Tuple []), [ VarDeclaration(p, (pos "a", pos (Identifier "Int")) :: [], ConstExpr(ConstInt(pos 0I))) ]) ] ]
    [ test1; test2 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a namespace with a def operator``() = 
    let test1 = "namespace System { def +():Unit {}}", [ makeNamespace "System" [ DefOperator(pos "+", [], pos (Tuple []), []) ] ]
    let test2 = 
        "namespace System { def +():Unit { var a: Int = 0 }}", 
        [ makeNamespace "System" 
              [ DefOperator(pos "+", [], pos (Tuple []), [ VarDeclaration(p, (pos "a", pos (Identifier "Int")) :: [], ConstExpr(ConstInt(pos 0I))) ]) ] ]
    [ test1; test2 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a for loop``() = 
    let test1 = 
        "namespace System { def +():Unit { for i = 0u upto 42u {} }}", 
        [ makeNamespace "System" 
              [ DefOperator(pos "+", [], pos (Tuple []), [ ForLoop(pos "i", ConstExpr(ConstUInt(pos 0I)), ConstExpr(ConstUInt(pos 42I)), []) ]) ] ]
    let test2 = 
        "namespace System { def +():Unit { for i = 0 upto 42 { val k = 5 var z = 7u } }}", 
        [ makeNamespace "System" 
              [ DefOperator
                    (pos "+", [], pos (Tuple []), 
                     [ ForLoop
                           (pos "i", ConstExpr(ConstInt(pos 0I)), ConstExpr(ConstInt(pos 42I)), 
                            ValDeclaration(p, (pos "k", pos Unknown) :: [], ConstExpr(ConstInt(pos 5I))) 
                            :: VarDeclaration(p, (pos "z", pos Unknown) :: [], ConstExpr(ConstUInt(pos 7I))) :: []) ]) ] ]
    [ test1; test2 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a while loop``() = 
    let test1 = 
        "namespace System { def +():Unit { while i < 42u {} }}", 
        [ makeNamespace "System" 
              [ DefOperator(pos "+", [], pos (Tuple []), [ WhileLoop(OperatorCall(Variable(pos "i"), pos "<", ConstExpr(ConstUInt(pos 42I))), []) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a function type``() = 
    let test1 = 
        "namespace System { asm map(f:[Int => (Int, Int)]): Unit {}}", 
        [ makeNamespace "System" 
              [ AsmFunction(pos "map", [ (pos "f", pos (FuncType(Identifier "Int", Tuple(Identifier "Int" :: Identifier "Int" :: [])))) ], pos (Tuple []), []) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a method call``() = 
    let test1 = 
        "namespace System { def map(): Unit { malloc(3) }}", 
        [ makeNamespace "System" [ DefFunction(pos "map", [], pos (Tuple []), [ MethodCallStatement(pos "malloc", None, ConstExpr(ConstInt(pos 3I))) ]) ] ]
    let test2 = 
        "namespace System { def map(): Unit { val n = malloc(4) }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos (Tuple []), 
                     [ ValDeclaration(p, (pos "n", pos Unknown) :: [], MethodCall(pos "malloc", None, ConstExpr(ConstInt(pos 4I)))) ]) ] ]
    let test3 = 
        "namespace System { def map(): Unit { val n = 42u.malloc(4) }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos (Tuple []), 
                     [ ValDeclaration
                           (p, (pos "n", pos Unknown) :: [], MethodCall(pos "malloc", Some(ConstExpr(ConstUInt(pos 42I))), ConstExpr(ConstInt(pos 4I)))) ]) ] ]
    let test4 = 
        "namespace System { def map(): Unit { 5.malloc() }}", 
        [ makeNamespace "System" 
              [ DefFunction(pos "map", [], pos (Tuple []), [ MethodCallStatement(pos "malloc", Some(ConstExpr(ConstInt(pos 5I))), TupleExpr(p, [])) ]) ] ]
    let test5 = 
        "namespace System { def map(): Unit { System:malloc(6) }}", 
        [ makeNamespace "System" [ DefFunction(pos "map", [], pos (Tuple []), [ MethodCallStatement(pos "System;malloc", None, ConstExpr(ConstInt(pos 6I))) ]) ] ]
    [ test1; test2; test3; test4; test5 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse an operator call``() = 
    let test1 = 
        "namespace System { def map(): Unit { val n = 4 + 2 }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos (Tuple []), 
                     [ ValDeclaration(p, (pos "n", pos Unknown) :: [], OperatorCall(ConstExpr(ConstInt(pos 4I)), pos "+", ConstExpr(ConstInt(pos 2I)))) ]) ] ]
    let test2 = 
        "namespace System { def map(): Unit { val a = b + (4 + c) }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos (Tuple []), 
                     [ ValDeclaration
                           (p, (pos "a", pos Unknown) :: [], 
                            OperatorCall(Variable(pos "b"), pos "+", OperatorCall(ConstExpr(ConstInt(pos 4I)), pos "+", Variable(pos "c")))) ]) ] ]
    [ test1; test2 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse several statements``() = 
    let test1 = 
        "namespace System { def map(): Unit { var a:Int = 0 var b: Int = 0 val c : Int = a + b }}", 
        [ makeNamespace "System" 
              [ DefFunction(pos "map", [], pos (Tuple []), 
                            [ VarDeclaration(p, (pos "a", pos (Identifier "Int")) :: [], ConstExpr(ConstInt(pos 0I)))
                              VarDeclaration(p, (pos "b", pos (Identifier "Int")) :: [], ConstExpr(ConstInt(pos 0I)))
                              ValDeclaration(p, (pos "c", pos (Identifier "Int")) :: [], OperatorCall(Variable(pos "a"), pos "+", Variable(pos "b"))) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse undefined types``() = 
    let test1 = 
        "namespace System { def map() { var a = 0 var b = 0 val c = a + b }}", 
        [ makeNamespace "System" 
              [ DefFunction(pos "map", [], pos Unknown, 
                            [ VarDeclaration(p, (pos "a", pos Unknown) :: [], ConstExpr(ConstInt(pos 0I)))
                              VarDeclaration(p, (pos "b", pos Unknown) :: [], ConstExpr(ConstInt(pos 0I)))
                              ValDeclaration(p, (pos "c", pos Unknown) :: [], OperatorCall(Variable(pos "a"), pos "+", Variable(pos "b"))) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse tuple expressions``() = 
    let test1 = 
        "namespace System { def map() { var a: (Int, UInt) = (0, 0u) }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos Unknown, 
                     [ VarDeclaration
                           (p, (pos "a", pos (Tuple(Identifier "Int" :: Identifier "UInt" :: []))) :: [], 
                            TupleExpr(p, ConstExpr(ConstInt(pos 0I)) :: ConstExpr(ConstUInt(pos 0I)) :: [])) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse tuple unfolding``() = 
    let test1 = 
        "namespace System { def map() { var (a: Int, b: UInt) = (0, 0u) }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos Unknown, 
                     [ VarDeclaration
                           (p, ((pos "a", pos (Identifier "Int")) :: (pos "b", pos (Identifier "UInt")) :: []), 
                            TupleExpr(p, ConstExpr(ConstInt(pos 0I)) :: ConstExpr(ConstUInt(pos 0I)) :: [])) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse unary minus``() = 
    let test1 = 
        "namespace System { def map() { var a = -4 }}", 
        [ makeNamespace "System" 
              [ DefFunction
                    (pos "map", [], pos Unknown, [ VarDeclaration(p, (pos "a", pos Unknown) :: [], UnaryOperatorCall(pos "-", ConstExpr(ConstInt(pos 4I)))) ]) ] ]
    [ test1 ] |> testOnDataMapAttempt parse

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

[<Test>]
let ``Parser should be able to parse nothing``() = [ "", [] ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a method call``() = 
    let input1 = "Void main() {} Void __main() { main() }"
    
    let output1 = 
        [ DefFunction([], pos "main", [], pos Void, [])
          DefFunction([], pos "__main", [], pos Void, [ MethodCallStatement(pos "main", []) ]) ]
    
    let input2 = "Void main(Int a, Int b) {} Void __main() { Int a = 3 Int b = 5 main(a, b) }"
    
    let output2 = 
        [ DefFunction([], pos "main", 
                      [ pos "a", pos (Identifier "Int")
                        pos "b", pos (Identifier "Int") ], pos Void, [])
          DefFunction([], pos "__main", [], pos Void, 
                      [ VarDeclaration(pos "a", pos (Identifier "Int"), ConstExpr(ConstInt(pos 3I)))
                        VarDeclaration(pos "b", pos (Identifier "Int"), ConstExpr(ConstInt(pos 5I)))
                        MethodCallStatement(pos "main", 
                                            [ Variable(pos "a")
                                              Variable(pos "b") ]) ]) ]
    [ input1, output1
      input2, output2 ]
    |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse a simple program``() = 
    let input = "Void __main() { Int a = 3 }"
    let output = 
        [ DefFunction([], pos "__main", [], pos Void, [ VarDeclaration(pos "a", pos (Identifier "Int"), ConstExpr(ConstInt(pos 3I))) ]) ]
    [ input, output ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse attributes``() = 
    let input = "[Inline] Void __main() { }"
    let output = [ DefFunction([ pos "Inline" ], pos "__main", [], pos Void, []) ]
    [ input, output ] |> testOnDataMapAttempt parse

[<Test>]
let ``Parser should be able to parse asm functions``() = 
    let input = "[Asm] Void __main() { nop }"
    let output = [ AsmFunction([ pos "Asm" ], pos "__main", [], pos Void, [ pos Nop ]) ]
    [ input, output ] |> testOnDataMapAttempt parse

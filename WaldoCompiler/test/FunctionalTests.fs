// --------------------------------------------------------------------------------------------------------------------
// <copyright file="FunctionalTests.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.FunctionalTests

open OFuncLib
open NUnit.Framework
open OCA.AsmLib
open OCA.WaldoCompiler

let pos = Position.addZero
let p = Position.zero

let compile source = 
    source
    |> Program.compile "file"
    |> Attempt.map (List.map Position.remove)

[<Test>]
let ``Should compile a method call``() = 
    let input = "void main() {} void __main() { main() }"
    
    let output = 
        [ Label "func;__main"
          Calli(LabelRef("func;main", 0I))
          Label "func;main" ]
    [ input, output ] |> testOnDataMapAttempt compile

[<Test>]
let ``Should compile a simple program``() = 
    let input = "void __main() { int a = 3 int b = 5 int c = a + b }"
    
    let output = 
        [ Label "func;__main"
          Addi(TReg(0us), Zero, Value(3I))
          Addi(TReg(1us), Zero, Value(5I))
          Add(TReg(2us), TReg(0us), TReg(1us)) ]
    [ input, output ] |> testOnDataMapAttempt compile

[<Test>]
let ``Should compile an asm function``() = 
    let input = "[Asm] void __main() { nop }"
    
    let output = 
        [ Label "func;__main"
          Nop ]
    [ input, output ] |> testOnDataMapAttempt compile

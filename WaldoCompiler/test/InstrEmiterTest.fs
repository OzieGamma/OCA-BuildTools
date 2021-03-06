﻿// --------------------------------------------------------------------------------------------------------------------
// <copyright file="InstrEmiterTest.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.InstrEmiterTest

open NUnit.Framework
open OCA.AsmLib
open OCA.WaldoCompiler
open OCA.WaldoCompiler.Parser
open OFuncLib

let pos = Position.addZero
let p = Position.zero

let emit source = 
    source
    |> InstrEmiter.emit
    |> Attempt.map (List.map Position.remove)

[<Test>]
let ``Should emit correctly for a simple method call``() = 
    let input = 
        [ DefFunction([], pos "main", [], pos Void, [])
          DefFunction([], pos "__main", [], pos Void, [ MethodCallStatement(pos "main", []) ]) ]
    
    let output = 
        [ Label "func;__main"
          Calli(LabelRef("func;main", 0I))
          Ret
          Label "func;main"
          Ret ]
    
    [ input, output ] |> testOnDataMapAttempt emit

[<Test>]
let ``Should emit correctly for a method call with arguments``() = 
    let input = 
        [ DefFunction([], pos "main", [ (pos "a", pos (Identifier "Int")) ], pos Void, [])
          DefFunction([], pos "__main", [], pos Void, [ MethodCallStatement(pos "main", [ ConstExpr(ConstInt(pos 5I)) ]) ]) ]
    
    let output = 
        [ Label "func;__main"
          Addi(SReg(0us), Zero, Value(5I))
          Calli(LabelRef("func;main", 0I))
          Ret
          Label "func;main"
          Ret ]
    
    [ input, output ] |> testOnDataMapAttempt emit

[<Test>]
let ``Should emit correctly for a simple program``() = 
    let input = 
        [ DefFunction
              ([], pos "__main", [], pos Void, 
               [ VarDeclaration(pos "a", pos (Identifier "Int"), ConstExpr(ConstInt(pos 3I))) ]) ]
    
    let output = 
        [ Label "func;__main"
          Addi(TReg(0us), Zero, Value(3I))
          Add(TReg(2us), TReg(0us), TReg(1us))
          Ret ]
    [ input, output ] |> testOnDataMapAttempt emit

[<Test>]
let ``Should emit correctly for asm functions``() = 
    let input = 
        [ AsmFunction([ pos "Asm" ], pos "__main", [], pos Void, 
                      [ pos Nop
                        pos Ret ]) ]
    
    let output = 
        [ Label "func;__main"
          Nop
          Ret ]
    
    [ input, output ] |> testOnDataMapAttempt emit

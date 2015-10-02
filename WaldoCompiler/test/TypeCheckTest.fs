// --------------------------------------------------------------------------------------------------------------------
// <copyright file="TypeCheckTest.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.TypeCheckTest

open NUnit.Framework
open OCA.AsmLib
open OCA.WaldoCompiler
open OCA.WaldoCompiler.Parser
open OFuncLib

let pos = Position.addZero
let p = Position.zero

[<Test>]
let ``TypeCheck should reject invalid type in variable declaration``() = 
    [ DefFunction
          ([], pos "main", [], pos Void, 
           [ VarDeclaration(pos "a", pos (Identifier "int"), ConstExpr(ConstBool(pos true))) ])
      DefFunction([], pos "main", [], pos (Identifier "Int"), []) ]
    |> UniquenessVerificator.verify
    |> shouldFail

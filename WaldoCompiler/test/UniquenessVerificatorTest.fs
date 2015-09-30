// --------------------------------------------------------------------------------------------------------------------
// <copyright file="UniquenessVerificatorTest.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.UniquenessVerificatorTest

open NUnit.Framework
open OCA.AsmLib
open OCA.WaldoCompiler
open OCA.WaldoCompiler.Parser
open OFuncLib

let pos = Position.addZero
let p = Position.zero

[<Test>]
let ``Parser should detect two functions with the same name``() = 
    [ DefFunction([], pos "main", [], pos Void, [])
      DefFunction([], pos "main", [], pos (Identifier "int"), []) ]
    |> UniquenessVerificator.verify
    |> shouldFail

[<Test>]
let ``Parser should accept two different functions``() = 
    let data = 
        [ DefFunction([], pos "main", [], pos Void, [])
          DefFunction([], pos "other", [], pos Void, []) ]
    [ data, data ] |> testOnDataMapAttempt UniquenessVerificator.verify

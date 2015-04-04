// --------------------------------------------------------------------------------------------------------------------
// <copyright file="ImmTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.ImmTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let immTokenData = 
    [ Id "kevin42kevin" :: [], LabelRef("kevin42kevin", 0I)
      Id "ozg" :: Operator "+" :: IntLit 42I :: [], LabelRef("ozg", 42I)
      Id "ozg" :: Operator "+" :: UIntLit 42I :: [], LabelRef("ozg", 42I)
      Id "ozg" :: Operator "-" :: UIntLit 42I :: [], LabelRef("ozg", -42I)
      Id "ozg" :: Operator "-" :: IntLit 42I :: [], LabelRef("ozg", -42I)
      Id "ozg" :: Operator "-" :: IntLit -42I :: [], LabelRef("ozg", 42I)
      Operator "-" :: IntLit 1I :: [], Value -1I ]

let validLabels = immTokenData |> List.map (fun (_, x) -> x)

(* Imm.fromTokens *)
[<Test>]
let ``Imm.fromTokens accepts valid input``() = immTokenData |> testOnDataMapAttempt Imm.fromTokens

[<Test>]
let ``Imm.fromTokens rejects invalid input``() = 
    [ IntLit 4I :: IntLit 4I :: [] ]
    |> testOnDataShouldFail Imm.fromTokens

(* Imm loop: tokens *)
[<Test>]
let ``Imm.tokens loops correctly``() = validLabels |> testOnDataLoop Imm.toTokens Imm.fromTokens
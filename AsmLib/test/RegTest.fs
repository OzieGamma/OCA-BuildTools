// --------------------------------------------------------------------------------------------------------------------
// <copyright file="RegTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.RegTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let regValidData = 
    [ Id "zero", Reg.Zero
      Id "ra", Reg.RA
      Id "ea", Reg.EA
      Id "sp", Reg.SP
      Id "t3", TReg 3us
      Id "s3", SReg 3us ]

let regValidReg = regValidData |> List.map (fun (_, x) -> x)

[<Test>]
let ``Reg.fromToken works on valid inputs``() = regValidData |> testOnDataMapAttempt Reg.fromToken

[<Test>]
let ``Reg.fromToken rejects invalid inputs``() = 
    [ Id "r-12"
      Id "r17"
      Id "roziegamma"
      Id "t-5"
      Id "t10000000000"
      Id "toziegamma"
      Id "s-4"
      Id "s100000000000"
      Id "soziegamma"
      Id "lala"
      LeftBrace
      Operator "+" ]
    |> testOnDataShouldFail Reg.fromToken

[<Test>]
let ``Reg.toToken loops correctly``() = regValidReg |> testOnDataLoop Reg.toToken Reg.fromToken

// --------------------------------------------------------------------------------------------------------------------
// <copyright file="OStringTest.fs" company="Oswald Maskens">
//   Copyright 2014-2015 Oswald Maskens
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
module OCA.AsmLib.Test.OStringTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let data = 
    [ [ 1u; 65u ], "A"
      [ 2u; 0x44434241u; 0x4645u ], "ABCDEF"
      [ 0u ], "" ]

let byteData = data |> List.map (fun (x, _) -> x)

[<Test>]
let ``OString.fromWords should work on valid input``() = data |> testOnDataMapAttempt OString.fromWords

[<Test>]
let ``OString.fromWords rejects invalid input``() = 
    [ [ 1u; 42u; 0u ] ]
    |> testOnDataShouldFail OString.fromWords

[<Test>]
let ``OString.toWords should loop correctly with fromWords``() = byteData |> testOnDataLoop OString.fromWords OString.toWords

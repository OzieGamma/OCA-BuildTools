// --------------------------------------------------------------------------------------------------------------------
// <copyright file="ParserRejectTest.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Test.ParserRejectTest

open OFuncLib
open NUnit.Framework

open OCA.AsmLib
open OCA.WaldoCompiler

let pos = Position.addZero
let p = Position.zero

let parse source = 
    source
    |> Lexer.tokenizeFile "f"
    |> Attempt.map (List.map Position.remove)
    |> Attempt.map (List.map Position.addZero)
    |> Attempt.bind (Parser.parseFile "f")

[<Test>]
let ``Parser should reject invalid const clauses``() = 
    [ "namespace System { const TIME =  }"; "namespace System { const TIME }"; "namespace System { const TIME = 2u"; "namespace System { const TIME =  \"lala }"; 
      "namespace System { const TIME = 'aa' }" ] |> testOnDataShouldFail parse

// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Program.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.Program

open OFuncLib

open OCA.AsmLib
open OCA.WaldoCompiler

let compile (fileName: string) (source: string) : PositionedListAttempt<Instr> = 
    let tokens = source |> Lexer.tokenizeFile fileName
    tokens
    |> Attempt.bind (Parser.parseFile fileName)
    |> Attempt.bind UniquenessVerificator.verify
    |> Attempt.bind InstrEmiter.emit  

[<EntryPoint>]
let main argv = 
    if argv.Length <> 2 then printfn "Invalid args %A" argv
    else
        let fileName = argv.[0]
        let source = System.IO.File.ReadAllText fileName
        
        let asm = 
            source
            |> compile fileName
        printfn "%A" asm
//            |> Attempt.bind Transform.instrToTokens
//            |> Attempt.map (List.map Position.remove)
//            |> Attempt.map Pretty.print
//            |> Attempt.mapFail Lexer.formatPositionInError
//        match asm with
//        | Ok res -> System.IO.File.WriteAllText(argv.[1], res)
//        | Fail msg -> msg |> List.iter (printfn "%s")
    0 // return an integer exit code

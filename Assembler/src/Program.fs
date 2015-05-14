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
module OCA.Assembler.Program

open OCA.AsmLib
open OFuncLib

let parseInt str = 
    match System.Int32.TryParse str with
    | (true, i) -> Ok i
    | _ -> Fail [ sprintf "Invalid padding %s" str |> Position.addZero ]

[<EntryPoint>]
let main argv = 
    if argv.Length <> 4 && argv.Length <> 5 then printfn "Invalid number of args %i" argv.Length
    else 
        let from = 
            match argv.[0] with
            | "-f" -> FileHelper.readAsmFile argv.[1]
            | "-b" -> FileHelper.readBinFile argv.[1]
            | _ -> Fail [ sprintf "Invalid args %A" argv |> Position.addZero ]
        
        let errors = 
            match argv.[2] with
            | "-f" -> from |> Attempt.bind (fun instr -> FileHelper.writeAsmFile argv.[3] instr)
            | "-b" -> 
                (from, parseInt argv.[4])
                |> Attempt.lift2
                |> Attempt.bind (fun (instr, padding) -> FileHelper.writeBinFile argv.[3] padding instr)
            | "-i" -> 
                (from, parseInt argv.[4])
                |> Attempt.lift2
                |> Attempt.bind (fun (instr, padding) -> FileHelper.writeStringBinFile argv.[3] padding instr)
            | _ -> Fail [ sprintf "Invalid args %A" argv |> Position.addZero ]
        
        match errors |> Attempt.mapFail Lexer.formatPositionInError with
        | Ok() -> printf "Done."
        | Fail errors -> errors |> List.iter (fun s -> printfn "%s" s)
    0 // return an integer exit code

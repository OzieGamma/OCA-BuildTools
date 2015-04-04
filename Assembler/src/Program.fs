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
open System

let bytesToUInt32 file bytes = 
    if (bytes |> Array.length) % 4 <> 0 then Fail [ "Binary files must have a multiple of 4 as number of bytes" |> Position.addZero ]
    else 
        let mutable i = 0
        let words = new System.Collections.Generic.List<uint32>()
        while i < bytes.Length do
            words.Add(BitConverter.ToUInt32(bytes, i))
            i <- i + 4
        words
        |> Seq.mapi (fun i word -> word |> Position.add (Position(uint32 i + 1u, 0u, file)))
        |> Ok

let nop = 
    Nop
    |> Kelos16.polyfill
    |> Attempt.bind (fun nop -> 
           match nop with
           | instr :: [] -> Ok instr
           | _ -> Fail [ "Nop should map to 1 instruction" ])
    |> Attempt.bind (Kelos16.labelResolver (fun _ -> Fail [ "Nop shouldn't depend on address" ]) (fun _ _ -> Fail [ "Nop shouldn't depend on address" ]) 0I)
    |> Attempt.bind Kelos16.toBin

let toBin from = 
    from
    |> Attempt.bind (Transform.instrToBin Kelos16.polyfill Kelos16.labelResolver Kelos16.toBin)
    |> Attempt.mapFail Lexer.formatPositionInError
    |> Attempt.map (Seq.map Position.remove)
    |> Attempt.map List.ofSeq

let pad length (data : List<uint32>) = 
    match Int32.TryParse length with
    | (true, l) when l >= data.Length -> 
        nop |> Attempt.map (fun nop -> 
                   [ data
                     List.init (l - data.Length) (fun _ -> nop) ]
                   |> List.concat)
    | (true, l) -> Fail [ sprintf "Could not fit %d in %d of padding" data.Length l ]
    | (false, _) -> Fail [ "When specifying --pad please put an unsigned integer as next argument" ]

let toStringBin source = source |> Seq.map (fun i -> System.Convert.ToString(int64 i, 2).PadLeft(32, '0'))

[<EntryPoint>]
let main argv = 
    if argv.Length <> 4 && argv.Length <> 5 then printfn "Invalid number of args %i" argv.Length
    else 
        let from = 
            match argv.[0] with
            | "-f" -> 
                System.IO.File.ReadAllText argv.[1]
                |> Lexer.tokenizeFile argv.[1]
                |> Attempt.bind Transform.tokensToInstr
            | "-b" -> 
                System.IO.File.ReadAllBytes argv.[1]
                |> bytesToUInt32 argv.[1]
                |> Attempt.map (Transform.binToInstr Kelos16.fromBin)
            | _ -> Fail [ sprintf "Invalid args %A" argv |> Position.addZero ]
        match argv.[2] with
        | "-f" -> 
            let res = 
                from
                |> Attempt.bind Transform.instrToTokens
                |> Attempt.mapFail Lexer.formatPositionInError
                |> Attempt.map (List.map Position.remove)
            match res with
            | Ok v -> 
                let asm = v |> Pretty.print
                IO.File.WriteAllText(argv.[3], asm)
            | Fail msg -> msg |> List.iter (printfn "%s")
        | "-b" -> 
            let res = 
                from
                |> toBin
                |> Attempt.bind (pad argv.[5])
            match res with
            | Ok v -> 
                let asm = 
                    v
                    |> Seq.collect BitConverter.GetBytes
                    |> Array.ofSeq
                IO.File.WriteAllBytes(argv.[3], asm)
            | Fail msg -> msg |> List.iter (printfn "%s")
        | "-i" -> 
            let res = 
                from
                |> toBin
                |> Attempt.bind (pad argv.[5])
                |> Attempt.map toStringBin
            match res with
            | Ok v -> IO.File.WriteAllLines(argv.[3], v)
            | Fail msg -> msg |> List.iter (printfn "%s")
        | _ -> printfn "Invalid args %A" argv
    0 // return an integer exit code

// --------------------------------------------------------------------------------------------------------------------
// <copyright file="FileReader.fs" company="Oswald Maskens">
//   Copyright 2015 Oswald Maskens
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
/// <sumary>
/// Contains helpers to load files immediately
/// </sumary>
module OCA.AsmLib.FileHelper

open OFuncLib

let private bytesToUInt32 file bytes = 
    if (bytes |> Array.length) % 4 <> 0 then Fail [ "Binary files must have a multiple of 4 as number of bytes" |> Position.addZero ]
    else 
        let mutable i = 0
        let words = new MutableList<uint32>()
        while i < bytes.Length do
            words.Add(System.BitConverter.ToUInt32(bytes, i))
            i <- i + 4
        words
        |> Seq.mapi (fun i word -> word |> Position.add (Position(uint32 i + 1u, 0u, file)))
        |> Ok

[<CompiledName("ReadAsmFile")>]
let public readAsmFile (fileName : string) : PositionedListAttempt<Instr> = 
    System.IO.File.ReadAllText fileName
    |> Lexer.tokenizeFile fileName
    |> Attempt.bind Transform.tokensToInstr

[<CompiledName("ReadUint32File")>]
let public readUint32File (fileName : string) : PositionedListAttempt<uint32> = 
    System.IO.File.ReadAllBytes fileName
    |> bytesToUInt32 fileName
    |> Attempt.map List.ofSeq

[<CompiledName("ReadBinFile")>]
let public readBinFile (fileName : string) : PositionedListAttempt<Instr> = 
    readUint32File fileName
    |> Attempt.map (Transform.binToInstr Kelos16.fromBin)

[<CompiledName("WriteAsmFile")>]
let public writeAsmFile (fileName : string) (instr : seq<Positioned<Instr>>) : GenericAttempt<unit, Positioned<string>> = 
    let res = 
        instr
        |> Transform.instrToTokens
        |> Attempt.map (List.map Position.remove)
    res |> Attempt.map (fun v -> 
               let asm = v |> Pretty.print
               System.IO.File.WriteAllText(fileName, asm))

let private nop = 
    Nop
    |> Kelos16.polyfill
    |> Attempt.bind (fun nop -> 
           match nop with
           | instr :: [] -> Ok instr
           | _ -> Fail [ "Nop should map to 1 instruction" ])
    |> Attempt.bind (Kelos16.labelResolver (fun _ -> Fail [ "Nop shouldn't depend on address" ]) (fun _ _ -> Fail [ "Nop shouldn't depend on address" ]) 0I)
    |> Attempt.bind Kelos16.toBin
    |> Attempt.mapFail Position.addZero

let private toBin from = 
    from
    |> Transform.instrToBin Kelos16.polyfill Kelos16.labelResolver Kelos16.toBin
    |> Attempt.map (Seq.map Position.remove)
    |> Attempt.map List.ofSeq

let private pad length (data : List<uint32>) = 
    if length >= data.Length then 
        nop |> Attempt.map (fun nop -> 
                   [ data
                     List.init (length - data.Length) (fun _ -> nop) ]
                   |> List.concat)
    else Fail [ sprintf "Could not fit %d in %d of padding" data.Length length |> Position.addZero ]

[<CompiledName("writeBinFile")>]
let public writeBinFile (fileName : string) (padding : int) (instr : seq<Positioned<Instr>>) : GenericAttempt<unit, Positioned<string>> = 
    let res = 
        instr
        |> toBin
        |> Attempt.bind (pad padding)
    res |> Attempt.map (fun v -> 
               let asm = 
                   v
                   |> Seq.collect System.BitConverter.GetBytes
                   |> Array.ofSeq
               System.IO.File.WriteAllBytes(fileName, asm))

let private toStringBin source = source |> Seq.map (fun i -> System.Convert.ToString(int64 i, 2).PadLeft(32, '0'))

[<CompiledName("WriteStringBinFile")>]
let public writeStringBinFile (fileName : string) (padding : int) (instr : seq<Positioned<Instr>>) : GenericAttempt<unit, Positioned<string>> = 
    let res = 
        instr
        |> toBin
        |> Attempt.bind (pad padding)
        |> Attempt.map toStringBin
    res |> Attempt.map (fun v -> System.IO.File.WriteAllLines(fileName, v))

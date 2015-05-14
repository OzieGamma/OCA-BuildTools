// --------------------------------------------------------------------------------------------------------------------
// <copyright file="OString.fs" company="Oswald Maskens">
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
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OCA.AsmLib.OString

open OFuncLib

/// <sumary>
/// Decodes an OCA string in byte representation into a .NET string
/// </sumary>
[<CompiledName("FromWords")>]
let fromWords (words : List<uint32>) : Attempt<string> =
    if uint32 words.Length - 1u <> words.Head then
        Fail [sprintf "Invalid OCA string %A" words]
    else
        Ok ""

/// <sumary>
/// Transforms a string into it's OCA byte representation
/// </sumary>
[<CompiledName("ToWords")>]
let toWords (s : string) : Attempt<List<uint32>> = 
    let bytes = System.Text.Encoding.ASCII.GetBytes(s)
    
    let words = 
        bytes
        |> List.ofSeq
        |> List.fold (fun (acc, count) c -> 
               match acc, count with
               | (_ :: tail), 0 -> ((uint32 c <<< 24) :: tail, 1)
               | (head :: tail), 1 -> ((head ||| (uint32 c <<< 16)) :: tail, 2)
               | (head :: tail), 2 -> ((head ||| (uint32 c <<< 8)) :: tail, 3)
               | (head :: tail), 3 -> (0u :: (head ||| uint32 c) :: tail, 0)
               | _ -> failwith "Invalid state Instr.expandMacros") (0u :: [], 0)
        |> (fun (list, _) -> list)
        |> List.rev
    (uint32 words.Length) :: words |> Ok

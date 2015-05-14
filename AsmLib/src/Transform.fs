// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Transform.fs" company="Oswald Maskens">
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
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module OCA.AsmLib.Transform

open OFuncLib

let inline private transform transform source = 
    source
    |> List.map (Positioned.mapAttempt transform)
    |> Attempt.liftList

[<CompiledName("TokensToInstr")>]
let tokensToInstr (source : seq<Positioned<Token>>) : PositionedListAttempt<Instr> = 
    source
    |> SeqExt.splitWithLookAhead (fun head look -> Instr.isInstrStart head.value (look |> Option.map (fun x -> x.value)))
    |> List.map (fun l -> 
           l
           |> List.map Position.remove
           |> Position.add l.Head.position)
    |> List.map (Positioned.mapAttempt Instr.fromTokens)
    |> Attempt.liftList

[<CompiledName("InstrToTokens")>]
let instrToTokens (source : seq<Positioned<Instr>>) : PositionedListAttempt<Token> = 
    source
    |> List.ofSeq
    |> transform Instr.toTokens
    |> Attempt.map (List.map Positioned.liftList)
    |> Attempt.map List.concat

[<CompiledName("InstrToBin")>]
let instrToBin (polyfill : Instr -> Attempt<List<Instr>>) 
    (labelResolver : (Imm -> Attempt<bigint>) -> (bigint -> Imm -> Attempt<bigint>) -> bigint -> Instr -> Attempt<'TInstr>) 
    (instrToBin : 'TInstr -> Attempt<uint32>) (source : seq<Positioned<Instr>>) : PositionedListAttempt<uint32> = 
    let listSrc = source |> List.ofSeq
    
    let notLabelDef (entry : Positioned<Instr>) = 
        match entry.value with
        | Define(_, _) -> false
        | Label(_) -> false
        | _ -> true
    
    // Implemented imperatively, functional was too much of a mess
    let findLabels (source : List<Positioned<Instr>>) : GenericAttempt<Map<string, bigint>, Positioned<string>> = 
        let arr = source |> Array.ofList
        let mutable labels = [] |> Map.ofList
        let duplicates = new MutableList<Positioned<string>>()
        let mutable address = 0I
        for i = 0 to arr.Length - 1 do
            match arr.[i].value with
            | Define(str, v) -> 
                if labels.ContainsKey str then duplicates.Add(Positioned(sprintf "Duplicate definition of %s" str, arr.[i].position))
                else labels <- labels.Add(str, v)
            | Label(str) -> 
                if labels.ContainsKey str then duplicates.Add(Positioned(sprintf "Duplicate definition of %s" str, arr.[i].position))
                else labels <- labels.Add(str, address)
            | _ -> address <- address + 1I
        if duplicates.Count > 0 then Fail(duplicates |> List.ofSeq)
        else Ok labels
    
    let resolveLabels labels instrResolver (address, instr) = 
        let findLabel l offset = 
            labels
            |> Map.tryFind l
            |> Attempt.ofOption [ sprintf "Could not find label %s" l ]
            |> Attempt.map (fun v -> v + offset)
        
        let relativeLabel address imm = 
            match imm with
            | Value v -> Ok v
            | LabelRef(l, offset) -> findLabel l offset |> Attempt.map (fun i -> i - address)
        
        let absoluteLabel imm = 
            match imm with
            | Value v -> Ok v
            | LabelRef(l, offset) -> findLabel l offset |> Attempt.map (fun i -> i)
        
        instr |> Positioned.mapAttempt (instrResolver absoluteLabel relativeLabel address)
    
    let pollyfilledSrc = 
        listSrc
        |> List.map (Positioned.mapAttempt polyfill)
        |> List.map (Attempt.map Positioned.liftList)
        |> Attempt.liftList
        |> Attempt.map List.concat
    
    let resolveLabelsAndAssemble src = 
        let newSource = 
            src
            |> List.filter notLabelDef
            |> List.mapi (fun i v -> (bigint i), v)
        
        let labelsResolved = 
            findLabels src |> Attempt.bind (fun labels -> 
                                  newSource
                                  |> List.map (resolveLabels labels labelResolver)
                                  |> Attempt.liftList)
        
        labelsResolved |> Attempt.bind (fun labelsResolved -> 
                              labelsResolved
                              |> List.map (Positioned.mapAttempt instrToBin)
                              |> Attempt.liftList)
    
    pollyfilledSrc |> Attempt.bind resolveLabelsAndAssemble

[<CompiledName("BinToInstr")>]
let binToInstr (binToInstr : uint32 -> Instr) (source : seq<Positioned<uint32>>) : List<Positioned<Instr>> = 
    source
    |> Seq.toList
    |> List.map (Positioned.map binToInstr)

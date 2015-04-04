(* --------------------------------------------------------------------------------------------------------------------
// <copyright file="Reg.fs" company="Oswald Maskens">
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
// -------------------------------------------------------------------------------------------------------------------- *)

namespace OCA.AsmLib

open OFuncLib

///<sumary>
/// Represents a register
///</sumary>
type Reg = 
    /// <sumary>
    /// Temporary register
    /// </sumary>
    | TReg of uint16
    /// <sumary>
    /// Saved register
    /// </sumary>
    | SReg of uint16
    /// <sumary>
    /// Zero register, always zero
    /// </sumary>
    | Zero
    /// <sumary>
    /// Return address register
    /// </sumary>
    | RA
    /// <sumary>
    /// Exception address register
    /// </sumary>
    | EA
    /// <sumary>
    /// Stack pointer register
    /// </sumary>
    | SP

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Reg = 
    let private parseNum (str : string) = 
        let succes, num = System.UInt16.TryParse(str.Substring(1))
        if succes then Ok num
        else Fail [ sprintf "%s should be a valid UInt16 but is not" str ]
    
    [<CompiledName("FromToken")>]
    let public fromToken (name : Token) : Attempt<Reg> = 
        match name with
        | Id "zero" -> Ok Zero
        | Id "ra" -> Ok RA
        | Id "ea" -> Ok EA
        | Id "sp" -> Ok SP
        | Id n when n.[0] = 't' -> parseNum n |> Attempt.map TReg
        | Id n when n.[0] = 's' -> parseNum n |> Attempt.map SReg
        | _ -> Fail [ sprintf "Can not recognize register %A" name ]
    
    [<CompiledName("ToToken")>]
    let public toToken (reg : Reg) : Attempt<Token> = 
        match reg with
        | Zero -> Ok(Id "zero")
        | RA -> Ok(Id "ra")
        | EA -> Ok(Id "ea")
        | SP -> Ok(Id "sp")
        | TReg t -> Ok(Id("t" + t.ToString()))
        | SReg s -> Ok(Id("s" + s.ToString()))

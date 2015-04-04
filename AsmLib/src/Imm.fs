(* --------------------------------------------------------------------------------------------------------------------
// <copyright file="Imm.fs" company="Oswald Maskens">
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

/// <sumary>
/// An immediate value, either a label or a bigint
/// </sumary>
type Imm = 
    /// <sumary>
    /// An immediate specified as a value
    /// </sumary>
    | Value of bigint
    /// <sumary>
    /// An immediate specified as a label + an offset
    /// </sumary>
    | LabelRef of string * offset : bigint

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Imm = 
    [<CompiledName("FromTokens")>]
    let public fromTokens (tokens : List<Token>) : Attempt<Imm> = 
        match tokens with
        | IntLit i :: [] -> Ok(Value i)
        | UIntLit u :: [] -> Ok(Value u)
        | Operator "-" :: IntLit i :: [] -> Ok(Value(i * -1I))
        | Id label :: [] -> Ok(LabelRef(label, 0I))
        | Id label :: Operator "+" :: IntLit i :: [] -> Ok(LabelRef(label, i))
        | Id label :: Operator "-" :: IntLit i :: [] -> Ok(LabelRef(label, -1I * i))
        | Id label :: Operator "+" :: UIntLit u :: [] -> Ok(LabelRef(label, u))
        | Id label :: Operator "-" :: UIntLit u :: [] -> Ok(LabelRef(label, -1I * u))
        | _ -> Fail [ sprintf "Invalid label usage %A" tokens ]
    
    [<CompiledName("FitsInXBitsSigned")>]
    let public fitsInXBitsSigned bits v = (-1I * (1I <<< (bits - 1))) <= v && v <= ((1I <<< (bits - 1)) - 1I)
    
    [<CompiledName("FitsInXBitsUnsigned")>]
    let public fitsInXBitsUnsigned bits v = 0I <= v && v <= ((1I <<< bits) - 1I)
    
    [<CompiledName("ToTokens")>]
    let public toTokens (imm : Imm) : Attempt<List<Token>> = 
        match imm with
        | Value v when v < 0I -> Ok(Operator "-" :: IntLit(v * -1I) :: [])
        | Value v -> Ok(UIntLit v :: [])
        | LabelRef(l, offset) when offset = 0I -> Ok(Id l :: [])
        | LabelRef(l, offset) when offset > 0I -> Ok(Id l :: Operator "+" :: IntLit offset :: [])
        | LabelRef(l, offset) -> Ok(Id l :: Operator "-" :: IntLit(offset * -1I) :: [])
        | _ -> Fail []

(* --------------------------------------------------------------------------------------------------------------------
// <copyright file="Instr.fs" company="Oswald Maskens">
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

type Instr = 
    /// <sumary>
    /// Not an operation, does nothing
    /// </sumary>
    | Nop
    /// <sumary>
    /// Launches a software interrupt
    /// </sumary>
    | Trap
    /// <sumary>
    /// Switches the CPU to an other mode. (Kernel/User)
    /// </sumary>
    | Switch of imm : Imm
    /// <sumary>
    /// Load word
    /// </sumary>
    | Ldw of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Sets a register to the specified value
    /// </sumary>
    | Set of rS : Reg * imm : Imm
    /// <sumary>
    /// Store word
    /// </sumary>
    | Stw of rB : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Add
    /// </sumary>
    | Add of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Sub
    /// </sumary>
    | Sub of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// And
    /// </sumary>
    | And of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Or
    /// </sumary>
    | Or of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Exclusive or
    /// </sumary>
    | Xor of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Not or
    /// </sumary>
    | Nor of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Not
    /// </sumary>
    | Not of rS : Reg * rA : Reg
    /// <sumary>
    /// Shift left logical
    /// </sumary>
    | Sll of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Shift right logical
    /// </sumary>
    | Srl of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare greater equal
    /// </sumary>
    | Compge of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare less than
    /// </sumary>
    | Complt of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare greater equal unsigned
    /// </sumary>
    | Compgeu of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare less than unsigned
    /// </sumary>
    | Compltu of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare equal
    /// </sumary>
    | Compeq of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Compare not equal
    /// </sumary>
    | Compne of rS : Reg * rA : Reg * rB : Reg
    /// <sumary>
    /// Add immediate
    /// </sumary>
    | Addi of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Subtract immediate
    /// </sumary>
    | Subi of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// And immediate
    /// </sumary>
    | Andi of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Or immediate
    /// </sumary>
    | Ori of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Exclusive or immediate
    /// </sumary>
    | Xori of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Not or immediate
    /// </sumary>
    | Nori of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Shift left logical immediate
    /// </sumary>
    | Slli of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Shift right logical immediate
    /// </sumary>
    | Srli of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare greater equal immediate
    /// </sumary>
    | Compgei of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare less than immediate
    /// </sumary>
    | Complti of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare greater equal unsigned immediate
    /// </sumary>
    | Compgeiu of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare less than unsigned immediate
    /// </sumary>
    | Compltiu of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare equal immediate
    /// </sumary>
    | Compeqi of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Compare nor equal immediate
    /// </sumary>
    | Compnei of rS : Reg * rA : Reg * imm : Imm
    /// <sumary>
    /// Branch
    /// </sumary>
    | Br of imm : Imm
    /// <sumary>
    /// Branch greater than
    /// </sumary>
    | Bge of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Branch less than
    /// </sumary>
    | Blt of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Branch greater than unsigned
    /// </sumary>
    | Bgeu of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Branch less than unsigned
    /// </sumary>
    | Bltu of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Branch equal
    /// </sumary>
    | Beq of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Branch not equal
    /// </sumary>
    | Bne of rA : Reg * rB : Reg * imm : Imm
    /// <sumary>
    /// Call immediate
    /// </sumary>
    | Calli of imm : Imm
    /// <sumary>
    /// Call register
    /// </sumary>
    | Callr of rA : Reg
    /// <sumary>
    /// Return
    /// </sumary>
    | Ret
    /// <sumary>
    /// Inlines an immediate value
    /// </sumary>
    | ImmWord of value : Imm
    /// <sumary>
    /// Defines a label to be equal to a certain value
    /// </sumary>
    | Define of label : string * value : bigint
    /// <sumary>
    /// Puts a label somewhere in the code
    /// </sumary>
    | Label of label : string
    /// <sumary>
    /// Inlines a string as an OCA-string, see string.html
    /// </sumary>
    | Text of string

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Instr = 
    [<CompiledName("IsInstrStart")>]
    let public isInstrStart (str : Token) (lookAhead : Option<Token>) : bool = 
        match str, lookAhead with
        | Id "nop", _ -> true
        | Id "trap", _ -> true
        | Id "switch", _ -> true
        | Id "set", _ -> true
        | Id "ldw", _ -> true
        | Id "stw", _ -> true
        | Id "add", _ -> true
        | Id "sub", _ -> true
        | Id "and", _ -> true
        | Id "or", _ -> true
        | Id "xor", _ -> true
        | Id "nor", _ -> true
        | Id "sll", _ -> true
        | Id "srl", _ -> true
        | Id "compge", _ -> true
        | Id "complt", _ -> true
        | Id "compgeu", _ -> true
        | Id "compltu", _ -> true
        | Id "compeq", _ -> true
        | Id "compne", _ -> true
        | Id "not", _ -> true
        | Id "addi", _ -> true
        | Id "subi", _ -> true
        | Id "andi", _ -> true
        | Id "ori", _ -> true
        | Id "xori", _ -> true
        | Id "nori", _ -> true
        | Id "slli", _ -> true
        | Id "srli", _ -> true
        | Id "compgei", _ -> true
        | Id "complti", _ -> true
        | Id "compgeiu", _ -> true
        | Id "compltiu", _ -> true
        | Id "compeqi", _ -> true
        | Id "compnei", _ -> true
        | Id "br", _ -> true
        | Id "bge", _ -> true
        | Id "blt", _ -> true
        | Id "bgeu", _ -> true
        | Id "bltu", _ -> true
        | Id "beq", _ -> true
        | Id "bne", _ -> true
        | Id "calli", _ -> true
        | Id "callr", _ -> true
        | Id "ret", _ -> true
        | Dot, Some(Id "word") -> true
        | Dot, Some(Def) -> true
        | Dot, Some(Id "text") -> true
        | Id _, Some Colon -> true
        | _ -> false
    
    [<CompiledName("FromTokens")>]
    let public fromTokens (str : seq<Token>) : Attempt<Instr> = 
        let notIgnored c = 
            match c with
            | LeftParen | RightParen | Comma -> false
            | _ -> true
        
        let parts = 
            str
            |> List.ofSeq
            |> List.filter notIgnored
        
        let reg = Reg.fromToken
        let imm = Imm.fromTokens
        let inline isLabelChar c = System.Char.IsLetterOrDigit(c) || c = '_'
        let inline isLabel (label : string) = label.Length > 0 && System.Char.IsLetter label.[0] && label |> Seq.forall isLabelChar
        let inline _Reg instr a = reg a |> Attempt.map instr
        let inline _2Reg instr a b = (reg a, reg b) |> Attempt.lift2tupleMap instr
        let inline _3Reg instr a b c = (reg a, reg b, reg c) |> Attempt.lift3tupleMap instr
        let inline _Imm instr i = imm i |> Attempt.map instr
        let inline _RegImm instr a i = (reg a, imm i) |> Attempt.lift2tupleMap instr
        let inline _2RegImm instr a b i = (reg a, reg b, imm i) |> Attempt.lift3tupleMap instr
        match parts with
        | Id "nop" :: [] -> Ok Nop
        | Id "trap" :: [] -> Ok Trap
        | Id "switch" :: i -> _Imm Switch i
        | Id "ldw" :: rS :: rA :: i -> _2RegImm Ldw rS rA i
        | Id "stw" :: rB :: rA :: i -> _2RegImm Stw rB rA i
        | Id "set" :: rS :: i -> _RegImm Set rS i
        | Id "add" :: rS :: rA :: rB :: [] -> _3Reg Add rS rA rB
        | Id "sub" :: rS :: rA :: rB :: [] -> _3Reg Sub rS rA rB
        | Id "and" :: rS :: rA :: rB :: [] -> _3Reg And rS rA rB
        | Id "or" :: rS :: rA :: rB :: [] -> _3Reg Or rS rA rB
        | Id "xor" :: rS :: rA :: rB :: [] -> _3Reg Xor rS rA rB
        | Id "nor" :: rS :: rA :: rB :: [] -> _3Reg Nor rS rA rB
        | Id "not" :: rS :: rA :: [] -> _2Reg Not rS rA
        | Id "sll" :: rS :: rA :: rB :: [] -> _3Reg Sll rS rA rB
        | Id "srl" :: rS :: rA :: rB :: [] -> _3Reg Srl rS rA rB
        | Id "compge" :: rS :: rA :: rB :: [] -> _3Reg Compge rS rA rB
        | Id "complt" :: rS :: rA :: rB :: [] -> _3Reg Complt rS rA rB
        | Id "compgeu" :: rS :: rA :: rB :: [] -> _3Reg Compgeu rS rA rB
        | Id "compltu" :: rS :: rA :: rB :: [] -> _3Reg Compltu rS rA rB
        | Id "compeq" :: rS :: rA :: rB :: [] -> _3Reg Compeq rS rA rB
        | Id "compne" :: rS :: rA :: rB :: [] -> _3Reg Compne rS rA rB
        | Id "addi" :: rS :: rA :: i -> _2RegImm Addi rS rA i
        | Id "subi" :: rS :: rA :: i -> _2RegImm Subi rS rA i
        | Id "andi" :: rS :: rA :: i -> _2RegImm Andi rS rA i
        | Id "ori" :: rS :: rA :: i -> _2RegImm Ori rS rA i
        | Id "xori" :: rS :: rA :: i -> _2RegImm Xori rS rA i
        | Id "nori" :: rS :: rA :: i -> _2RegImm Nori rS rA i
        | Id "slli" :: rS :: rA :: i -> _2RegImm Slli rS rA i
        | Id "srli" :: rS :: rA :: i -> _2RegImm Srli rS rA i
        | Id "compgei" :: rS :: rA :: i -> _2RegImm Compgei rS rA i
        | Id "complti" :: rS :: rA :: i -> _2RegImm Complti rS rA i
        | Id "compgeiu" :: rS :: rA :: i -> _2RegImm Compgeiu rS rA i
        | Id "compltiu" :: rS :: rA :: i -> _2RegImm Compltiu rS rA i
        | Id "compeqi" :: rS :: rA :: i -> _2RegImm Compeqi rS rA i
        | Id "compnei" :: rS :: rA :: i -> _2RegImm Compnei rS rA i
        | Id "br" :: i -> _Imm Br i
        | Id "bge" :: rA :: rB :: i -> _2RegImm Bge rA rB i
        | Id "blt" :: rA :: rB :: i -> _2RegImm Blt rA rB i
        | Id "bgeu" :: rA :: rB :: i -> _2RegImm Bgeu rA rB i
        | Id "bltu" :: rA :: rB :: i -> _2RegImm Bltu rA rB i
        | Id "beq" :: rA :: rB :: i -> _2RegImm Beq rA rB i
        | Id "bne" :: rA :: rB :: i -> _2RegImm Bne rA rB i
        | Id "calli" :: i -> _Imm Calli i
        | Id "callr" :: rA :: [] -> _Reg Callr rA
        | Id "ret" :: [] -> Ok Ret
        | Dot :: Id "word" :: i -> _Imm ImmWord i
        | Dot :: Def :: Id label :: UIntLit value :: [] when isLabel label -> Ok(Define(label, value))
        | Dot :: Def :: Id label :: IntLit value :: [] when isLabel label -> Ok(Define(label, value))
        | Id label :: Colon :: [] when isLabel label -> Ok(Label label)
        | Dot :: Id "text" :: StringLit(s) :: [] -> Ok(Text s)
        | _ -> Fail [ sprintf "Unrecognized instruction %A" parts ]
    
    [<CompiledName("ToTokens")>]
    let public toTokens (instr : Instr) : Attempt<List<Token>> = 
        let reg = Reg.toToken
        let imm = Imm.toTokens
        let inline _Reg str a = reg a |> Attempt.map (fun a -> Id str :: a :: [])
        let inline _2Reg str a b = (reg a, reg b) |> Attempt.lift2curriedMap (fun a b -> Id str :: a :: Comma :: b :: [])
        let inline _3Reg str a b c = (reg a, reg b, reg c) |> Attempt.lift3curriedMap (fun a b c -> Id str :: a :: Comma :: b :: Comma :: c :: [])
        let inline _RegImm str a i = (reg a, imm i) |> Attempt.lift2curriedMap (fun a i -> Id str :: a :: Comma :: i)
        let inline _2RegImm str a b i = (reg a, reg b, imm i) |> Attempt.lift3curriedMap (fun a b i -> Id str :: a :: Comma :: b :: Comma :: i)
        let inline _Imm str i = imm i |> Attempt.map (fun i -> Id str :: i)
        match instr with
        | Nop -> Ok(Id "nop" :: [])
        | Trap -> Ok(Id "trap" :: [])
        | Switch i -> _Imm "switch" i
        | Set(rS, i) -> _RegImm "set" rS i
        | Ldw(rS, rA, i) -> _2RegImm "ldw" rS rA i
        | Stw(rB, rA, i) -> _2RegImm "stw" rB rA i
        | Add(rS, rA, rB) -> _3Reg "add" rS rA rB
        | Sub(rS, rA, rB) -> _3Reg "sub" rS rA rB
        | And(rS, rA, rB) -> _3Reg "and" rS rA rB
        | Or(rS, rA, rB) -> _3Reg "or" rS rA rB
        | Xor(rS, rA, rB) -> _3Reg "xor" rS rA rB
        | Nor(rS, rA, rB) -> _3Reg "nor" rS rA rB
        | Not(rS, rA) -> _2Reg "not" rS rA
        | Sll(rS, rA, rB) -> _3Reg "sll" rS rA rB
        | Srl(rS, rA, rB) -> _3Reg "srl" rS rA rB
        | Compge(rS, rA, rB) -> _3Reg "compge" rS rA rB
        | Complt(rS, rA, rB) -> _3Reg "complt" rS rA rB
        | Compgeu(rS, rA, rB) -> _3Reg "compgeu" rS rA rB
        | Compltu(rS, rA, rB) -> _3Reg "compltu" rS rA rB
        | Compeq(rS, rA, rB) -> _3Reg "compeq" rS rA rB
        | Compne(rS, rA, rB) -> _3Reg "compne" rS rA rB
        | Addi(rS, rA, i) -> _2RegImm "addi" rS rA i
        | Subi(rS, rA, i) -> _2RegImm "subi" rS rA i
        | Andi(rS, rA, i) -> _2RegImm "andi" rS rA i
        | Ori(rS, rA, i) -> _2RegImm "ori" rS rA i
        | Xori(rS, rA, i) -> _2RegImm "xori" rS rA i
        | Nori(rS, rA, i) -> _2RegImm "nori" rS rA i
        | Slli(rS, rA, i) -> _2RegImm "slli" rS rA i
        | Srli(rS, rA, i) -> _2RegImm "srli" rS rA i
        | Compgei(rS, rA, i) -> _2RegImm "compgei" rS rA i
        | Complti(rS, rA, i) -> _2RegImm "complti" rS rA i
        | Compgeiu(rS, rA, i) -> _2RegImm "compgeiu" rS rA i
        | Compltiu(rS, rA, i) -> _2RegImm "compltiu" rS rA i
        | Compeqi(rS, rA, i) -> _2RegImm "compeqi" rS rA i
        | Compnei(rS, rA, i) -> _2RegImm "compnei" rS rA i
        | Br i -> _Imm "br" i
        | Bge(rA, rB, i) -> _2RegImm "bge" rA rB i
        | Blt(rA, rB, i) -> _2RegImm "blt" rA rB i
        | Bgeu(rA, rB, i) -> _2RegImm "bgeu" rA rB i
        | Bltu(rA, rB, i) -> _2RegImm "bltu" rA rB i
        | Beq(rA, rB, i) -> _2RegImm "beq" rA rB i
        | Bne(rA, rB, i) -> _2RegImm "bne" rA rB i
        | Calli i -> _Imm "calli" i
        | Callr rA -> _Reg "callr" rA
        | Ret -> Ok(Id "ret" :: [])
        | ImmWord i -> imm i |> Attempt.map (fun i -> Dot :: Id "word" :: i)
        | Define(label, v) -> Ok(Dot :: Def :: Id label :: UIntLit v :: [])
        | Label label -> Ok(Id label :: Colon :: [])
        | Text s -> Ok(Dot :: Id "text" :: StringLit s :: [])
    
    /// <sumary>
    /// Removes Nop, Not, Br, Ret, Text instructions
    /// </sumary>
    [<CompiledName("MacroPolyfill")>]
    let macroPolyfill (instr : Instr) : Attempt<List<Instr>> = 
        match instr with
        | Nop -> Ok(Add(Zero, Zero, Zero) :: [])
        | Not(rS, rA) -> Ok(Nor(rS, rA, rA) :: [])
        | Br i -> Ok(Beq(Zero, Zero, i) :: [])
        | Ret -> Ok(Callr(RA) :: [])
        | Text s -> OString.toWords s |> Attempt.map (List.map (fun i -> Instr.ImmWord(Value(bigint i))))
        | _ -> Ok(instr :: [])

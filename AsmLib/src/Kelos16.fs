(* --------------------------------------------------------------------------------------------------------------------
// <copyright file="Kelos16.fs" company="Oswald Maskens">
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

///<sumary>
/// ldw      0000 ---- AAAA SSSS IIII IIII IIII IIII
/// stw      0010 BBBB AAAA ---- IIII IIII IIII IIII
///
/// add      010- BBBB AAAA SSSS 0000 ---- ---- ----
/// addi     011Z 0000 AAAA SSSS IIII IIII IIII IIII
///
/// sub      010- BBBB AAAA SSSS 0001 ---- ---- ----
/// subi     011Z 0001 AAAA SSSS IIII IIII IIII IIII
///
/// and      010- BBBB AAAA SSSS 0010 ---- ---- ----
/// andi     011Z 0010 AAAA SSSS IIII IIII IIII IIII
///
/// or       010- BBBB AAAA SSSS 0011 ---- ---- ----
/// ori      011Z 0011 AAAA SSSS IIII IIII IIII IIII
///
/// xor      010- BBBB AAAA SSSS 0100 ---- ---- ----
/// xori     011Z 0100 AAAA SSSS IIII IIII IIII IIII
///
/// nor      010- BBBB AAAA SSSS 0101 ---- ---- ----
/// nori     011Z 0101 AAAA SSSS IIII IIII IIII IIII
///
/// sll      010- BBBB AAAA SSSS 0110 ---- ---- ----
/// slli     0110 0110 AAAA SSSS IIII IIII IIII IIII
///
/// srl      010- BBBB AAAA SSSS 0111 ---- ---- ----
/// srli     0110 0111 AAAA SSSS IIII IIII IIII IIII
///
/// compge   010- BBBB AAAA SSSS 1000 ---- ---- ----
/// compgei  0111 1000 AAAA SSSS IIII IIII IIII IIII
///
/// complt   010- BBBB AAAA SSSS 1001 ---- ---- ----
/// complti  0111 1001 AAAA SSSS IIII IIII IIII IIII
///
/// compgeu  010- BBBB AAAA SSSS 1010 ---- ---- ----
/// compgeui 0110 1010 AAAA SSSS IIII IIII IIII IIII
///
/// compltu  010- BBBB AAAA SSSS 1011 ---- ---- ----
/// compltui 0110 1011 AAAA SSSS IIII IIII IIII IIII
///
/// compeq   010- BBBB AAAA SSSS 1100 ---- ---- ----
/// compeqi  011Z 1100 AAAA SSSS IIII IIII IIII IIII
///
/// compne   010- BBBB AAAA SSSS 1101 ---- ---- ----
/// compnei  011Z 1101 AAAA SSSS IIII IIII IIII IIII
///
/// PC := PC + 1 + I_s
/// bge     100Z BBBB AAAA 1000 IIII IIII IIII IIII
/// blt     100Z BBBB AAAA 1001 IIII IIII IIII IIII
/// bgeu    100Z BBBB AAAA 1010 IIII IIII IIII IIII
/// bltu    100Z BBBB AAAA 1011 IIII IIII IIII IIII
/// beq     100Z BBBB AAAA 1100 IIII IIII IIII IIII
/// bne     100Z BBBB AAAA 1101 IIII IIII IIII IIII
///
/// callr   1010 ---- AAAA 0001 ---- ---- ---- ----
/// calli   1011 IIII IIII 0001 IIII IIII IIII IIII
///
/// trap    110- ---- ---- ---- ---- ---- ---- ----
/// switch  111Z ---- ---- ---- IIII IIII IIII IIII
///
/// Z = sign extend for the immediate value
/// - `r0/zero` is the zero constant, writing to it has no effect
/// - `r1/ra` is used to store the return address of a subroutine call
/// - `r2/ea` is used to store the exception return address
/// - `r3/sp` is used to store the stack pointer.
/// - `r4-r5` are reserved
/// - `r6-r10` (t0-t4) are used for temporary values. 
///   They must be saved by the caller if needed.
///   Return values of a subroutine are stored in `t0-t4` (`t0` first return value)
/// - `r11-r15` (s0-s4) are used for saved values.
///   They must be saved by the callee if needed.
///   Arguments of a subroutine are passed in `s0-s4` (`s0` first argument)
///</sumary>
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module OCA.AsmLib.Kelos16

open OCA.AsmLib
open OFuncLib

type ResolvedInstr = 
    | RTrap
    | RSwitch of imm : bigint
    | RLdw of rS : uint32 * rA : uint32 * imm : bigint
    | RStw of rB : uint32 * rA : uint32 * imm : bigint
    | RAdd of rS : uint32 * rA : uint32 * rB : uint32
    | RSub of rS : uint32 * rA : uint32 * rB : uint32
    | RAnd of rS : uint32 * rA : uint32 * rB : uint32
    | ROr of rS : uint32 * rA : uint32 * rB : uint32
    | RXor of rS : uint32 * rA : uint32 * rB : uint32
    | RNor of rS : uint32 * rA : uint32 * rB : uint32
    | RSll of rS : uint32 * rA : uint32 * rB : uint32
    | RSrl of rS : uint32 * rA : uint32 * rB : uint32
    | RCompge of rS : uint32 * rA : uint32 * rB : uint32
    | RComplt of rS : uint32 * rA : uint32 * rB : uint32
    | RCompgeu of rS : uint32 * rA : uint32 * rB : uint32
    | RCompltu of rS : uint32 * rA : uint32 * rB : uint32
    | RCompeq of rS : uint32 * rA : uint32 * rB : uint32
    | RCompne of rS : uint32 * rA : uint32 * rB : uint32
    | RAddi of rS : uint32 * rA : uint32 * imm : bigint
    | RSubi of rS : uint32 * rA : uint32 * imm : bigint
    | RAndi of rS : uint32 * rA : uint32 * imm : bigint
    | ROri of rS : uint32 * rA : uint32 * imm : bigint
    | RXori of rS : uint32 * rA : uint32 * imm : bigint
    | RNori of rS : uint32 * rA : uint32 * imm : bigint
    | RSlli of rS : uint32 * rA : uint32 * imm : bigint
    | RSrli of rS : uint32 * rA : uint32 * imm : bigint
    | RCompgei of rS : uint32 * rA : uint32 * imm : bigint
    | RComplti of rS : uint32 * rA : uint32 * imm : bigint
    | RCompgeiu of rS : uint32 * rA : uint32 * imm : bigint
    | RCompltiu of rS : uint32 * rA : uint32 * imm : bigint
    | RCompeqi of rS : uint32 * rA : uint32 * imm : bigint
    | RCompnei of rS : uint32 * rA : uint32 * imm : bigint
    | RBge of rA : uint32 * rB : uint32 * imm : bigint
    | RBlt of rA : uint32 * rB : uint32 * imm : bigint
    | RBgeu of rA : uint32 * rB : uint32 * imm : bigint
    | RBltu of rA : uint32 * rB : uint32 * imm : bigint
    | RBeq of rA : uint32 * rB : uint32 * imm : bigint
    | RBne of rA : uint32 * rB : uint32 * imm : bigint
    | RCalli of imm : bigint
    | RCallr of rA : uint32
    | RImmWord of value : bigint

let private reg (reg : Reg) : Attempt<uint32> = 
    match reg with
    | Zero -> Ok 0u
    | RA -> Ok 1u
    | EA -> Ok 2u
    | SP -> Ok 3u
    | TReg t when t < 5us -> Ok(uint32 t + 6u)
    | SReg s when s < 5us -> Ok(uint32 s + 11u)
    | _ -> Fail [ sprintf "Invalid reg %A" reg ]

let private invReg (reg : uint32) : Attempt<Reg> = 
    match reg with
    | 0u -> Ok Zero
    | 1u -> Ok RA
    | 2u -> Ok EA
    | 3u -> Ok SP
    | r when 6u <= r && r <= 10u -> Ok(TReg(uint16 r - 6us))
    | r when 11u <= r && r <= 15u -> Ok(SReg(uint16 r - 11us))
    | _ -> Fail [ sprintf "Invalid reg number %A" reg ]

[<CompiledName("Polyfill")>]
let public polyfill (instr : Instr) : Attempt<List<Instr>> = 
    let next f inp = 
        inp
        |> Attempt.map (List.map f)
        |> Attempt.bind Attempt.liftList
    
    let kelosPolyfill instr = 
        match instr with
        | Set(rS, i) -> Ok(Addi(rS, Zero, i))
        | _ -> Ok instr
    
    instr
    |> Instr.macroPolyfill
    |> next kelosPolyfill

[<CompiledName("LabelResolver")>]
let public labelResolver (absoluteLabel : Imm -> Attempt<bigint>) (relativeLabel : bigint -> Imm -> Attempt<bigint>) (address : bigint) (instr : Instr) : Attempt<ResolvedInstr> = 
    let inline _Reg instr a = reg a |> Attempt.map instr
    let inline _2Reg instr a b = (reg a, reg b) |> Attempt.lift2tupleMap instr
    let inline _3Reg instr a b c = (reg a, reg b, reg c) |> Attempt.lift3tupleMap instr
    let inline _Imm instr i = i |> Attempt.map instr
    let inline _RegImm instr a i = (reg a, i) |> Attempt.lift2tupleMap instr
    let inline _2RegImm instr a b i = (reg a, reg b, i) |> Attempt.lift3tupleMap instr
    match instr with
    | Trap -> Ok RTrap
    | Switch i -> absoluteLabel i |> _Imm RSwitch
    | Ldw(rS, rA, i) -> absoluteLabel i |> _2RegImm RLdw rS rA
    | Stw(rB, rA, i) -> absoluteLabel i |> _2RegImm RStw rB rA
    | Add(rS, rA, rB) -> _3Reg RAdd rS rA rB
    | Sub(rS, rA, rB) -> _3Reg RSub rS rA rB
    | And(rS, rA, rB) -> _3Reg RAnd rS rA rB
    | Or(rS, rA, rB) -> _3Reg ROr rS rA rB
    | Xor(rS, rA, rB) -> _3Reg RXor rS rA rB
    | Nor(rS, rA, rB) -> _3Reg RNor rS rA rB
    | Sll(rS, rA, rB) -> _3Reg RSll rS rA rB
    | Srl(rS, rA, rB) -> _3Reg RSrl rS rA rB
    | Compge(rS, rA, rB) -> _3Reg RCompge rS rA rB
    | Complt(rS, rA, rB) -> _3Reg RComplt rS rA rB
    | Compgeu(rS, rA, rB) -> _3Reg RCompgeu rS rA rB
    | Compltu(rS, rA, rB) -> _3Reg RCompltu rS rA rB
    | Compeq(rS, rA, rB) -> _3Reg RCompeq rS rA rB
    | Compne(rS, rA, rB) -> _3Reg RCompne rS rA rB
    | Addi(rS, rA, i) -> absoluteLabel i |> _2RegImm RAddi rS rA
    | Subi(rS, rA, i) -> absoluteLabel i |> _2RegImm RSubi rS rA
    | Andi(rS, rA, i) -> absoluteLabel i |> _2RegImm RAndi rS rA
    | Ori(rS, rA, i) -> absoluteLabel i |> _2RegImm ROri rS rA
    | Xori(rS, rA, i) -> absoluteLabel i |> _2RegImm RXori rS rA
    | Nori(rS, rA, i) -> absoluteLabel i |> _2RegImm RNori rS rA
    | Slli(rS, rA, i) -> absoluteLabel i |> _2RegImm RSlli rS rA
    | Srli(rS, rA, i) -> absoluteLabel i |> _2RegImm RSrli rS rA
    | Compgei(rS, rA, i) -> absoluteLabel i |> _2RegImm RCompgei rS rA
    | Complti(rS, rA, i) -> absoluteLabel i |> _2RegImm RComplti rS rA
    | Compgeiu(rS, rA, i) -> absoluteLabel i |> _2RegImm RCompgeiu rS rA
    | Compltiu(rS, rA, i) -> absoluteLabel i |> _2RegImm RCompltiu rS rA
    | Compeqi(rS, rA, i) -> absoluteLabel i |> _2RegImm RCompeqi rS rA
    | Compnei(rS, rA, i) -> absoluteLabel i |> _2RegImm RCompnei rS rA
    | Bge(rA, rB, i) -> relativeLabel address i |> _2RegImm RBge rA rB
    | Blt(rA, rB, i) -> relativeLabel address i |> _2RegImm RBlt rA rB
    | Bgeu(rA, rB, i) -> relativeLabel address i |> _2RegImm RBgeu rA rB
    | Bltu(rA, rB, i) -> relativeLabel address i |> _2RegImm RBltu rA rB
    | Beq(rA, rB, i) -> relativeLabel address i |> _2RegImm RBeq rA rB
    | Bne(rA, rB, i) -> relativeLabel address i |> _2RegImm RBne rA rB
    | Calli i -> relativeLabel address i |> _Imm RCalli
    | Callr rA -> _Reg RCallr rA
    | ImmWord i -> absoluteLabel i |> _Imm RImmWord
    | _ -> Fail [ sprintf "Invalid state. Instr %A is not supported by Kelos16 and should be polyfilled" instr ]

[<CompiledName("ToBin")>]
let rec public toBin (instr : ResolvedInstr) : Attempt<uint32> = 
    let imm16 i =
        let word =
            if i >= 0I then uint32 i
            else ~~~(uint32 (i * -1I)) + 1u
        word &&& 0xFFFFu

    let build op signExtend rA rB rS i =
       let instr =  (op <<< 29) ||| (signExtend <<< 28) ||| (rA <<< 20) ||| (rB <<< 24) ||| (rS <<< 16) ||| (imm16 i)
       instr |> Ok

    match instr with
    | RTrap -> Ok(0b1100u <<< 28)
    | RSwitch i when Imm.fitsInXBitsUnsigned 16 i -> Ok((0b1110u <<< 28) ||| (uint32 i))
    | RLdw(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b000u 0u rA 0u rS i
    | RLdw _ -> Fail [ sprintf "Ldw should have a 16 bits immediate value %A" instr ]
    | RStw(rB, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b001u 0u rA rB 0u i
    | RStw _ -> Fail [ sprintf "Stw should have a 16 bits immediate value %A" instr ]
    | RAdd(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0000u <<< 12))
    | RSub(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0001u <<< 12))
    | RAnd(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0010u <<< 12))
    | ROr(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0011u <<< 12))
    | RXor(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0100u <<< 12))
    | RNor(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0101u <<< 12))
    | RSll(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0110u <<< 12))
    | RSrl(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b0111u <<< 12))
    | RCompge(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1000u <<< 12))
    | RComplt(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1001u <<< 12))
    | RCompgeu(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1010u <<< 12))
    | RCompltu(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1011u <<< 12))
    | RCompeq(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1100u <<< 12))
    | RCompne(rS, rA, rB) -> build 0b010u 0u rA rB rS (bigint (0b1101u <<< 12))
    | RAddi(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0000u rS i
    | RAddi(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0000u rS i
    | RSubi(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0001u rS i
    | RSubi(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0001u rS i
    | RAndi(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0010u rS i
    | RAndi(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0010u rS i
    | ROri(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0011u rS i
    | ROri(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0011u rS i
    | RXori(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0100u rS i
    | RXori(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0100u rS i
    | RNori(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0101u rS i
    | RNori(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0101u rS i
    | RSlli(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0110u rS i
    | RSlli(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0110u rS i
    | RSrli(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b0111u rS i
    | RSrli(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b0111u rS i
    | RCompgei(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b1000u rS i
    | RComplti(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b1001u rS i
    | RCompgeiu(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b1010u rS i
    | RCompltiu(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b1011u rS i
    | RCompeqi(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b1100u rS i
    | RCompeqi(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b1100u rS i
    | RCompnei(rS, rA, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b011u 0u rA 0b1101u rS i
    | RCompnei(rS, rA, i) when Imm.fitsInXBitsSigned 16 i -> build 0b011u 1u rA 0b1101u rS i
    | RBge(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1000u i
    | RBge(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1000u i
    | RBlt(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1001u i
    | RBlt(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1001u i
    | RBgeu(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1010u i
    | RBgeu(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1010u i
    | RBltu(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1011u i
    | RBltu(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1011u i
    | RBeq(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1100u i
    | RBeq(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1100u i
    | RBne(rA, rB, i) when Imm.fitsInXBitsUnsigned 16 i -> build 0b100u 0u rA rB 0b1101u i
    | RBne(rA, rB, i) when Imm.fitsInXBitsSigned 16 i -> build 0b100u 1u rA rB 0b1101u i
    | RCalli i when Imm.fitsInXBitsSigned 24 i ->
        let asUint32 = if i < 0I then ~~~(uint32 (i * -1I)) + 1u else uint32 i
        let firstPart = asUint32 &&& 0xFF0000u
        let secondPart = asUint32 &&& 0xFFFFu
        Ok ((0b1011u <<< 28) ||| (firstPart <<< 4) ||| (0b0001u <<< 16) ||| secondPart)
    | RCallr rA -> build 0b101u 0u rA 0u 0b0001u 0I
    | RImmWord i when Imm.fitsInXBitsUnsigned 32 i -> uint32 i |> Ok
    | RImmWord i when Imm.fitsInXBitsSigned 32 i -> ~~~(uint32 (i * -1I)) + 1u |> Ok
    | _ -> Fail [ sprintf "ToBin Not implemented %A" instr ]

[<CompiledName("FromBin")>]
let public fromBin (instr : uint32) : Instr = 
    let immWord = bigint instr |> Value |> ImmWord
    let mask start vEnd value = 
        let leftShift = 31 - start
        let rightShift = vEnd + leftShift
        (value <<< leftShift) >>> rightShift
    
    let opcode = instr |> mask 31 29
    let singExtend = (instr |> mask 28 28) = 1u
    let rB = instr |> mask 27 24
    let rA = instr |> mask 23 20
    let rS = instr |> mask 19 16
    let i = instr |> mask 15 0
    
    let imm16 (i : uint32) =  
        if singExtend && (i |> mask 15 15 = 1u) then 
            let signed = ~~~(0xFFFF0000u ||| i) + 1u
            (bigint signed) * -1I |> Value |> Ok
        else bigint i |> Value |> Ok
    let imm24 (i : uint32) =  
        if singExtend && (i |> mask 23 23 = 1u) then 
            let signed = ~~~(0xFF000000u ||| i) + 1u
            (bigint signed) * -1I |> Value |> Ok
        else bigint i |> Value |> Ok
    
    let calliImm = ((instr |> mask 27 20) <<< 16) ||| i
    let inline _Reg instr a = invReg a |> Attempt.map instr |> Attempt.orElse immWord
    let inline _2Reg instr a b = (invReg a, invReg b) |> Attempt.lift2tupleMap instr |> Attempt.orElse immWord
    let inline _3Reg instr a b c = (invReg a, invReg b, invReg c) |> Attempt.lift3tupleMap instr |> Attempt.orElse immWord
    let inline _Imm instr i = imm16 i |> Attempt.map instr |> Attempt.orElse immWord
    let inline _RegImm instr a i = (invReg a, imm16 i) |> Attempt.lift2tupleMap instr |> Attempt.orElse immWord
    let inline _2RegImm instr a b i = (invReg a, invReg b, imm16 i) |> Attempt.lift3tupleMap instr |> Attempt.orElse immWord
    match opcode with
    | 0b000u -> _2RegImm Ldw rS rA i
    | 0b001u -> _2RegImm Stw rB rA i
    | 0b010u -> 
        let aluOp = instr |> mask 15 12
        match aluOp with
        | 0b0000u -> _3Reg Add rS rA rB
        | 0b0001u -> _3Reg Sub rS rA rB
        | 0b0010u -> _3Reg And rS rA rB
        | 0b0011u -> _3Reg Or rS rA rB
        | 0b0100u -> _3Reg Xor rS rA rB
        | 0b0101u -> _3Reg Nor rS rA rB
        | 0b0110u -> _3Reg Sll rS rA rB
        | 0b0111u -> _3Reg Srl rS rA rB
        | 0b1000u -> _3Reg Compge rS rA rB
        | 0b1001u -> _3Reg Complt rS rA rB
        | 0b1010u -> _3Reg Compgeu rS rA rB
        | 0b1011u -> _3Reg Compltu rS rA rB
        | 0b1100u -> _3Reg Compeq rS rA rB
        | 0b1101u -> _3Reg Compne rS rA rB
        | 0b1110u -> immWord
        | 0b1111u -> immWord
        | _ -> failwith "Invalid state"
    | 0b011u -> 
        let aluOp = rB
        match aluOp with
        | 0b0000u -> _2RegImm Addi rS rA i
        | 0b0001u -> _2RegImm Subi rS rA i
        | 0b0010u -> _2RegImm Andi rS rA i
        | 0b0011u -> _2RegImm Ori rS rA i
        | 0b0100u -> _2RegImm Xori rS rA i
        | 0b0101u -> _2RegImm Nori rS rA i
        | 0b0110u -> _2RegImm Slli rS rA i
        | 0b0111u -> _2RegImm Srli rS rA i
        | 0b1000u -> _2RegImm Compgei rS rA i
        | 0b1001u -> _2RegImm Complti rS rA i
        | 0b1010u -> _2RegImm Compgeiu rS rA i
        | 0b1011u -> _2RegImm Compltiu rS rA i
        | 0b1100u -> _2RegImm Compeqi rS rA i
        | 0b1101u -> _2RegImm Compnei rS rA i
        | 0b1110u -> immWord
        | 0b1111u -> immWord
        | _ -> failwith "Invalid state"
    | 0b100u -> 
        let aluOp = rS
        match aluOp with
        | 0b0000u -> immWord
        | 0b0001u -> immWord
        | 0b0010u -> immWord
        | 0b0011u -> immWord
        | 0b0100u -> immWord
        | 0b0101u -> immWord
        | 0b0110u -> immWord
        | 0b0111u -> immWord
        | 0b1000u -> _2RegImm Bge rA rB i
        | 0b1001u -> _2RegImm Blt rA rB i
        | 0b1010u -> _2RegImm Bgeu rA rB i
        | 0b1011u -> _2RegImm Bltu rA rB i
        | 0b1100u -> _2RegImm Beq rA rB i
        | 0b1101u -> _2RegImm Bne rA rB i
        | 0b1110u -> immWord
        | 0b1111u -> immWord
        | _ -> failwith "Invalid state"
    | 0b101u -> 
        if rS = 0b0001u then
            if singExtend then imm24 calliImm |> Attempt.map Calli |> Attempt.orElse immWord
            else _Reg Callr rA
        else immWord
    | 0b110u -> Trap
    | 0b111u -> _Imm Switch i
    | _ -> failwith "Invalid state"

// --------------------------------------------------------------------------------------------------------------------
// <copyright file="InstrTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.InstrTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let inline public ob (s : string) : uint32 = s.Replace("|", "").Replace(" ", "") |> (fun x -> System.Convert.ToUInt32(x, 2))

let regularInstructions = 
    [ Id "nop" :: [], Nop
      Id "trap" :: [], Trap
      Id "switch" :: IntLit 4I :: [], Switch(Value 4I)
      Id "ldw" :: Id "t10" :: Id "s6" :: LeftParen :: IntLit 10I :: RightParen :: [], Ldw(TReg 10us, SReg 6us, Value 10I)
      Id "ldw" :: Id "t10" :: Id "s6" :: Id "RAM" :: Operator "+" :: IntLit 1I :: [], Ldw(TReg 10us, SReg 6us, LabelRef("RAM", 1I))
      Id "stw" :: Id "t10" :: Id "s6" :: Id "RAM" :: Operator "-" :: IntLit 2I :: [], Stw(TReg 10us, SReg 6us, LabelRef("RAM", -2I))
      Id "stw" :: Id "t10" :: Id "s6" :: UIntLit 10I :: [], Stw(TReg 10us, SReg 6us, Value 10I)
      Id "set" :: Id "t10" :: UIntLit 10I :: [], Set(TReg 10us, Value 10I)
      Id "not" :: Id "s6" :: Id "t10" :: [], Not(SReg 6us, TReg 10us)
      Id "br" :: Id "end" :: [], Br(LabelRef("end", 0I))
      Id "calli" :: Id "end" :: [], Calli(LabelRef("end", 0I))
      Id "callr" :: Id "s4" :: [], Callr(SReg 4us)
      Id "ret" :: [], Ret
      Dot :: Id "word" :: UIntLit 4242I :: [], ImmWord(Value 4242I)
      Dot :: Def :: Id "oswald" :: UIntLit 4242I :: [], Define("oswald", 4242I)
      Id "OzieGamma" :: Colon :: [], Label "OzieGamma"
      Id "Int_add" :: Colon :: [], Label "Int_add"
      Dot :: Id "text" :: StringLit("string text") :: [], Text("string text") ]

let rOpInstructions = 
    [ "add", Add
      "sub", Sub
      "and", And
      "or", Or
      "xor", Xor
      "nor", Nor
      "sll", Sll
      "srl", Srl
      "compge", Compge
      "complt", Complt
      "compgeu", Compgeu
      "compltu", Compltu
      "compeq", Compeq
      "compne", Compne ]
    |> List.collect (fun (str, op) -> [ Id str :: Id "s6" :: Id "t10" :: Id "zero" :: [], op (SReg 6us, TReg 10us, Zero) ])

let iOpInstructions = 
    [ "addi", Addi
      "subi", Subi
      "andi", Andi
      "ori", Ori
      "xori", Xori
      "nori", Nori
      "slli", Slli
      "srli", Srli
      "compgei", Compgei
      "complti", Complti
      "compgeiu", Compgeiu
      "compltiu", Compltiu
      "compeqi", Compeqi
      "compnei", Compnei ]
    |> List.collect (fun (str, op) -> [ Id str :: Id "s6" :: Id "t10" :: IntLit 42I :: [], op (SReg 6us, TReg 10us, Value 42I) ])

let brInstructions = 
    [ "bge", Bge
      "blt", Blt
      "bgeu", Bgeu
      "bltu", Bltu
      "beq", Beq
      "bne", Bne ]
    |> List.collect (fun (str, op) -> [ Id str :: Id "s6" :: Id "s6" :: Id "end" :: [], op (SReg 6us, SReg 6us, LabelRef("end", 0I)) ])

let friendlyData = List.concat [ regularInstructions; rOpInstructions; iOpInstructions; brInstructions ]
let validInstructions = friendlyData |> List.map (fun (_, x) -> x)

let invalidInstructions = 
    [ Id "RAM" :: Operator "+" :: IntLit 1I :: []
      Dot :: Def :: Id "RAM" :: Operator "+" :: UIntLit 5353I :: []
      Id "ldw" :: Id "t10" :: Id "s4" :: Id "R" :: Operator "-" :: Id "AM" :: []
      Id "OZ" :: Colon :: Id "GE" :: [] ]

[<Test>]
let ``Instr.fromTokens should parse correct input``() = friendlyData |> testOnDataMapAttempt Instr.fromTokens

[<Test>]
let ``Instr.fromTokens should reject invalid input``() = invalidInstructions |> testOnDataShouldFail Instr.fromTokens

[<Test>]
let ``Instr.tokens should loop correctly``() = validInstructions |> testOnDataLoop Instr.toTokens Instr.fromTokens

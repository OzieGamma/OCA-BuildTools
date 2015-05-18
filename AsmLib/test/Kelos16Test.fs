// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Kelos16Test.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.Kelos16Test

open NUnit.Framework
open OCA.AsmLib
open OFuncLib
open OCA.AsmLib.Kelos16
open Unchecked

let inline public ob (s : string) : uint32 = s.Replace("|", "").Replace(" ", "").Replace("-", "0") |> (fun x -> System.Convert.ToUInt32(x, 2))

[<Test>]
let ``Kelos16.polyfill should handle not supported instructions``() = 
    [ Not(SP, EA), Nor(SP, EA, EA) :: []
      Br(Imm.Value 42I), Beq(Zero, Zero, Imm.Value 42I) :: []
      Ret, Callr RA :: []
      Set(SP, Value 42I), Addi(SP, Zero, Value 42I) :: [] ]
    |> testOnDataMapAttempt Kelos16.polyfill

[<Test>]
let ``Kelos16.labelResolver resolves labels as expected``() = 
    let absolute i = 
        match i with
        | Value v -> Ok v
        | LabelRef(_, offset) -> Ok(42I + offset)
    
    let relative _ i = 
        match i with
        | Value v -> Ok v
        | LabelRef(_, offset) -> Ok(7I + offset)
    
    [ Switch(Value 3I), RSwitch 3I
      Switch(LabelRef("end", 0I)), RSwitch 42I
      Ldw(SP, EA, LabelRef("", 1I)), RLdw(3u, 2u, 43I)
      Stw(SP, EA, LabelRef("", -1I)), RStw(3u, 2u, 41I)
      Ldw(SP, EA, LabelRef("", 1I)), RLdw(3u, 2u, 43I)
      Addi(SP, EA, LabelRef("", 1I)), RAddi(3u, 2u, 43I)
      Subi(SP, EA, LabelRef("", 1I)), RSubi(3u, 2u, 43I)
      Andi(SP, EA, LabelRef("", 1I)), RAndi(3u, 2u, 43I)
      Ori(SP, EA, LabelRef("", 1I)), ROri(3u, 2u, 43I)
      Xori(SP, EA, LabelRef("", 1I)), RXori(3u, 2u, 43I)
      Nori(SP, EA, LabelRef("", 1I)), RNori(3u, 2u, 43I)
      Slli(SP, EA, LabelRef("", 1I)), RSlli(3u, 2u, 43I)
      Srli(SP, EA, LabelRef("", 1I)), RSrli(3u, 2u, 43I)
      Compgei(SP, EA, LabelRef("", 1I)), RCompgei(3u, 2u, 43I)
      Complti(SP, EA, LabelRef("", 1I)), RComplti(3u, 2u, 43I)
      Compgeiu(SP, EA, LabelRef("", 1I)), RCompgeiu(3u, 2u, 43I)
      Compltiu(SP, EA, LabelRef("", 1I)), RCompltiu(3u, 2u, 43I)
      Compeqi(SP, EA, LabelRef("", 1I)), RCompeqi(3u, 2u, 43I)
      Compnei(SP, EA, LabelRef("", 1I)), RCompnei(3u, 2u, 43I)
      Bge(SP, EA, LabelRef("", 1I)), RBge(3u, 2u, 8I)
      Blt(SP, EA, LabelRef("", 0I)), RBlt(3u, 2u, 7I)
      Bgeu(SP, EA, LabelRef("", -1I)), RBgeu(3u, 2u, 6I)
      Bltu(SP, EA, LabelRef("", 1I)), RBltu(3u, 2u, 8I)
      Beq(SP, EA, LabelRef("", 1I)), RBeq(3u, 2u, 8I)
      Bne(SP, EA, LabelRef("", 1I)), RBne(3u, 2u, 8I)
      Calli(LabelRef("", 3I)), RCalli 10I ]
    |> testOnDataMapAttempt (Kelos16.labelResolver absolute relative 0I)

let binData = 
    [ RLdw(7u, 8u, (bigint 0xFFEEu)), ob "0000 ---- 1000 0111 1111 1111 1110 1110"
      RStw(7u, 8u, (bigint 0xAF47u)), ob "0010 0111 1000 ---- 1010 1111 0100 0111"
      RAdd(7u, 8u, 3u), ob "010- 0011 1000 0111 0000 ---- ---- ----"
      RAddi(7u, 8u, (bigint 0xAF47u)), ob "0110 0000 1000 0111 1010 1111 0100 0111"
      RAddi(7u, 8u, -1I), ob "0111 0000 1000 0111 1111 1111 1111 1111"
      RSub(7u, 8u, 3u), ob "010- 0011 1000 0111 0001 ---- ---- ----"
      RSubi(7u, 8u, (bigint 0xAF47u)), ob "0110 0001 1000 0111 1010 1111 0100 0111"
      RSubi(7u, 8u, -1I), ob "0111 0001 1000 0111 1111 1111 1111 1111"
      RAnd(7u, 8u, 3u), ob "010- 0011 1000 0111 0010 ---- ---- ----"
      RAndi(7u, 8u, (bigint 0xAF47u)), ob "0110 0010 1000 0111 1010 1111 0100 0111"
      RAndi(7u, 8u, -1I), ob "0111 0010 1000 0111 1111 1111 1111 1111"
      ROr(7u, 8u, 3u), ob "010- 0011 1000 0111 0011 ---- ---- ----"
      ROri(7u, 8u, (bigint 0xAF47u)), ob "0110 0011 1000 0111 1010 1111 0100 0111"
      ROri(7u, 8u, -1I), ob "0111 0011 1000 0111 1111 1111 1111 1111"
      RXor(7u, 8u, 3u), ob "010- 0011 1000 0111 0100 ---- ---- ----"
      RXori(7u, 8u, (bigint 0xAF47u)), ob "0110 0100 1000 0111 1010 1111 0100 0111"
      RXori(7u, 8u, -1I), ob "0111 0100 1000 0111 1111 1111 1111 1111"
      RNor(7u, 8u, 3u), ob "010- 0011 1000 0111 0101 ---- ---- ----"
      RNori(7u, 8u, (bigint 0xAF47u)), ob "0110 0101 1000 0111 1010 1111 0100 0111"
      RNori(7u, 8u, -1I), ob "0111 0101 1000 0111 1111 1111 1111 1111"
      RSll(7u, 8u, 3u), ob "010- 0011 1000 0111 0110 ---- ---- ----"
      RSlli(7u, 8u, 4I), ob "0110 0110 1000 0111 0000 0000 0000 0100"
      RSrl(7u, 8u, 3u), ob "010- 0011 1000 0111 0111 ---- ---- ----"
      RSrli(7u, 8u, 4I), ob "0110 0111 1000 0111 0000 0000 0000 0100"
      RCompge(7u, 8u, 3u), ob "010- 0011 1000 0111 1000 ---- ---- ----"
      RCompgei(7u, 8u, (bigint 0x0F47u)), ob "0111 1000 1000 0111 0000 1111 0100 0111"
      RCompgei(7u, 8u, -1I), ob "0111 1000 1000 0111 1111 1111 1111 1111"
      RComplt(7u, 8u, 3u), ob "010- 0011 1000 0111 1001 ---- ---- ----"
      RComplti(7u, 8u, (bigint 0x0F47u)), ob "0111 1001 1000 0111 0000 1111 0100 0111"
      RComplti(7u, 8u, -1I), ob "0111 1001 1000 0111 1111 1111 1111 1111"
      RCompgeu(7u, 8u, 3u), ob "010- 0011 1000 0111 1010 ---- ---- ----"
      RCompgeiu(7u, 8u, (bigint 0xFF47u)), ob "0110 1010 1000 0111 1111 1111 0100 0111"
      RCompltu(7u, 8u, 3u), ob "010- 0011 1000 0111 1011 ---- ---- ----"
      RCompltiu(7u, 8u, (bigint 0xFF47u)), ob "0110 1011 1000 0111 1111 1111 0100 0111"
      RCompeq(7u, 8u, 3u), ob "010- 0011 1000 0111 1100 ---- ---- ----"
      RCompeqi(7u, 8u, (bigint 0xAF47u)), ob "0110 1100 1000 0111 1010 1111 0100 0111"
      RCompeqi(7u, 8u, -1I), ob "0111 1100 1000 0111 1111 1111 1111 1111"
      RCompne(7u, 8u, 3u), ob "010- 0011 1000 0111 1101 ---- ---- ----"
      RCompnei(7u, 8u, (bigint 0xAF47u)), ob "0110 1101 1000 0111 1010 1111 0100 0111"
      RCompnei(7u, 8u, -1I), ob "0111 1101 1000 0111 1111 1111 1111 1111"
      RBge(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1000 1010 1111 0100 0111"
      RBlt(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1001 1010 1111 0100 0111"
      RBgeu(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1010 1010 1111 0100 0111"
      RBltu(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1011 1010 1111 0100 0111"
      RBeq(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1100 1010 1111 0100 0111"
      RBne(8u, 3u, (bigint 0xAF47u)), ob "1000 0011 1000 1101 1010 1111 0100 0111"
      RCallr(7u), ob "1010 ---- 0111 0001 ---- ---- ---- ----"
      RCalli(-1I), ob "1011 1111 1111 0001 1111 1111 1111 1111"
      RCalli((bigint 0x7FAF47u)), ob "1011 0111 1111 0001 1010 1111 0100 0111"
      RTrap, ob "110- ---- ---- ---- ---- ---- ---- ----"
      RSwitch(bigint 0xAF47u), ob "111- ---- ---- ---- 1010 1111 0100 0111" ]

let invalidBinData = 
    [ RLdw(0u, 0u, -1I)
      RStw(0u, 0u, -1I)
      RCalli((bigint 0xFFAF47u)) ]

let ResolevInstructions = binData |> List.map (fun (x, _) -> x)

[<Test>]
let ``Kelos16.toBin should correctly map to machine code``() = binData |> testOnDataMapAttempt Kelos16.toBin

[<Test>]
let ``Kelos16.toBin should reject invalid immediate values``() = invalidBinData |> testOnDataShouldFail Kelos16.toBin

[<Test>]
let ``Kelos16.toBin should loop correctly``() = 
    let absolute i = 
        match i with
        | Value v -> Ok v
        | LabelRef _ -> Fail [ "Kelos16.toBin test, should not get there" ]
    
    let relative _ i = 
        match i with
        | Value v -> Ok v
        | LabelRef _ -> Fail [ "Kelos16.toBin test, should not get there" ]
    
    let from bin = 
        bin
        |> Kelos16.fromBin
        |> Kelos16.labelResolver absolute relative 0I
    
    ResolevInstructions |> testOnDataLoop Kelos16.toBin from

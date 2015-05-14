// --------------------------------------------------------------------------------------------------------------------
// <copyright file="TransformTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.TransformTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let tokenData = 
    let tokens1 = 
        [ Id "add"
          Id "s10"
          Id "s10"
          Id "s10"
          Id "stw"
          Id "s10"
          Id "t5"
          UIntLit 10I ], 
        [ Add(SReg 10us, SReg 10us, SReg 10us)
          Stw(SReg 10us, TReg 5us, Value 10I) ]
    
    let tokens2 = 
        [ Id "start"
          Colon
          Id "add"
          Id "s10"
          Id "s10"
          Id "s10"
          Id "ozg"
          Colon
          Id "stw"
          Id "s10"
          Id "t5"
          UIntLit 10I ], 
        [ Label "start"
          Add(SReg 10us, SReg 10us, SReg 10us)
          Label "ozg"
          Stw(SReg 10us, TReg 5us, Value 10I) ]
    
    let tokens3 = 
        [ Id "stw"
          Id "s3"
          Comma
          Id "zero"
          LeftParen
          Id "RAM"
          Operator "+"
          IntLit 1I
          RightParen
          Id "stw"
          Comma
          Id "zero"
          Comma
          Id "ra"
          Comma
          Id "RAM"
          Operator "-"
          IntLit 1I
          Id "RAM"
          Colon ], 
        [ Stw(SReg 3us, Zero, LabelRef("RAM", 1I))
          Stw(Zero, RA, LabelRef("RAM", -1I))
          Label "RAM" ]
    
    let tokens4 = 
        [ Id "START"
          Colon
          Dot
          Def
          Id "word"
          IntLit 42I
          Dot
          Id "word"
          Id "word"
          Operator "+"
          IntLit 42I ], 
        [ Label("START")
          Define("word", 42I)
          ImmWord(LabelRef("word", 42I)) ]
    
    [ tokens1; tokens2; tokens3; tokens4 ]

let tokenSource = tokenData |> List.map (fun (_, x) -> x)
let invalidToken = [ [ Id "add" ] ]

let f from x = 
    x
    |> List.map Position.addZero
    |> from
    |> Attempt.map (List.map Position.remove)

[<Test>]
let ``Transform.fromTokensToHla should work on valid friendly``() = tokenData |> testOnDataMapAttempt (f Transform.tokensToInstr)

[<Test>]
let ``Transform.fromTokensToHla should reject invalid friendly``() = invalidToken |> testOnDataShouldFail (f Transform.tokensToInstr)

[<Test>]
let ``Transform.tokens should loop correctly``() = tokenSource |> testOnDataLoop (f Transform.instrToTokens) (f Transform.tokensToInstr)

let binData = 
    let bin1 = 
        [ Addi(SP, SP, Value 42I)
          Andi(EA, SP, Value 64I) ], 
        [ Addi(SP, SP, Value 42I)
          Andi(EA, SP, Value 64I) ]
    
    let bin2 = 
        [ Label "end"
          Br(LabelRef("end", 0I)) ], [ Beq(Zero, Zero, Value -1I) ]
    
    let bin3 = 
        [ Ldw(RA, Zero, LabelRef("RAM", 2I))
          Label "RAM" ], [ Ldw(RA, Zero, Value 3I) ]
    
    let bin4 = 
        [ Ldw(RA, Zero, LabelRef("RAM", -1I))
          Label "RAM" ], [ Ldw(RA, Zero, Value 0I) ]
    
    let bin5 = 
        [ Text "ABCDEFGH" ], 
        [ ImmWord(Value(bigint 0x41424344u))
          ImmWord(Value(bigint 0x45464748u))
          Ldw(Zero, Zero, Value 0I) ]
    
    let bin6 = [ Text "AB" ], [ ImmWord(Value(bigint 0x41420000u)) ]
    
    let bin7 = 
        [ Define("LED_BASE", 4096I)
          Set(TReg(0us), LabelRef("LED_BASE", 1I)) ], [ Addi(TReg(0us), Zero, Value(4097I)) ]
    [ bin1; bin2; bin3; bin4; bin5; bin6; bin7 ]

[<Test>]
let ``Transform.bin should loop correctly``() = 
    let toBinAndBack inp = 
        inp
        |> Transform.instrToBin Kelos16.polyfill Kelos16.labelResolver Kelos16.toBin
        |> Attempt.map (Transform.binToInstr Kelos16.fromBin)
    binData |> testOnDataMapAttempt (f toBinAndBack)

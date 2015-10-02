// --------------------------------------------------------------------------------------------------------------------
// <copyright file="LexerTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.LexerTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

let listify f inp = f inp |> List.ofSeq

(* formatPositionInError *)
[<Test>]
let ``formatPositionInError should correctly format string``() = 
    [ Positioned("error", Position(42u, 1u, "f")), "Error in file f [1:42]: error" ] 
    |> testOnDataMap Lexer.formatPositionInError

(* addPositionInCol *)
[<Test>]
let ``addPositionInCol should correctly add positions in a col``() = 
    [ 's', Positioned('s', Position(35u, 42u, "f"))
      'q', Positioned('q', Position(35u, 42u, "f"))
      't', Positioned('t', Position(35u, 42u, "f")) ]
    |> testOnDataMap (Lexer.positionCol "f" 42u 35u)

(* addPositionInRow *)
let addPositionInRow file row src = 
    src
    |> Lexer.positionRow file row
    |> List.ofSeq

[<Test>]
let ``addPositionInRow should correctly add positions in a row``() = 
    let test1 = 
        "str", 
        [ Positioned('s', Position(1u, 42u, "f"))
          Positioned('t', Position(2u, 42u, "f"))
          Positioned('r', Position(3u, 42u, "f")) ]
    [ test1 ] |> testOnDataMap (addPositionInRow "f" 42u)

(* addPositionInFile *)
let addPositionInFile file src = 
    src
    |> Lexer.positionFile file
    |> List.ofSeq
    |> List.map List.ofSeq

[<Test>]
let ``addPositionInFile should correctly add positions in a file``() = 
    let test1 = 
        "str\nabc", 
        [ [ Positioned('s', Position(1u, 1u, "f"))
            Positioned('t', Position(2u, 1u, "f"))
            Positioned('r', Position(3u, 1u, "f")) ]
          [ Positioned('a', Position(1u, 2u, "f"))
            Positioned('b', Position(2u, 2u, "f"))
            Positioned('c', Position(3u, 2u, "f")) ] ]
    [ test1 ] |> testOnDataMap (addPositionInFile "f")

(* removeCommentsInRow *)
let removeCommentsInRow inp = 
    let charArray = 
        inp
        |> Lexer.positionRow "f" 1u
        |> Lexer.removeCommentsInRow
        |> Seq.map Position.remove
        |> Array.ofSeq
    new System.String(charArray)

[<Test>]
let ``removeCommentsInRow should correctly remove comments in a row``() = 
    [ "str abc #comment", "str abc "
      "abc", "abc" ]
    |> testOnDataMap removeCommentsInRow

(* removeCommentsInFile *)
let removeCommentsInFile inp = 
    inp
    |> Lexer.positionFile "f"
    |> Lexer.removeCommentsInFile
    |> Seq.map (Seq.map Position.remove)
    |> List.ofSeq
    |> List.map Array.ofSeq
    |> List.map (fun x -> new System.String(x))

[<Test>]
let ``removeCommentsInFile should correctly remove comments in a file``() = 
    [ "str abc #comment\nlala #lol\ntrolo", [ "str abc "; "lala "; "trolo" ] ] |> testOnDataMap removeCommentsInFile

(* tokenize *)
let tokenize inp = 
    inp
    |> Lexer.positionRow "f" 1u
    |> Lexer.tokenize

[<Test>]
let ``tokenize should correctly tokenize``() = 
    let test1 = 
        "{}[]():/lala**o+33-+a4+42u", 
        [ Positioned(LeftBrace, Position(1u, 1u, "f"))
          Positioned(RightBrace, Position(2u, 1u, "f"))
          Positioned(LeftBracket, Position(3u, 1u, "f"))
          Positioned(RightBracket, Position(4u, 1u, "f"))
          Positioned(LeftParen, Position(5u, 1u, "f"))
          Positioned(RightParen, Position(6u, 1u, "f"))
          Positioned(Colon, Position(7u, 1u, "f"))
          Positioned(Operator "/", Position(8u, 1u, "f"))
          Positioned(Id "lala", Position(9u, 1u, "f"))
          Positioned(Operator "**", Position(13u, 1u, "f"))
          Positioned(Id "o", Position(15u, 1u, "f"))
          Positioned(Operator "+", Position(16u, 1u, "f"))
          Positioned(IntLit 33I, Position(17u, 1u, "f"))
          Positioned(Operator "-+", Position(19u, 1u, "f"))
          Positioned(Id "a4", Position(21u, 1u, "f"))
          Positioned(Operator "+", Position(23u, 1u, "f"))
          Positioned(UIntLit 42I, Position(24u, 1u, "f")) ]
    
    let test2 = "&", [ Positioned(Operator "&", Position(1u, 1u, "f")) ]
    let test3 = "0xFF", [ Positioned(IntLit 255I, Position(1u, 1u, "f")) ]
    
    let test4 = 
        "$System;Collections", 
        [ Positioned(Dolar, Position(1u, 1u, "f"))
          Positioned(Id "System;Collections", Position(2u, 1u, "f")) ]
    
    let test5 = 
        "%System;Collections", 
        [ Positioned(Percentage, Position(1u, 1u, "f"))
          Positioned(Id "System;Collections", Position(2u, 1u, "f")) ]
    
    let test6 = ";System;Collections", [ Positioned(Id ";System;Collections", Position(1u, 1u, "f")) ]
    
    let test7 = 
        "for i = 0u upto 42u", 
        [ Positioned(For, Position(1u, 1u, "f"))
          Positioned(Id "i", Position(5u, 1u, "f"))
          Positioned(Equals, Position(7u, 1u, "f"))
          Positioned(UIntLit 0I, Position(9u, 1u, "f"))
          Positioned(Upto, Position(12u, 1u, "f"))
          Positioned(UIntLit 42I, Position(17u, 1u, "f")) ]
    
    let test8 = 
        "while (i < 4)", 
        [ Positioned(While, Position(1u, 1u, "f"))
          Positioned(LeftParen, Position(7u, 1u, "f"))
          Positioned(Id "i", Position(8u, 1u, "f"))
          Positioned(Operator "<", Position(10u, 1u, "f"))
          Positioned(IntLit 4I, Position(12u, 1u, "f"))
          Positioned(RightParen, Position(13u, 1u, "f")) ]
    
    let test9 = 
        "while (i <= 4)", 
        [ Positioned(While, Position(1u, 1u, "f"))
          Positioned(LeftParen, Position(7u, 1u, "f"))
          Positioned(Id "i", Position(8u, 1u, "f"))
          Positioned(Operator "<=", Position(10u, 1u, "f"))
          Positioned(IntLit 4I, Position(13u, 1u, "f"))
          Positioned(RightParen, Position(14u, 1u, "f")) ]
    
    let test10 = "'a'", [ Positioned(CharLit('a'), Position(1u, 1u, "f")) ]
    let test11 = "'\\n'", [ Positioned(CharLit('\n'), Position(1u, 1u, "f")) ]
    let test12 = "\"string\"", [ Positioned(StringLit("string"), Position(1u, 1u, "f")) ]
    let test13 = "\"str\\ning\"", [ Positioned(StringLit("str\ning"), Position(1u, 1u, "f")) ]
    let test14 = "\"\\\\n\"", [ Positioned(StringLit("\\n"), Position(1u, 1u, "f")) ]
    let test15 = "->", [ Positioned(RightArrow, Position(1u, 1u, "f")) ]
    let test16 = "true", [ Positioned(BoolLit(true), Position(1u, 1u, "f")) ]
    let test17 = "false", [ Positioned(BoolLit(false), Position(1u, 1u, "f")) ]
    [ test1; test2; test3; test4; test5; test6; test7; test8; test9; test10; test11; test12; test13; test14; test15; 
      test16; test17 ] |> testOnDataMapAttempt tokenize

[<Test>]
let ``tokenize should reject unclosed char and string literals``() = [ "'a"; "\"" ] |> testOnDataShouldFail tokenize

[<Test>]
let ``tokenize should reject invalid char literals``() = [ "'aa'"; "'\\'" ] |> testOnDataShouldFail tokenize

[<Test>]
let ``tokenizeFile should function correctly``() = 
    let test1 = 
        "asm\n{#comment }\n}", 
        [ Positioned(Id "asm", Position(1u, 1u, "f"))
          Positioned(LeftBrace, Position(1u, 2u, "f"))
          Positioned(RightBrace, Position(1u, 3u, "f")) ]
    [ test1 ] |> testOnDataMapAttempt (Lexer.tokenizeFile "f")

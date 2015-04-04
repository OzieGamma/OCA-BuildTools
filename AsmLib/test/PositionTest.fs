// --------------------------------------------------------------------------------------------------------------------
// <copyright file="PositionTest.fs" company="Oswald Maskens">
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
module OCA.AsmLib.Test.PositionedTest

open NUnit.Framework
open OCA.AsmLib
open OFuncLib

(* map *)
[<Test>]
let ``Positioned.map should work on valid input``() = [ Position.addZero 42, Position.addZero 84 ] |> testOnDataMap (Positioned.map (fun x -> x * 2))

(* liftAttempt *)
[<Test>]
let ``Positioned.liftAttempt should work on valid Ok``() = [ Position.addZero (Ok 42), Ok(Position.addZero 42) ] |> testOnDataMap Positioned.liftAttempt

[<Test>]
let ``Positioned.liftAttempt should work on valid Fail``() = 
    [ Position.addZero (Fail [ 42; 53 ]), 
      Fail([ Position.addZero 42
             Position.addZero 53 ]) ]
    |> testOnDataMap Positioned.liftAttempt

(* mapAttempt *)
[<Test>]
let ``Positioned.mapAttempt should work on Ok``() = [ Position.addZero 42, Ok(Position.addZero 42) ] |> testOnDataMap (Positioned.mapAttempt Ok)

[<Test>]
let ``Positioned.mapAttempt should work on Fail``() = 
    [ Position.addZero 42, 
      Fail([ Position.addZero 42
             Position.addZero 53 ]) ]
    |> testOnDataMap (Positioned.mapAttempt (fun _ -> Fail [ 42; 53 ]))

(* liftList *)
let ``Positioned.liftList should work``() =
    let pos = Position(42u, 42u, "f")
    [Positioned(1 :: 2 ::[], pos), Positioned(1, pos) :: Positioned(2, pos) :: []] |> testOnDataMap (Positioned.liftList)

(* position *)

let zero = Position(0u, 0u, "")
let five = Position(5u, 55u, "f")

(* zero *)
[<Test>]
let ``Position.zero should have col 0u, row 0u, file "" ``() = zero |> shouldEq Position.zero

(* remove *)
[<Test>]
let ``Position.remove should work on valid input``() = [ Positioned(42, five), 42 ] |> testOnDataMap Position.remove

(* add *)
[<Test>]
let ``Position.add should work on valid input``() = [ 42, Positioned(42, five) ] |> testOnDataMap (Position.add five)

(* addZero *)
[<Test>]
let ``Position.addZero should work ``() = [ 42, Positioned(42, zero) ] |> testOnDataMap Position.addZero

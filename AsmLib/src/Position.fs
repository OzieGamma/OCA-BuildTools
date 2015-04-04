// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Position.fs" company="Oswald Maskens">
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
namespace OCA.AsmLib

open OFuncLib

type Position = 
    | Position of col : uint32 * row : uint32 * file : string

type Positioned<'T> = 
    | Positioned of v : 'T * pos : Position
    
    member this.value = 
        match this with
        | Positioned(value, _) -> value
    
    member this.position = 
        match this with
        | Positioned(_, position) -> position

type GenericPositionedAttempt<'TOk, 'TFail> = GenericAttempt<Positioned<'TOk>, Positioned<'TFail>>

type PositionedAttempt<'T> = GenericPositionedAttempt<'T, string>

type GenericPositionedListAttempt<'TOk, 'TFail> = GenericAttempt<List<Positioned<'TOk>>, Positioned<'TFail>>

type PositionedListAttempt<'T> = GenericPositionedListAttempt<'T, string>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Position = 
    [<CompiledName("Zero")>]
    let public zero = Position(0u, 0u, "")
    
    [<CompiledName("Remove")>]
    let public remove (positioned : Positioned<'A>) : 'A = positioned.value
    
    [<CompiledName("Add")>]
    let public add (pos : Position) (value : 'A) : Positioned<'A> = Positioned(value, pos)
    
    [<CompiledName("AddZero")>]
    let public addZero (v : 'T) : Positioned<'T> = v |> add zero

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Positioned = 
    [<CompiledName("Map")>]
    let public map (f : 'A -> 'B) (positioned : Positioned<'A>) : Positioned<'B> = 
        match positioned with
        | Positioned(v, pos) -> Positioned(f v, pos)
    
    [<CompiledName("LiftAttempt")>]
    let public liftAttempt (attempt : Positioned<GenericAttempt<'TOk, 'TFail>>) : GenericPositionedAttempt<'TOk, 'TFail> = 
        attempt.value
        |> Attempt.map (Position.add attempt.position)
        |> Attempt.mapFail (Position.add attempt.position)
    
    [<CompiledName("MapAttempt")>]
    let public mapAttempt (f : 'A -> GenericAttempt<'TOk, 'TFail>) (positioned : Positioned<'A>) : GenericPositionedAttempt<'TOk, 'TFail> = 
        positioned
        |> map f
        |> liftAttempt

    [<CompiledName("LiftList")>]
    let public liftList (list : Positioned<List<'T>>) : List<Positioned<'T>> = 
        let pos = list.position
        list.value |> List.map (Position.add pos)
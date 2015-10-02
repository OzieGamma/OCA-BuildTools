// --------------------------------------------------------------------------------------------------------------------
// <copyright file="InstrEmiter.fs" company="Oswald Maskens">
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
module OCA.WaldoCompiler.InstrEmiter

open OCA.AsmLib
open OCA.WaldoCompiler
open OCA.WaldoCompiler.Parser
open OFuncLib

let private compileFunction f = 
    let rec compileFunctionInternals name body acc = 
        match body with
        | [] -> 
            Ok [ Position.addZero Ret ] :: acc
            |> Attempt.liftList
            |> Attempt.map List.rev
            |> Attempt.map (List.concat)
        | MethodCallStatement(Positioned(name, pos), args) :: tail -> 
            let statements = 
                if args.Length <> 0 then Fail [ Positioned("Args not implemented", pos) ]
                else Ok [ Positioned(Calli(LabelRef("func;" + name, 0I)), pos) ]
            compileFunctionInternals name body.Tail (statements :: acc)
    match f with
    | DefFunction(attr, Positioned(name, pos), args, retType, body) -> 
        compileFunctionInternals name body [ Ok [ Positioned(Label("func;" + name), pos) ] ]
    | AsmFunction(attr, Positioned(name, pos), args, retType, body) -> 
        if body.IsEmpty then Fail [ Positioned("Asm function body can't be empty, 'ret' is the bare minimum", pos) ]
        elif (body |> List.rev).Head.value <> Ret then Fail [ Positioned("Asm function has to finish with 'ret'", pos) ]
        else Ok(Positioned(Label("func;" + name), pos) :: body)

let emit (source : Tree) : PositionedListAttempt<Instr> = 
    let main, rest = source |> List.partition (fun decl -> decl.name = "__main")
    if main
       |> List.length
       = 0 then Fail [ Positioned("No __main found", Position.zero) ]
    else // Exactly 1 main function since UniquenessVerifier passed
        let compiled = (main.Head |> compileFunction) :: (rest |> List.map compileFunction)
        compiled
        |> Attempt.liftList
        |> Attempt.map (List.concat)

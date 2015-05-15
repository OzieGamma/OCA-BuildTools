// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Program.fs" company="Oswald Maskens">
//   Copyright 2015 Oswald Maskens
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
module OCA.VM.Program

open OCA.AsmLib
open OFuncLib
open System
open System.Net
open System.Text
open Unchecked

let host = "http://localhost:5679/"

let runVM (instrs : List<Positioned<uint32>>) : unit = 
    let state = new State(instrs |> Array.ofList)
    
    let uintImm i = 
        match i with
        | Value v -> 
            if v < 0I then uint32 (~~~(int64 (v * -1I)) + 1L)
            else uint32 v
        | LabelRef _ -> failwith "Labels not supported yet."
    
    let intImm i = 
        match i with
        | Value v -> 
            if v > bigint Int32.MaxValue then int (~~~(int64 (v * -1I)) + 1L)
            else int v
        | LabelRef _ -> failwith "Labels not supported yet."
    
    let runInstr() = 
        let instr = state.readMemory state.pc |> Kelos16.fromBin
        // Increment PC now, like the hardware does.
        state.pc <- state.pc + 1u
        match instr with
        | Trap -> failwith "Trap not yet supported"
        | Switch _ -> failwith "Trap not yet supported"
        | Ldw(rS, rA, imm) -> 
            let data = (state.readReg rA) + uintImm imm |> state.readMemory
            data |> state.writeReg rS
        | Stw(rB, rA, imm) -> 
            let data = state.readReg rB
            let addr = (state.readReg rA) + uintImm imm
            data |> state.writeMemory addr
        | Add(rS, rA, rB) -> (state.readReg rA) + (state.readReg rB) |> state.writeReg rS
        | Sub(rS, rA, rB) -> (state.readReg rA) - (state.readReg rB) |> state.writeReg rS
        | And(rS, rA, rB) -> (state.readReg rA) &&& (state.readReg rB) |> state.writeReg rS
        | Or(rS, rA, rB) -> (state.readReg rA) ||| (state.readReg rB) |> state.writeReg rS
        | Xor(rS, rA, rB) -> (state.readReg rA) ^^^ (state.readReg rB) |> state.writeReg rS
        | Nor(rS, rA, rB) -> ~~~((state.readReg rA) ||| (state.readReg rB)) |> state.writeReg rS
        | Sll(rS, rA, rB) -> (state.readReg rA) <<< int (state.readReg rB) |> state.writeReg rS
        | Srl(rS, rA, rB) -> (state.readReg rA) >>> int (state.readReg rB) |> state.writeReg rS
        | Compge(rS, rA, rB) -> 
            (if (int (state.readReg rA)) >= (int (state.readReg rB)) then 1u
             else 0u)
            |> state.writeReg rS
        | Complt(rS, rA, rB) -> 
            (if (int (state.readReg rA)) < (int (state.readReg rB)) then 1u
             else 0u)
            |> state.writeReg rS
        | Compgeu(rS, rA, rB) -> 
            (if (state.readReg rA) >= (state.readReg rB) then 1u
             else 0u)
            |> state.writeReg rS
        | Compltu(rS, rA, rB) -> 
            (if (state.readReg rA) < (state.readReg rB) then 1u
             else 0u)
            |> state.writeReg rS
        | Compeq(rS, rA, rB) -> 
            (if (state.readReg rA) = (state.readReg rB) then 1u
             else 0u)
            |> state.writeReg rS
        | Compne(rS, rA, rB) -> 
            (if (state.readReg rA) <> (state.readReg rB) then 1u
             else 0u)
            |> state.writeReg rS
        | Addi(rS, rA, imm) -> (state.readReg rA) + uintImm imm |> state.writeReg rS
        | Subi(rS, rA, imm) -> (state.readReg rA) - uintImm imm |> state.writeReg rS
        | Andi(rS, rA, imm) -> (state.readReg rA) &&& uintImm imm |> state.writeReg rS
        | Ori(rS, rA, imm) -> (state.readReg rA) ||| uintImm imm |> state.writeReg rS
        | Xori(rS, rA, imm) -> (state.readReg rA) ^^^ uintImm imm |> state.writeReg rS
        | Nori(rS, rA, imm) -> ~~~((state.readReg rA) ||| uintImm imm) |> state.writeReg rS
        | Slli(rS, rA, imm) -> (state.readReg rA) <<< intImm imm |> state.writeReg rS
        | Srli(rS, rA, imm) -> (state.readReg rA) >>> intImm imm |> state.writeReg rS
        | Compgei(rS, rA, imm) -> 
            (if (int (state.readReg rA)) >= (intImm imm) then 1u
             else 0u)
            |> state.writeReg rS
        | Complti(rS, rA, imm) -> 
            (if (int (state.readReg rA)) < (intImm imm) then 1u
             else 0u)
            |> state.writeReg rS
        | Compgeiu(rS, rA, imm) -> 
            (if (state.readReg rA) >= uintImm imm then 1u
             else 0u)
            |> state.writeReg rS
        | Compltiu(rS, rA, imm) -> 
            (if (state.readReg rA) < uintImm imm then 1u
             else 0u)
            |> state.writeReg rS
        | Compeqi(rS, rA, imm) -> 
            (if (state.readReg rA) = uintImm imm then 1u
             else 0u)
            |> state.writeReg rS
        | Compnei(rS, rA, imm) -> 
            (if (state.readReg rA) <> uintImm imm then 1u
             else 0u)
            |> state.writeReg rS
        | Bge(rA, rB, imm) -> 
            if (int (state.readReg rA)) >= (int (state.readReg rB)) then state.pc <- state.pc + (uintImm imm)
        | Blt(rA, rB, imm) -> 
            if (int (state.readReg rA)) < (int (state.readReg rB)) then state.pc <- state.pc + (uintImm imm)
        | Bgeu(rA, rB, imm) -> 
            if (state.readReg rA) >= (state.readReg rB) then state.pc <- state.pc + (uintImm imm)
        | Bltu(rA, rB, imm) -> 
            if (state.readReg rA) < (state.readReg rB) then state.pc <- state.pc + (uintImm imm)
        | Beq(rA, rB, imm) -> 
            if (state.readReg rA) = (state.readReg rB) then state.pc <- state.pc + (uintImm imm)
        | Bne(rA, rB, imm) -> 
            if (state.readReg rA) <> (state.readReg rB) then state.pc <- state.pc + (uintImm imm)
        | Calli imm -> 
            state.writeReg RA state.pc
            state.pc <- state.pc + (uintImm imm)
        | Callr rA -> 
            state.writeReg RA state.pc
            state.pc <- state.readReg rA
        | ImmWord _ -> failwith "Not recognized instruction. Might have hit data region"
        | _ -> failwith "instr not implemented %A" instr
    
    let running = ref true
    
    let listener (handler : HttpListenerRequest -> HttpListenerResponse -> Async<unit>) = 
        let listener = new HttpListener()
        listener.Prefixes.Add host
        listener.Start()
        let task = Async.FromBeginEnd(listener.BeginGetContext, listener.EndGetContext)
        async { 
            while true do
                let! context = task
                Async.Start(handler context.Request context.Response)
        }
        |> Async.Start
    
    let output (req : HttpListenerRequest) = 
        let url = Uri(host).MakeRelativeUri(req.Url).OriginalString
        printfn "Requested : '%s'" url
        match url.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray with
        | [] -> ("text/html", IO.File.ReadAllText "src\UI.html")
        | "next" :: [] -> 
            runInstr()
            ("text/json", state.jsonify)
        | "run" :: num :: [] -> 
            let watch = System.Diagnostics.Stopwatch.StartNew()
            let runs = UInt32.Parse num
            let mutable i = 1u
            runInstr() // Avoid getting stuck on breakpoint
            while i < runs && not (state.isBreakpoint ()) do
                runInstr()
                i <- i + 1u
            watch.Stop ()
            printfn "Actually run: %d in %dms" i watch.ElapsedMilliseconds
            ("text/json", state.jsonify)
        | "br" :: num :: [] -> 
            let point = UInt32.Parse num
            state.toggleBreakpoint point
            ("text/json", state.jsonify)
        | "state" :: [] -> ("text/json", state.jsonify)
        | "stop" :: [] -> 
            running := false
            ("text/html", "Stopped.")
        | _ -> ("text/html", "Invalid request")
    
    listener (fun req resp -> 
        async { 
            let (media, txt) = output req
            let bytes = Encoding.ASCII.GetBytes txt
            resp.ContentType <- media
            resp.OutputStream.Write(bytes, 0, txt.Length)
            resp.OutputStream.Close()
        })
    while !running do
        ()

let launchIE url = 
    let proc = new System.Diagnostics.Process()
    proc.EnableRaisingEvents <- false
    proc.StartInfo.FileName <- url
    proc.Start() |> ignore

[<EntryPoint>]
let main argv = 
    if argv.Length <> 2 then printfn "Invalid number of args %i" argv.Length
    else 
        launchIE host
        let instr = 
            match argv.[0] with
            | "-f" -> FileHelper.readAsmFile argv.[1] |> Attempt.bind (Transform.instrToBin Kelos16.polyfill Kelos16.labelResolver Kelos16.toBin)
            | "-b" -> FileHelper.readUint32File argv.[1]
            | _ -> Fail [ sprintf "Invalid args %A" argv |> Position.addZero ]
        
        let errors = instr |> Attempt.map runVM
        match errors |> Attempt.mapFail Lexer.formatPositionInError with
        | Ok() -> printf "Done."
        | Fail errors -> errors |> List.iter (fun s -> printfn "%s" s)
    0 // return an integer exit code

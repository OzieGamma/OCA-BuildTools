// --------------------------------------------------------------------------------------------------------------------
// <copyright file="VMState.fs" company="Oswald Maskens">
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
namespace OCA.VM

open OCA.AsmLib
open OFuncLib

type State(instr : Positioned<uint32> []) = 
    let sp = ref 0u
    let ra = ref 0u
    let ea = ref 0u
    let rfS = Array.init 5 (fun i -> uint32 i)
    let rfT = Array.init 5 (fun i -> uint32 i)
    let breakpoints = new System.Collections.Generic.HashSet<uint32>()
    
    let rom = 
        Memory.rom "ROM-0" 4096u (fun addr -> 
            if addr < uint32 instr.Length then Ok(instr.[int addr] |> Position.remove)
            else Fail [ sprintf "No instruction here %d" addr ])
    
    let ram = Memory.ram "RAM-0" 4096u
    
    let memory = 
        Bus.none
        |> Bus.add rom 0u
        |> Attempt.bind (Bus.add ram 4096u)
        |> Attempt.get id
    
    member val pc = 0u with get, set
    
    member this.readReg r = 
        match r with
        | Zero -> 0u
        | SP -> !sp
        | RA -> !ra
        | EA -> !ea
        | SReg n -> rfS.[int n]
        | TReg n -> rfT.[int n]
    
    member this.writeReg r value = 
        match r with
        | Zero -> ()
        | SP -> sp := value
        | RA -> ra := value
        | EA -> ea := value
        | SReg n -> rfS.[int n] <- value
        | TReg n -> rfT.[int n] <- value
    
    member this.readMemory addr = 
        match memory.read addr with
        | Ok i -> i
        | Fail msg -> 
            msg |> List.iter (fun m -> printfn "%s" m)
            failwith "Memory failure"
    
    member this.writeMemory addr v = 
        match memory.write addr v with
        | Ok() -> ()
        | Fail msg -> 
            msg |> List.iter (fun m -> printfn "%s" m)
            failwith "Memory failure"
    
    member this.toggleBreakpoint addr = 
        if not (breakpoints.Contains addr) then this.addBreakpoint addr
        else this.removeBreakpoint addr
    
    member this.addBreakpoint addr = breakpoints.Add addr |> ignore
    member this.removeBreakpoint addr = breakpoints.Remove addr |> ignore
    member this.isBreakpoint () = breakpoints.Contains (this.pc)
    member this.jsonify = 
        let watch = new System.Diagnostics.Stopwatch()
        watch.Start()
        let sb = new System.Text.StringBuilder()
        sb.Append("{\"reg\":{\"PC\":").Append(this.pc).Append(",\"SP\":").Append(!sp).Append(",\"EA\":").Append(!ea).Append(",\"RA\":").Append(!ra) |> ignore
        for i = 0 to 4 do
            sb.Append(",\"s").Append(i).Append("\":").Append(this.readReg (SReg(uint16 i))) |> ignore
        for i = 0 to 4 do
            sb.Append(",\"t").Append(i).Append("\":").Append(this.readReg (TReg(uint16 i))) |> ignore
        sb.Append("},\"mem\":{") |> ignore
        memory.jsonify sb
        sb.Append("}, \"breakpoints\":[").Append(System.String.Join(",", breakpoints)).Append("]}") |> ignore
        let json = sb.ToString()
        watch.Stop()
        printfn "Generation time %d ms" watch.ElapsedMilliseconds
        json

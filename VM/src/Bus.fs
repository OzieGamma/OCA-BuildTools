// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Bus.fs" company="Oswald Maskens">
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

open OFuncLib
open System.Text

type Bus = 
    { read : uint32 -> Attempt<uint32>
      write : uint32 -> uint32 -> Attempt<unit>
      overlaps : uint32 -> uint32 -> Option<string>
      jsonify : StringBuilder -> unit }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Bus = 
    [<CompiledName("None")>]
    let none = 
        { read = (fun addr -> Fail [ sprintf "No memory here %d to read" addr ])
          write = (fun addr value -> Fail [ sprintf "No memory here %d to write" addr ])
          overlaps = (fun _ _ -> None)
          jsonify = (fun sb -> sb.Append "\"end\":0" |> ignore) }
    
    [<CompiledName("Add")>]
    let public add (memory : Memory) (offset : uint32) (prev : Bus) : Attempt<Bus> = 
        match prev.overlaps offset (offset + memory.size - 1u) with
        | Some name -> Fail [ sprintf "Memory %s overlaps with previously added memory %s" memory.name name ]
        | None -> 
            let inline addrInMemory addr = addr >= offset && addr < offset + memory.size
            
            let read addr = 
                if addrInMemory addr then memory.read (addr - offset)
                else prev.read addr
            
            let write addr value = 
                if addrInMemory addr then memory.write (addr - offset) value
                else prev.write addr value
            
            let overlaps start finish = 
                if (addrInMemory start) || (addrInMemory finish) then Some memory.name
                else None
            
            let jsonify sb = 
                prev.jsonify sb
                sb.Append(",\"").Append(memory.name).Append("\":{\"start\":").Append(offset).Append(",\"value\":") |> ignore
                memory.jsonify sb
                sb.Append("}") |> ignore
            
            Ok { read = read
                 write = write
                 overlaps = overlaps
                 jsonify = jsonify }

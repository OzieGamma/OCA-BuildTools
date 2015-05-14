// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Memory.fs" company="Oswald Maskens">
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
open System.Text

type Memory = 
    { read : uint32 -> Attempt<uint32>
      write : uint32 -> uint32 -> Attempt<unit>
      size : uint32
      name : string
      jsonify : StringBuilder -> unit }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Memory = 
    [<CompiledName("RAM")>]
    let ram (name : string) (size : uint32) : Memory = 
        let mem = Array.init (int size) (fun i -> uint32 i)
        let inMemory addr = addr < size
        
        let read addr = 
            if inMemory addr then Ok(mem.[int addr])
            else Fail [ sprintf "Can't read RAM %s at address %d" name addr ]
        
        let write addr value = 
            if inMemory addr then 
                mem.[int addr] <- value
                Ok()
            else Fail [ sprintf "Can't write RAM %s at address %d with value %d" name addr value ]
        
        let jsonify (sb : StringBuilder) = 
            sb.Append("{\"type\":\"RAM\",\"data\":[") |> ignore
            for i = 0 to int (size - 1u) do
                if i <> 0 then sb.Append "," |> ignore
                sb.Append(mem.[i]) |> ignore
            sb.Append "]}" |> ignore
        
        { read = read
          write = write
          size = size
          name = name
          jsonify = jsonify }
    
    [<CompiledName("ROM")>]
    let rom (name : string) (size : uint32) (initData : uint32 -> Attempt<uint32>) : Memory = 
        let write addr value = Fail [ sprintf "Can't write ROM. Attempt %s at address %d with value %d" name addr value ]
        
        let json = 
            let sb = new StringBuilder()
            sb.Append("{\"type\":\"ROM\",\"data\":[") |> ignore
            for i = 0 to int (size - 1u) do
                if i <> 0 then sb.Append "," |> ignore
                let entry = initData (uint32 i)
                match entry with
                | Ok v -> sb.Append("{\"v\":").Append(v).Append(",\"i\":\"").Append(sprintf "%A" (Kelos16.fromBin v)).Append("\"}") |> ignore
                | Fail _ -> sb.Append "-1" |> ignore
            sb.Append "]}" |> ignore
            sb.ToString()
        
        let jsonify (sb : StringBuilder) = sb.Append(json) |> ignore
        { read = initData
          write = write
          size = size
          name = name
          jsonify = jsonify }
    
    [<CompiledName("TextVGA")>]
    let textVga (name : string) (height : uint32) (width : uint32) (colorWidth : uint32) (numColors : uint32) : Memory = 
        let read addr = Fail [ "Not implemented" ]
        let write addr value = Fail [ "Not implemented" ]
        let jsonify (sb : StringBuilder) = sb.Append("{\"type\":\"VGA\"}") |> ignore
        { read = read
          write = write
          size = height * width
          name = name
          jsonify = jsonify }

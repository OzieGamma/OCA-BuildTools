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
    let textVga (name : string) (numRow : uint32) (numCol : uint32) (numColors : uint32) : Memory = 
        let vramWidth = uint32 (System.Math.Ceiling(System.Math.Log(float (numRow * numCol), 2.0)))
        let colorWidth = uint32 (System.Math.Ceiling(System.Math.Log(float numColors, 2.0)))
        let color_n_vram_bit = System.Math.Max(vramWidth, colorWidth)
        let colorMask = 1u <<< int color_n_vram_bit

        let colorData = Array.init (int numColors) (fun _ -> 0xFFFu)
        let colorDataString = Array.init (int numColors) (fun _ -> "FFF")
        let charData = Array.init (int (numRow * numCol)) (fun _ -> ' ')

        let read addr =
            if (addr &&& colorMask) = colorMask then
                let relAddr = int (addr - colorMask)
                if relAddr < colorData.Length then
                    Ok colorData.[relAddr]
                else 
                    Fail ["Read not inside VGA color zone"]
            else
                if addr < uint32 charData.Length then
                    Ok (uint32 charData.[int addr])
                else
                    Fail [ "Read not inside VGA sprite range" ]


        let write addr value = 
            if (addr &&& colorMask) = colorMask then
                let relAddr = int (addr - colorMask)
                if relAddr < colorData.Length then
                    colorData.[relAddr] <- value &&& 0xFFFu
                    colorDataString.[relAddr] <- sprintf "%X" colorData.[relAddr]
                    Ok ()
                else 
                    Fail ["Write not inside VGA color zone"]
            else
                if addr < uint32 charData.Length then
                    if value < 128u then
                        charData.[int addr] <- char value
                        Ok ()
                    else
                        Fail ["Write, VGA only supports chars < 128"]
                else
                    Fail [ "Write not inside VGA sprite range" ]

        let jsonify (sb : StringBuilder) = 
            sb.Append("{\"type\":\"VGA\",\"rows\":").Append(numRow).Append(",\"cols\":").Append(numCol).Append(",\"data\":[") |> ignore
            for row = 0 to (int numRow) - 1 do
                if row <> 0 then sb.Append "," |> ignore
                sb.Append "\"" |> ignore
                for col = 0 to (int numCol) - 1 do
                    sb.Append(charData.[row * (int numCol) + col]) |> ignore
                sb.Append "\"" |> ignore
            sb.Append("], \"colors\": [") |> ignore
            for i = 0 to colorData.Length - 1 do
                if i <> 0 then sb.Append "," |> ignore
                sb.Append("\"").Append(colorDataString.[i]).Append("\"") |> ignore
            sb.Append("]}") |> ignore

        { read = read
          write = write
          size = 1u <<< (int color_n_vram_bit + 1)
          name = name
          jsonify = jsonify }

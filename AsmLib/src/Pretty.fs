// --------------------------------------------------------------------------------------------------------------------
// <copyright file="Lexer.fs" company="Oswald Maskens">
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

open System

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Pretty = 
    let private stringify token = 
        match token with
        | NumberSign -> "#"
        | LeftBracket -> "["
        | RightBracket -> "]"
        | LeftParen -> "("
        | RightParen -> ")"
        | LeftBrace -> "{" + Environment.NewLine
        | RightBrace -> "}" + Environment.NewLine + Environment.NewLine
        | Dot -> "."
        | Colon -> ":"
        | Comma -> ","
        | Equals -> "="
        | Dolar -> "$"
        | Percentage -> "%"
        | Operator str -> str
        | Id str -> str
        | IntLit i -> i.ToString()
        | UIntLit u -> u.ToString()
        | CharLit c -> "'" + c.ToString() + "'"
        | StringLit s -> "\"" + s + "\""
        | BoolLit b -> 
            if b then "true"
            else "false"
        | Def -> "def"
        | Asm -> "asm"
        | Namespace -> "namespace"
        | Const -> "const"
        | Using -> "using"
        | For -> "for"
        | While -> "while"
        | Upto -> "upto"
        | Var -> "var"
        | Val -> "val"
    
    [<CompiledName("FormatPositionInError")>]
    let rec public print (tokens : List<Token>) : string = 
        match tokens with
        | head :: look :: tail when Instr.isInstrStart head (Some look) -> Environment.NewLine + (stringify head) + " " + (print (look :: tail))
        | head :: tail when Instr.isInstrStart head None -> Environment.NewLine + (stringify head) + " " + (print tail)
        | head :: tail -> (stringify head) + " " + (print tail)
        | [] -> ""

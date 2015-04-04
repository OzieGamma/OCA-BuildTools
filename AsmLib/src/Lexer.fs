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
open OFuncLib

type Token = 
    | NumberSign
    | LeftBracket
    | RightBracket
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Dot
    | Colon
    | Comma
    | Equals
    | Dolar
    | Percentage
    | Operator of string
    | Id of string
    | IntLit of bigint
    | UIntLit of bigint
    | CharLit of char
    | StringLit of string
    | BoolLit of bool
    | Def
    | Asm
    | Namespace
    | Const
    | Using
    | For
    | Upto
    | While
    | Val
    | Var

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Lexer = 
    [<CompiledName("FormatPositionInError")>]
    let public formatPositionInError (error : Positioned<string>) : string = 
        match error with
        | Positioned(str, Position(col, row, file)) -> sprintf "Error in file %s [%u:%u]: %s" file row col str
    
    [<CompiledName("PositionCol")>]
    let public positionCol (file : string) (row : uint32) (col : uint32) (c : char) : Positioned<char> = Positioned(c, Position(col, row, file))
    
    [<CompiledName("PositionRow")>]
    let public positionRow (file : string) (row : uint32) (line : seq<char>) : seq<Positioned<char>> = 
        line |> Seq.mapi (fun col -> positionCol file row (uint32 col + 1u))
    
    [<CompiledName("PositionFile")>]
    let public positionFile (file : string) (source : string) : seq<seq<Positioned<char>>> = 
        source.Split '\n' |> Seq.mapi (fun row -> positionRow file (uint32 row + 1u))
    
    [<CompiledName("RemoveCommentsInRow")>]
    let public removeCommentsInRow (src : seq<Positioned<char>>) : seq<Positioned<char>> = src |> Seq.takeWhile (fun x -> x.value <> '#')
    
    [<CompiledName("RemoveCommentsInFile")>]
    let public removeCommentsInFile (src : seq<seq<Positioned<char>>>) : seq<seq<Positioned<char>>> = src |> Seq.map removeCommentsInRow
    
    (* tokenize *)
    let private isSpace (c : Positioned<char>) = 
        match c.value with
        | ' ' | '\n' | '\t' | '\r' -> true
        | _ -> false
    
    let private isSingleChar (c : Positioned<char>) = 
        match c.value with
        | '#' | '[' | ']' | '(' | ')' | '{' | '}' | '.' | ':' | ',' | '%' | '$' -> true
        | _ -> false
    
    let private isOperatorChar (c : Positioned<char>) = 
        match c.value with
        | '+' | '-' | '*' | '/' | '<' | '>' | '|' | '&' | '^' | '~' | '=' -> true
        | _ -> false
    
    let private isHexChar (c : Positioned<char>) = 
        match c.value with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> true
        | _ -> false
    
    let private isDigitChar (c : Positioned<char>) = Char.IsDigit c.value
    let private isIdStart (c : Positioned<char>) = Char.IsLetter c.value || c.value = '_' || c.value = ';'
    let private isIdChar (c : Positioned<char>) = Char.IsLetterOrDigit c.value || c.value = '_' || c.value = ';'
    
    let private tokenizeSingleChar char = 
        char |> Positioned.mapAttempt (fun c -> 
                    match c with
                    | ']' -> Ok RightBracket
                    | '(' -> Ok LeftParen
                    | ')' -> Ok RightParen
                    | '{' -> Ok LeftBrace
                    | '}' -> Ok RightBrace
                    | '.' -> Ok Dot
                    | ':' -> Ok Colon
                    | '#' -> Ok NumberSign
                    | '[' -> Ok LeftBracket
                    | ',' -> Ok Comma
                    | '%' -> Ok Percentage
                    | '$' -> Ok Dolar
                    | _ -> Fail [ sprintf "Expected single char token, but got %c" c ])
    
    let private tokHelper f chars = 
        let head = chars |> Seq.head
        let noPos = chars |> Seq.map Position.remove
        let str = new System.String(noPos |> Array.ofSeq)
        Positioned(f str, head.position) |> Positioned.liftAttempt
    
    let private tokenizeId = tokHelper (fun str -> Ok(Id str))
    let private tokenizeOperator = tokHelper (fun str -> Ok(Operator str))

    let private tokenizeIntegerLit tokenizer numberstyle = 
        tokHelper (fun str -> 
            match bigint.TryParse(str, numberstyle, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> Ok(tokenizer v)
            | false, _ -> Fail [ sprintf "Can not parse [u]int literal %A" str ])
    
    let private escapeStringLit (str : string) = 
        let error = ref false
        let escape = ref false
        let acc = new System.Text.StringBuilder()
        str |> Seq.iter (fun c -> 
                   if !escape then 
                       match c with
                       | 'n' -> acc.Append('\n') |> ignore
                       | _ -> error := true
                       escape := false
                   else 
                       match c with
                       | '\\' -> escape := true
                       | c -> acc.Append(c) |> ignore)
        if !error then Fail [ sprintf "Invalidly escaped literal %s" str ]
        else Ok(acc.ToString())
    
    let private filterKeywords tok = 
        match tok with
        | Id "asm" -> Asm
        | Id "def" -> Def
        | Id "namespace" -> Namespace
        | Id "const" -> Const
        | Id "using" -> Using
        | Id "while" -> While
        | Id "for" -> For
        | Id "upto" -> Upto
        | Operator "=" -> Equals
        | Id "true" -> BoolLit true
        | Id "false" -> BoolLit false
        | Id "val" -> Val
        | Id "var" -> Var
        | _ -> tok
    
    // Imperative for performance reasons.
    [<CompiledName("Tokenize")>]
    let public tokenize (source : seq<Positioned<char>>) : PositionedListAttempt<Token> = 
        let result = new MutableList<PositionedAttempt<Token>>()
        let acc = new MutableList<Positioned<char>>()
        let inp = source |> Seq.toArray
        let i = ref 0
        let next() = i := !i + 1
        
        let accWhile f = 
            while !i < inp.Length && f inp.[!i] do
                acc.Add(inp.[!i])
                next()
        
        let intOrUInt numberstyle = 
            if !i < inp.Length && inp.[!i].value = 'u' then
                result.Add(tokenizeIntegerLit UIntLit numberstyle acc)
                next()
            else result.Add(tokenizeIntegerLit IntLit numberstyle acc)
        
        // Eats the end separator, the start should already have been eaten
        let stringOrCharLit separator = 
            let error = ref false
            let escape = ref false
            let acc = new System.Text.StringBuilder()
            while !i < inp.Length && not (inp.[!i - 1].value <> '\\' && inp.[!i].value = separator) do
                if !escape then 
                    match inp.[!i].value with
                    | 'n' -> acc.Append('\n') |> ignore
                    | '\\' -> acc.Append('\\') |> ignore
                    | _ -> error := true
                    escape := false
                else 
                    match inp.[!i].value with
                    | '\\' -> escape := true
                    | c -> acc.Append(c) |> ignore
                next()
            if !i >= inp.Length then Fail [ "Unclosed literal" ]
            else if !error then 
                next() // Eat separator
                Fail [ "Invalidly escaped literal" ]
            else 
                next() // Eat separator
                Ok(acc.ToString())
        
        while !i < inp.Length do
            acc.Clear()
            if isSpace inp.[!i] then next()
            else if inp.[!i].value = '"' then 
                let head = inp.[!i]
                next() // Opening "
                let literal = stringOrCharLit '"' |> Attempt.map StringLit
                result.Add(head |> Positioned.mapAttempt (fun _ -> literal))
            else if inp.[!i].value = '\'' then 
                let head = inp.[!i]
                next() // Opening '
                let literal = 
                    stringOrCharLit '\'' |> Attempt.bind (fun str -> 
                                                if str.Length <> 1 then Fail [ sprintf "Character literals must be 1 char long, invalid: %s" str ]
                                                else Ok(CharLit str.[0]))
                result.Add(head |> Positioned.mapAttempt (fun _ -> literal))
            else if isSingleChar inp.[!i] then 
                result.Add(tokenizeSingleChar inp.[!i])
                next()
            else if isOperatorChar inp.[!i] then 
                accWhile isOperatorChar
                result.Add(tokenizeOperator acc)
            else if !i + 1 < inp.Length && inp.[!i].value = '0' && inp.[!i + 1].value = 'x' && isHexChar inp.[!i + 2] then 
                acc.Add(Positioned('0', inp.[!i].position)) //Add 0 in front to make sure it is parsed as positive
                next()
                next()
                accWhile isHexChar
                intOrUInt Globalization.NumberStyles.HexNumber
            else if isDigitChar inp.[!i] then 
                accWhile isDigitChar
                intOrUInt Globalization.NumberStyles.Integer
            else if isIdStart inp.[!i] then 
                acc.Add(inp.[!i])
                next()
                accWhile isIdChar
                result.Add(tokenizeId acc)
            else 
                let failure = inp.[!i] |> Positioned.mapAttempt (fun c -> Fail [ sprintf "Can not tokenize char %c" c ])
                result.Add(failure)
                next()
        result
        |> List.ofSeq
        |> Attempt.liftList
        |> Attempt.map (List.map (Positioned.map filterKeywords))
    
    [<CompiledName("TokenizeFile")>]
    let public tokenizeFile (file : string) (source : string) : PositionedListAttempt<Token> = 
        source
        |> positionFile file
        |> removeCommentsInFile
        |> Seq.concat
        |> tokenize

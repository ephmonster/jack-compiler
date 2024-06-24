module Tokenizer

open System.IO
open System.Text.RegularExpressions
open System
open System.Collections.Generic
open Token

let inline (>=<) a (b,c) = a >= b && a <= c
let inline (+=) a b = a := a.Value + b
let inline (+==) a b = a := a.Value + $"{b}" 

let Keywords =
    ["class"; "constructor"; "function"; "method"; "field"; 
     "static"; "var"; "int"; "char"; "boolean"; "void"; "true"; 
     "false"; "null"; "this"; "let"; "do"; "if"; "else"; 
     "while"; "return"] |> Set

let Symbols = 
    ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; 
     '*'; '/'; '&'; '|'; '<'; '>'; '='; '~'] |> Set

let Ops = 
    ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '='] |> Set

let UniaryOps = 
    ['-'; '~'] |> Set


type Tokenizer(filepath: string) = 
    class
        let mutable _tokens = new List<Token>()
        let _reader = File.ReadAllText(filepath)
        let mutable _lines = File.ReadAllText(filepath).Split('\n')
        let mutable _curToken: Token = Token("null","null")
        let mutable current_word = ""
        let mutable position = ref 0

        member _.NextChar() =
            let next_char = _reader[position.Value]
            position += 1
            next_char
        
        member _.hasMoreTokens() = 
            position.Value < _reader.Length

        // member this.Advance() = 
        //     let mutable str = ""
        //     let mutable ch = this.NextChar()
        //     this.Skip()
        //     while ch <> -1 && not(System.Char.IsWhiteSpace(char ch)) do
        //         str <- str + $"{ch}"
        //         ch <- _reader.Read()
        //     _curToken = str

        // Will return a list of tokens 
        member this.Tokenize() = 
            let mutable i = 0
            let mutable lines = ResizeArray<string>()
            while i < _lines.Length do
                let mutable curLine = Regex.Replace(Regex.Replace(_lines[i].Trim(), @"\/\/.*$", ""), @"\/\*.*?\*\/", "");
                let mutable x = "a"
                if String.length(curLine) = 0 || curLine.StartsWith "//" ||
                    curLine.StartsWith "\r" then
                    i <- i + 1
                elif curLine.StartsWith "/*" then
                    while not(_lines[i].EndsWith "*/\r") do   
                        i <- i + 1
                    i <- i + 1
                else 
                    i <- i + 1
                    lines.Add(curLine)

            let mutable chars = ResizeArray<char>()
            for i in lines.ToArray() do
                let x = Regex.Replace($"{i}", @"\\n", "")
                chars.AddRange (x.Trim().ToCharArray())

            for i in chars.ToArray() do
                printf $"{i}"

            (* Start of the actual analysis *)
            let mutable chars = chars.ToArray()
            let position = ref 0

            while position.Value < chars.Length do
                let mutable current_char = chars[position.Value]
                
                // whitespace
                if current_char = ' ' then 
                    position += 1

                // keywords or identifiers
                elif Char.IsLetter(current_char) || current_char = '_' then
                    current_word <- current_word + $"{current_char}"
                    position += 1
                    let mutable next_char = chars[position.Value]
                    while position.Value < chars.Length && 
                        (Char.IsLetterOrDigit(next_char) || next_char = '_') do
                        if Char.IsLetter(next_char) then
                            position += 1
                            current_word <- current_word + $"{next_char}"
                            next_char <- chars[position.Value]
                            
                    // reserved keywords
                    if Tokenizer.isKeyword(current_word) then
                        _curToken <- Token("keyword", current_word)
                        _tokens.Add(_curToken)
                        if current_word = "null" then
                            ()
                    // identifier 
                    elif Tokenizer.isIdentifier(current_word) then
                        _curToken <- Token("identifier", current_word)
                        _tokens.Add(_curToken)
                    else exit(-1)
                    current_word <- ""
                    printfn "%A" _curToken
                
                // integers
                elif Char.IsDigit(current_char) then
                    current_word <- current_word + $"{current_char}"
                    position += 1
                    let mutable next_char = chars[position.Value]
                    while position.Value < chars.Length && Char.IsDigit(next_char) do
                        position += 1
                        current_word <- current_word + $"{next_char}"
                        next_char <- chars[position.Value]
                    if Tokenizer.isIntegerConstant(current_word) then
                        _curToken <- Token("integerConstant", current_word)
                        _tokens.Add(_curToken)
                    else exit(-1)
                    current_word <- ""
                    printfn "%A" _curToken
                
                // strings
                elif current_char = '"' then
                    position += 1
                    let mutable next_char = chars[position.Value]
                    while position.Value < chars.Length && next_char <> '"' do
                        position += 1
                        current_word <- current_word + $"{next_char}"
                        next_char <- chars[position.Value]
                     
                    // might have to check for next char
                    _curToken <- Token("stringConstant", current_word)
                    _tokens.Add(_curToken)
                    current_word <- ""
                    position += 1 // since cur_char = ", skip
                    printfn "%A" _curToken

                elif Symbols.Contains(current_char) then
                    _curToken <- Token("symbol", $"{current_char}")
                    _tokens.Add(_curToken)
                    position += 1
                    printfn "%A" _curToken
                else exit(-1)

            _tokens
                
                
        static member isKeyword(token: string) = 
            Keywords.Contains(token)
            
        member this.isSymbol() =
            String.length(current_word) = 1 && Symbols.Contains(char current_word)

        static member isIntegerConstant(token: string) = 
            let success, value = Int32.TryParse(token)
            success && value >=< (0, 32767) // value between 0 and 2^15

        static member isStringConstant(token: string) =
            Regex.IsMatch(token, "^\"[^\n\"]*\"$")

        static member isIdentifier(token: string) =
            Regex.IsMatch(token, "^[a-zA-Z_][a-zA-Z0-9_]*$")
    end





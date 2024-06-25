module Tokenizer

open System.IO
open System.Text.RegularExpressions
open System
open System.Collections.Generic
open Token

let inline (>=<) a (b,c) = a >= b && a <= c
let inline (+=) a b = a := a.Value + b 

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

let isKeyword(word: string) = 
        Keywords.Contains(word)
        
let isSymbol(word:string) =
    word.Length = 1 && Symbols.Contains(char word)

let isIntegerConstant(word: string) = 
    let success, value = Int32.TryParse(word)
    success && value >=< (0, 32767) // value between 0 and 2^15

let isStringConstant(word: string) =
    Regex.IsMatch(word, "^\"[^\n\"]*\"$")

let isIdentifier(word: string) =
    Regex.IsMatch(word, "^[a-zA-Z_][a-zA-Z0-9_]*$")

type Tokenizer(filepath: string) as self = 
    let mutable _tokens = new List<Token>()
    let mutable _curToken = Token()
    let mutable _chars: char array = Array.empty  
    let mutable _position = ref -1
    do 
        self.Tokenizer(filepath)

    member _.Tokenizer(filepath:string) =
        let i = ref 0
        let mutable input = File.ReadAllText(filepath).Split('\n')
        let mutable lines = ResizeArray<string>()
        while i.Value < input.Length do
            let mutable curLine = Regex.Replace(Regex.Replace(input[i.Value].Trim(), @"\/\/.*$", ""), @"\/\*.*?\*\/", "");
            if String.length(curLine) = 0 || curLine.StartsWith "//" ||
                curLine.StartsWith "\r" then
                i += 1
            elif curLine.StartsWith "/*" then
                while not(input[i.Value].EndsWith("*/\r")) do   
                    i += 1
                i += 1
            else 
                i += 1
                lines.Add(curLine)

        let mutable chars = ResizeArray<char>()
        for i in lines.ToArray() do
            let x = Regex.Replace($"{i}", @"\\n", "")
            chars.AddRange(x.Trim().ToCharArray())

        _chars <- chars.ToArray()

    member _.NextChar() =
        _position += 1
        _chars[_position.Value]

    member _.Peek() =
        _chars[_position.Value + 1]
    
    member _.hasMoreTokens() = 
        _position.Value + 1 < _chars.Length

    member _.TokenType() =
        match _curToken.Type with
            | "keyword" -> "KEYWORD"
            | "symbol" -> "SYMBOL"
            | "identifier" -> "IDENTIFIER"
            | "integerConstant" -> "INT_CONST"
            | "stringConstant" -> "STRING_CONST" 
            | "null" -> "NULL_TYPE"

    member _.Keyword() = 
        let mutable t = ""
        if _curToken.Type = "keyword" then
            t <- _curToken.Lexeme.ToUpper()  
        t       
    
    member _.Symbol() =
        let mutable t = ""
        if _curToken.Type = "symbol" then
            t <- _curToken.Lexeme
        t 

    member _.Identifier() =
        let mutable t = ""
        if _curToken.Type = "identifier" then
            t <- _curToken.Lexeme
        t 

    member _.IntVal() =
        let mutable t = ""
        if _curToken.Type = "integerConstant" then
            t <- _curToken.Lexeme
        t 
    
    member _.stringVal() =
        let mutable t = ""
        if _curToken.Type = "stringConstant" then
            t <- _curToken.Lexeme
        t 
    
    member this.Advance() = 
        let mutable current_word = ""
        let mutable current_char = this.NextChar()
        let mutable next_char = char 0

        // whitespace
        if current_char = ' ' then 
            this.Advance()   

        // keywords or identifiers
        elif Char.IsLetter(current_char) || current_char = '_' then
            current_word <- current_word + $"{current_char}"

            while this.hasMoreTokens() && (Char.IsLetterOrDigit(this.Peek()) || this.Peek() = '_') do
                next_char <- this.NextChar()
                current_word <- current_word + $"{next_char}"
                    
            // reserved keywords
            if isKeyword(current_word) then
                this.AddToken(Token("keyword", current_word))
            // identifier 
            elif isIdentifier(current_word) then
                this.AddToken(Token("identifier", current_word))
            else exit(-1)
        
        // integers
        elif Char.IsDigit(current_char) then
            current_word <- current_word + $"{current_char}"

            while this.hasMoreTokens() && Char.IsDigit(this.Peek()) do
                next_char <- this.NextChar()
                current_word <- current_word + $"{next_char}"

            if isIntegerConstant(current_word) then
                this.AddToken(Token("integerConstant", current_word))
            else exit(-1)
        
        // strings
        elif current_char = '"' then
            let mutable next_char = this.NextChar()
            
            while this.hasMoreTokens() && next_char <> '"' do
                current_word <- current_word + $"{next_char}"
                next_char <- this.NextChar()
            
            this.AddToken(Token("stringConstant", current_word))

        elif Symbols.Contains(current_char) then
            this.AddToken(Token("symbol", $"{current_char}"))

        else exit(-1)

    // get tokens
    member this.Tokenize() =
        while this.hasMoreTokens() do
            this.Advance()

        _tokens

     // Also prints the token to console
    member _.AddToken(token:Token) =
        _curToken <- token        
        _tokens.Add(token)
        printfn "%A" token




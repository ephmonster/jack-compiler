module Tokenizer

open System.Text
open System.IO
open System.Text.RegularExpressions
open System

let inline (>=<) a (b,c) = a >= b && a <= c


let LegalKeywords =
    ["class"; "constructor"; "function"; "method"; "field"; 
     "static"; "var"; "int"; "char"; "boolean"; "void"; "true"; 
     "false"; "null"; "this"; "let"; "do"; "if"; "else"; 
     "while"; "return"] |> Set

let LegalSymbols = 
    ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; 
     '*'; '/'; '&'; '|'; '<'; '>'; '='; '~'] |> Set


type Token(typ, text) = 
    class
        let Type = typ
        let Text = text

        member _.IsKeyword() = Type = "KEYWORD"
        member _.IsSymbol() = Type = "SYMBOL"
        member _.IsIdentifier() = Type = "IDENTIFIER"
        member _.IsInteger() = Type = "INT_CONST"
        member _.IsString() = Type = "STRING_CONST"
    end


type Tokenizer(filepath: string) = 
    class
        let mutable _tokens: List<Token> = []
        let _reader = new StreamReader(filepath)
        let mutable _lines = File.ReadAllText(filepath).Split('\n')
        let _curToken = ""

        member _.NextChar() =
            _reader.Read() 
        
        member _.Skip() =
            while not _reader.EndOfStream && Char.IsWhiteSpace(char(_reader.Peek())) do
                _reader.Read() |> ignore

        member _.hasMoreTokens() = 
            not _reader.EndOfStream

        member this.Advance() = 
            let mutable str = ""
            let mutable ch = this.NextChar()
            this.Skip()
            while ch <> -1 && not(System.Char.IsWhiteSpace(char ch)) do
                str <- str + $"{ch}"
                ch <- _reader.Read()
            _curToken = str

        member this.Tokenize() = 
            let mutable i = 0
            let mutable lines = ResizeArray<string>()
            while i < _lines.Length do
                let mutable curLine = Regex.Replace(Regex.Replace(_lines[i].Trim(), @"\/\/.*$", ""), @"\/\*.*?\*\/", "");
                
                if String.length(curLine) = 0 || curLine.StartsWith "//" ||
                    curLine.StartsWith "\r" then
                    i <- i + 1
                elif curLine.StartsWith "/*" then
                    while not(_lines[i].EndsWith "*/\r") do   
                        i <- i + 1
                    i <- i + 1
                else 
                    i <- i + 1
                    lines.Add curLine

            let mutable chars = ResizeArray<char>()
            for i in lines.ToArray() do
                printfn $"{i}"
                chars.AddRange (i.Trim().ToCharArray())

            //for i in chars.ToArray() do
                //printfn $"{i}"
                
            
            let mutable curWord = ""
            let mutable curChar = ""
            let mutable chars = chars.ToArray()
            let mutable position = 0

            while position < chars.Length do
                curChar <- $"{chars[position]}"

                // case: going through lots of whitespace
                if curChar = " " && curWord.Trim() = String.Empty then
                    position <- position + 1
                // case: reading characters
                elif Char.IsLetter(char(curChar)) then
                    curWord <- curWord + curChar
                    position <- position + 1
                // case: reading numbers
                elif Char.IsNumber(char(curChar)) then
                    curWord <- curWord + curChar
                    position <- position + 1
                // case: char is empty or a symbol
                elif curChar = " " || LegalSymbols.Contains(char curWord) then
                    curWord <- curWord + $"{chars[position]}"
                
                
                
                                    

        

        member this.TokenType() =
            if this.isKeyword() then
                "KEYWORD"
            elif this.isSymbol() then
                "SYMBOL"
            else
                "ERROR"
            

        member this.isKeyword() = 
            Tokenizer.LegalKeywords.Contains(_curToken)

        member this.isSymbol() =
            String.length(_curToken) = 1 && Tokenizer.LegalSymbols.Contains(char _curToken)

        static member isIntegerConstant(token: string) = 
            let success, value = Int32.TryParse(token)
            success && value >=< (0, 32767) // value between 0 and 2^15

        static member isStringConstant(token: string) =
            Regex.IsMatch(token, "^\"[^\n\"]*\"$")

        static member isIdentifier(token: string) =
            Regex.IsMatch(token, "^[a-zA-Z_][a-zA-Z0-9_]*$")
    end





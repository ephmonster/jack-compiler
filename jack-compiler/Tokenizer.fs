module Tokenizer

open System.Text
open System.IO
open System.Text.RegularExpressions

// Define sets of keywords and symbols
let keywords = 
    Set.ofList ["class"; "constructor"; "function"; "method"; "field"; "static"; "var";
                "int"; "char"; "boolean"; "void"; "true"; "false"; "null"; "this"; "let";
                "do"; "if"; "else"; "while"; "return"]

let symbols = 
    Set.ofList ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; '*'; 
                '/'; '&'; '|'; '<'; '>'; '='; '~']

// Function to check if a token is a keyword
let isKeyword token = 
    keywords.Contains(token)

// Function to check if a token is a symbol
let isSymbol token =
    token.Length = 1 && symbols.Contains(token.[0])

// Function to check if a token is an integer constant
let isIntegerConstant token =
    let success, value = Int32.TryParse(token)
    success && value >= 0 && value <= 32767

// Function to check if a token is a string constant
let isStringConstant token =
    let pattern = "^\"[^\n\"]*\"$"
    Regex.IsMatch(token, pattern)

// Function to check if a token is an identifier
let isIdentifier token =
    let pattern = "^[a-zA-Z_][a-zA-Z0-9_]*$"
    Regex.IsMatch(token, pattern)


// our class for defintion for the tokenizer
type Tokenizer(filepath:string) =
    let reader = new StreamReader(filepath)

    let mutable currentToken : string option = None 

    let advanceOnWhite (reader: StreamReader) = 
        while not reader.EndOfStream && System.Char.IsWhiteSpace(char (reader.Peek())) do
            reader.Read()

    member this.hasMoreTokens() = 
        not reader.EndOfStream

    member this.advance() = 
        let sb = StringBuilder()
        let mutable nextChar = reader.Read()
        advanceOnWhite(reader)
        while nextChar <> -1 && not (System.Char.IsWhiteSpace(char nextChar)) do
            sb.Append(char nextChar)
            nextChar <- reader.Read()
        let currentToken = sb.ToString()
        currentToken










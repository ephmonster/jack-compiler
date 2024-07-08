module Program
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic


[<StructuredFormatDisplay("<{Type}>{Lexeme}</{Type}>")>]
type public Token(typ:string, lex:string) = 
    let xmlTok typ lex =
        if typ = "symbol" then
            match lex with
            |"<" -> "&lt;"
            |">" -> "&gt;"
            |"&" -> "&amp;"
            |_ -> lex
        else
            lex

    member val Type = typ 
    member val Lexeme = xmlTok typ lex
    new() = Token(null, null)

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

    member _.Tokens=_tokens

    member _.Tokenizer(filepath:string) =
        let i = ref 0
        let mutable input = File.ReadAllText(filepath).Split('\n')
        let mutable lines = ResizeArray<string>()
        while i.Value < input.Length do
            let mutable curLine = Regex.Replace(Regex.Replace(input.[i.Value].Trim(), @"\/\/.*$", ""), @"\/\*.*?\*\/", "");
            if String.length(curLine) = 0 || curLine.StartsWith "//" ||
                curLine.StartsWith "\r" then
                i += 1
            elif curLine.StartsWith "/*" then
                while not(input.[i.Value].EndsWith("*/\r")) do   
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
        _chars.[_position.Value]

    member _.Peek() =
        _chars.[_position.Value + 1]
    
    member _.hasMoreTokens() = 
        _position.Value + 1 < _chars.Length

    member _.tokenType() =
        match _curToken.Type with
            | "keyword" -> "KEYWORD"
            | "symbol" -> "SYMBOL"
            | "identifier" -> "IDENTIFIER"
            | "integerConstant" -> "INT_CONST"
            | "stringConstant" -> "STRING_CONST" 
            | "null" -> "NULL_TYPE"

    member _.keyWord() = 
        let mutable t = ""
        if _curToken.Type = "keyword" then
            t <- _curToken.Lexeme.ToUpper()  
        t       
    
    member _.symbol() =
        let mutable t = ""
        if _curToken.Type = "symbol" then
            t <- _curToken.Lexeme
        t 

    member _.identifier() =
        let mutable t = ""
        if _curToken.Type = "identifier" then
            t <- _curToken.Lexeme
        t 

    member _.intVal() =
        let mutable t = ""
        if _curToken.Type = "integerConstant" then
            t <- _curToken.Lexeme
        t 
    
    member _.stringVal() =
        let mutable t = ""
        if _curToken.Type = "stringConstant" then
            t <- _curToken.Lexeme
        t 
    
    member this.advance() = 
        let mutable current_word = ""
        let mutable current_char = this.NextChar()
        let mutable next_char = char 0

        // whitespace
        if current_char = ' ' then 
            this.advance()   

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
            this.advance()

        _tokens

     // Also prints the token to console
    member _.AddToken(token:Token) =
        _curToken <- token        
        _tokens.Add(token)
        //printfn "%A" token



//(1) need to go over and make sure tokenizer functions like .symbol() are only called when legal
//(2) make sure that tokenizer.advance() is synchronized among functions
//(3) make sure handling symbol matching properly, some symbols return gt&
//(4) still need to finish rough draft for last two functions
type CompilationEngine(inputFile: string, outputFile: string) = 
    let writer = new StreamWriter(outputFile)
    let tokenizer = new Tokenizer(inputFile)

    member this.compileClass() = 
        writer.WriteLine("<class>")
        Console.WriteLine("<class>")
        tokenizer.advance()
        match tokenizer.keyWord() with 
            | "CLASS" -> 
                writer.WriteLine("<keyword> class </keyword>")
                Console.WriteLine("<keyword> class </keyword>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        match tokenizer.symbol() with 
            | "{" -> 
                writer.WriteLine("<symbol> { </symbol>")
                Console.WriteLine("<symbol> { </symbol>")
            | _ -> failwith("Incorrect compileClass")
        tokenizer.advance()
        while tokenizer.keyWord() = "STATIC" || tokenizer.keyWord() = "FIELD" do
            writer.WriteLine("<classVarDec>")
            Console.WriteLine("<classVarDec>")
            this.compileClassVarDec()
            writer.WriteLine("</classVarDec>")
            Console.WriteLine("</classVarDec>")
            tokenizer.advance()
        while tokenizer.keyWord() = "CONSTRUCTOR" || tokenizer.keyWord() = "FUNCTION" || tokenizer.keyWord() = "METHOD" do
            writer.WriteLine("<subroutineDec>")
            Console.WriteLine("<subroutineDec>")
            this.compileSubroutine()
            writer.WriteLine("</subroutineDec>")
            Console.WriteLine("</subroutineDec>")
            tokenizer.advance()
        match tokenizer.symbol() with 
            | "}" -> 
                writer.WriteLine("<symbol> } </symbol>")
                Console.WriteLine("<symbol> } </symbol>")
            | _ -> failwith("Incorrect compileClass")
        writer.WriteLine("</class>")
        Console.WriteLine("</class>")
        writer.Flush()
        writer.Close()
        


    member this.compileClassVarDec() =
        if tokenizer.keyWord() = "STATIC" then
            writer.WriteLine("<keyword> static </keyword>")
            Console.WriteLine("<keyword> static </keyword>")
        elif tokenizer.keyWord() = "FIELD" then
            writer.WriteLine("<keyword> field </keyword>")
            Console.WriteLine("<keyword> field </keyword>")
        tokenizer.advance()
        match tokenizer.tokenType() with
            | "KEYWORD" -> 
                match tokenizer.keyWord() with
                    | "INT" -> 
                        writer.WriteLine("<keyword> int </keyword>")
                        Console.WriteLine("<keyword> int </keyword>")
                    | "CHAR" -> 
                        writer.WriteLine("<keyword> char </keyword>")
                        Console.WriteLine("<keyword> char </keyword>")
                    | "BOOLEAN" -> 
                        writer.WriteLine("<keyword> boolean </keyword>")
                        Console.WriteLine("<keyword> boolean </keyword>")
                    | _ -> failwith("Incorrect ClassVarDec")
            | "IDENTIFIER" -> 
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
        tokenizer.advance()
        if tokenizer.symbol() = "," then
            while tokenizer.symbol() = "," do
                writer.WriteLine("<symbol> , </symbol>")
                Console.WriteLine("<symbol> , </symbol>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    |"IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect ClassVarDec")
                tokenizer.advance()
        match tokenizer.symbol() with 
            |";" -> 
                writer.WriteLine("<symbol> ; </symbol>")
                Console.WriteLine("<symbol> ; </symbol>")
            | _ -> failwith("Incorrect ClassVarDec")

    member this.compileSubroutine() =
        match tokenizer.keyWord() with
            |"CONSTRUCTOR" -> 
                writer.WriteLine("<keyword> constructor </keyword>")
                Console.WriteLine("<keyword> constructor </keyword>")
            |"METHOD" -> 
                writer.WriteLine("<keyword> method </keyword>")
                Console.WriteLine("<keyword> method </keyword>")
            |"FUNCTION" -> 
                writer.WriteLine("<keyword> function </keyword>")
                Console.WriteLine("<keyword> function </keyword>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.tokenType() with
                   | "KEYWORD" -> 
                        match tokenizer.keyWord() with
                            | "VOID" -> 
                                writer.WriteLine("<keyword> void </keyword>")
                                Console.WriteLine("<keyword> void </keyword>")
                            | "INT" -> 
                                writer.WriteLine("<keyword> int </keyword>")
                                Console.WriteLine("<keyword> int </keyword>")
                            | "CHAR" -> 
                                writer.WriteLine("<keyword> char </keyword>")
                                Console.WriteLine("<keyword> char </keyword>")
                            | "BOOLEAN" -> 
                                writer.WriteLine("<keyword> boolean </keyword>")
                                Console.WriteLine("<keyword> boolean </keyword>")
                            | _ -> failwith("Incorrect SubRoutine")
                   | "IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                   | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.tokenType() with 
            | "IDENTIFIER" -> 
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        match tokenizer.symbol() with 
            | "(" -> 
                writer.WriteLine("<symbol> ( </symbol>")
                Console.WriteLine("<symbol> ( </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        writer.WriteLine("<parameterList>")
        Console.WriteLine("<parameterList>")
        this.compileParameterList()
        writer.WriteLine("</parameterList>")
        Console.WriteLine("</parameterList>")
        match tokenizer.symbol() with
            | ")" -> 
                writer.WriteLine("<symbol> ) </symbol>")
                Console.WriteLine("<symbol> ) </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        tokenizer.advance()
        writer.WriteLine("<subroutineBody>")
        Console.WriteLine("<subroutineBody>")
        this.compileSubroutineBody()
        writer.WriteLine("</subroutineBody>")
        Console.WriteLine("</subroutineBody>")

        member this.compileParameterList()=
            if tokenizer.tokenType() = "IDENTIFIER" || tokenizer.keyWord() = "VOID" || tokenizer.keyWord() = "INT" ||tokenizer.keyWord()="CHAR" || tokenizer.keyWord() = "BOOLEAN" then
                match tokenizer.tokenType() with
                | "KEYWORD" -> 
                     match tokenizer.keyWord() with
                         | "VOID" -> 
                            writer.WriteLine("<keyword> void </keyword>")
                            Console.WriteLine("<keyword> void </keyword>")
                         | "INT" -> 
                            writer.WriteLine("<keyword> int </keyword>")
                            Console.WriteLine("<keyword> int </keyword>")
                         | "CHAR" -> 
                            writer.WriteLine("<keyword> char </keyword>")
                            Console.WriteLine("<keyword> char </keyword>")
                         | "BOOLEAN" -> 
                            writer.WriteLine("<keyword> boolean </keyword>")
                            Console.WriteLine("<keyword> boolean </keyword>")
                         | _ -> failwith("Incorrect paramList")
                | "IDENTIFIER" -> 
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    | "IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect ParamList")
                tokenizer.advance()
                while tokenizer.symbol() = "," do
                    writer.WriteLine("<symbol> , </symbol>")
                    Console.WriteLine("<symbol> , </symbol>")
                    tokenizer.advance()
                    match tokenizer.tokenType() with
                    | "KEYWORD" -> 
                         match tokenizer.keyWord() with
                             | "VOID" -> 
                                writer.WriteLine("<keyword> void </keyword>")
                                Console.WriteLine("<keyword> void </keyword>")
                             | "INT" -> 
                                writer.WriteLine("<keyword> int </keyword>")
                                Console.WriteLine("<keyword> int </keyword>")
                             | "CHAR" -> 
                                writer.WriteLine("<keyword> char </keyword>")
                                Console.WriteLine("<keyword> char </keyword>")
                             | "BOOLEAN" -> 
                                writer.WriteLine("<keyword> boolean </keyword>")
                                Console.WriteLine("<keyword> boolean </keyword>")
                             | _ -> failwith("Incorrect paramList")
                    | "IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    |_ -> failwith("Incorrect paramList")
                    tokenizer.advance()
                    match tokenizer.tokenType() with 
                        | "IDENTIFIER" -> 
                            writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                            Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        | _ -> failwith("Incorrect ParamList")
                    tokenizer.advance()

       

        member this.compileSubroutineBody()=
            match tokenizer.symbol() with 
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect subRoutineBody")
            tokenizer.advance()
            while tokenizer.keyWord() = "VAR" do
                writer.WriteLine("<varDec>")
                Console.WriteLine("<varDec>")
                this.compileVarDec()
                writer.WriteLine("</varDec>")
                Console.WriteLine("</varDec>")
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with 
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect subRoutineBody")

        member this.compileVarDec() =
            writer.WriteLine("<keyword> var </keyword>")
            Console.WriteLine("<keyword> var </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
            | "KEYWORD" -> 
                 match tokenizer.keyWord() with
                     | "INT" -> 
                        writer.WriteLine("<keyword> int </keyword>")
                        Console.WriteLine("<keyword> int </keyword>")
                     | "CHAR" -> 
                        writer.WriteLine("<keyword> char </keyword>")
                        Console.WriteLine("<keyword> char </keyword>")
                     | "BOOLEAN" -> 
                        writer.WriteLine("<keyword> boolean </keyword>")
                        Console.WriteLine("<keyword> boolean </keyword>")
                     | _ -> failwith("Incorrect varDec")
            | "IDENTIFIER" -> 
                writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
            | _ -> failwith("Incorrect varDec")
            tokenizer.advance()
            match tokenizer.tokenType() with 
                | "IDENTIFIER" -> 
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                | _ -> failwith("Incorrect varDec")
            tokenizer.advance()
            while tokenizer.symbol() = "," do
                writer.WriteLine("<symbol> , </symbol>")
                Console.WriteLine("<symbol> , </symbol>")
                tokenizer.advance()
                match tokenizer.tokenType() with 
                    | "IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    | _ -> failwith("Incorrect varDec")
                tokenizer.advance()
            match tokenizer.tokenType() with
                | "SYMBOL" ->
                    match tokenizer.symbol() with
                    |";" -> 
                        writer.WriteLine("<symbol> ; </symbol>")
                        Console.WriteLine("<symbol> ; </symbol>")
                    | _ -> failwith("Incorrect varDec")
                |_-> failwith("Incorrect varDec")
            tokenizer.advance()

        member this.compileStatements()=
            if tokenizer.keyWord() = "LET" || tokenizer.keyWord() = "IF" || tokenizer.keyWord() = "WHILE" || tokenizer.keyWord() = "DO" || tokenizer.keyWord() = "RETURN" then
                while tokenizer.keyWord() = "LET" || tokenizer.keyWord() = "IF" || tokenizer.keyWord() = "WHILE" || tokenizer.keyWord() = "DO" || tokenizer.keyWord() = "RETURN" do
                    match tokenizer.keyWord() with
                        | "LET" -> 
                            writer.WriteLine("<letStatement>")
                            Console.WriteLine("<letStatement>")
                            this.compileLet()
                            writer.WriteLine("</letStatement>")
                            Console.WriteLine("</letStatement>")
                        | "IF" -> 
                            writer.WriteLine("<ifStatement>")
                            Console.WriteLine("<ifStatement>")
                            this.compileIf()
                            writer.WriteLine("</ifStatement>")
                            Console.WriteLine("</ifStatement>")
                        | "WHILE" -> 
                            writer.WriteLine("<whileStatement>")
                            Console.WriteLine("<whileStatement>")
                            this.compileWhile()
                            writer.WriteLine("</whileStatement>")
                            Console.WriteLine("</whileStatement>")
                        | "DO" -> 
                            writer.WriteLine("<doStatement>")
                            Console.WriteLine("<doStatement>")
                            this.compileDo()
                            writer.WriteLine("</doStatement>")
                            Console.WriteLine("</doStatement>")
                        | "RETURN" -> 
                            writer.WriteLine("<returnStatement>")
                            Console.WriteLine("<returnStatement>")
                            this.compileReturn()
                            writer.WriteLine("</returnStatement>")
                            Console.WriteLine("</returnStatement>")
            else
               ()

        member this.compileLet()=
            writer.WriteLine("<keyword> let </keyword>")
            Console.WriteLine("<keyword> let </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                | "IDENTIFIER" -> 
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                | _ -> failwith("Incorrect let")
            tokenizer.advance()
            if tokenizer.symbol() = "[" then
                writer.WriteLine("<symbol> [ </symbol>")
                Console.WriteLine("<symbol> [ </symbol>")
                tokenizer.advance()
                writer.WriteLine("<expression>")
                Console.WriteLine("<expression>")
                this.compileExpression()
                writer.WriteLine("</expression>")
                Console.WriteLine("</expression>")
                match tokenizer.symbol() with
                    | "]" -> 
                        writer.WriteLine("<symbol> ] </symbol>")
                        Console.WriteLine("<symbol> ] </symbol>")
                    | _ -> failwith("Incorrect let")
                tokenizer.advance()
            match tokenizer.symbol() with
                | "=" -> 
                    writer.WriteLine("<symbol> = </symbol>")
                    Console.WriteLine("<symbol> = </symbol>")
                | _ -> failwith("Incorrect let")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ";" -> 
                    writer.WriteLine("<symbol> ; </symbol>")
                    Console.WriteLine("<symbol> ; </symbol>")
                | _ -> failwith("Incorrect let")
            tokenizer.advance()

        member this.compileIf()=
            writer.WriteLine("<keyword> if </keyword>")
            Console.WriteLine("<keyword> if </keyword>")
            tokenizer.advance()
            match tokenizer.symbol() with 
                | "(" -> 
                    writer.WriteLine("<symbol> ( </symbol>")
                    Console.WriteLine("<symbol> ( </symbol>")
                |_ -> failwith("Incorrect if")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ")" -> 
                    writer.WriteLine("<symbol> ) </symbol>")
                    Console.WriteLine("<symbol> ) </symbol>")
                | _ -> failwith("Incorrect if")
            tokenizer.advance()
            match tokenizer.symbol() with
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect if")
            tokenizer.advance()
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect if")
            tokenizer.advance()
            if tokenizer.keyWord() = "ELSE" then
                writer.WriteLine("<keyword> else </keyword>")
                Console.WriteLine("<keyword> else </keyword>")
                tokenizer.advance()
                match tokenizer.symbol() with
                    | "{" -> 
                        writer.WriteLine("<symbol> { </symbol>")
                        Console.WriteLine("<symbol> { </symbol>")
                    | _ -> failwith("Incorrect if")
                tokenizer.advance()
                writer.WriteLine("<statements>")
                Console.WriteLine("<statements>")
                this.compileStatements()
                writer.WriteLine("</statements>")
                Console.WriteLine("</statements>")
                match tokenizer.symbol() with
                    | "}" -> 
                        writer.WriteLine("<symbol> } </symbol>")
                        Console.WriteLine("<symbol> } </symbol>")
                    | _ -> failwith("Incorrect if")
                tokenizer.advance()



        member this.compileWhile()=
            writer.WriteLine("<keyword> while </keyword>")
            Console.WriteLine("<keyword> while </keyword>")
            tokenizer.advance()
            match tokenizer.symbol() with 
                | "(" -> 
                    writer.WriteLine("<symbol> ( </symbol>")
                    Console.WriteLine("<symbol> ( </symbol>")
                |_ -> failwith("Incorrect while")
            tokenizer.advance()
            writer.WriteLine("<expression>")
            Console.WriteLine("<expression>")
            this.compileExpression()
            writer.WriteLine("</expression>")
            Console.WriteLine("</expression>")
            match tokenizer.symbol() with
                | ")" -> 
                    writer.WriteLine("<symbol> ) </symbol>")
                    Console.WriteLine("<symbol> ) </symbol>")
                | _ -> failwith("Incorrect while")
            tokenizer.advance()
            match tokenizer.symbol() with
                | "{" -> 
                    writer.WriteLine("<symbol> { </symbol>")
                    Console.WriteLine("<symbol> { </symbol>")
                | _ -> failwith("Incorrect while")
            tokenizer.advance()
            writer.WriteLine("<statements>")
            Console.WriteLine("<statements>")
            this.compileStatements()
            writer.WriteLine("</statements>")
            Console.WriteLine("</statements>")
            match tokenizer.symbol() with
                | "}" -> 
                    writer.WriteLine("<symbol> } </symbol>")
                    Console.WriteLine("<symbol> } </symbol>")
                | _ -> failwith("Incorrect while")
            tokenizer.advance()

        member this.compileDo()=
            writer.WriteLine("<keyword> do </keyword>")
            Console.WriteLine("<keyword> do </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                |"IDENTIFIER" -> 
                    writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                    tokenizer.advance()
                    if tokenizer.tokenType() = "SYMBOL" then
                        match tokenizer.symbol() with
                            | "(" -> 
                                writer.WriteLine("<symbol> ( </symbol>")
                                Console.WriteLine("<symbol> ( </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<expressionList>")
                                Console.WriteLine("<expressionList>")
                                this.compileExpressionList()
                                writer.WriteLine("</expressionList>")
                                Console.WriteLine("</expressionList>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect do")
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | ";" -> 
                                        writer.WriteLine("<symbol> ; </symbol>")
                                        Console.WriteLine("<symbol> ; </symbol>")
                                    |_ -> 
                                        failwith("incorrect do")
                            | "." ->
                                writer.WriteLine("<symbol> . </symbol>")
                                Console.WriteLine("<symbol> . </symbol>")
                                tokenizer.advance()
                                match tokenizer.tokenType() with
                                    |"IDENTIFIER" -> 
                                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                    |_->failwith("incorrect do")
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | "(" -> 
                                        writer.WriteLine("<symbol> ( </symbol>")
                                        Console.WriteLine("<symbol> ( </symbol>")
                                    |_ -> failwith("incorrect do")
                                tokenizer.advance()
                                writer.WriteLine("<expressionList>")
                                Console.WriteLine("<expressionList>")
                                this.compileExpressionList()
                                writer.WriteLine("</expressionList>")
                                Console.WriteLine("</expressionList>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect do")
                                tokenizer.advance()
                                match tokenizer.symbol() with
                                    | ";" -> 
                                        writer.WriteLine("<symbol> ; </symbol>")
                                        Console.WriteLine("<symbol> ; </symbol>")
                                    |_ -> failwith("incorrect do")                                 
                            |_ -> failwith("Incorrect do")
                |_ -> failwith("Incorrect do")
            tokenizer.advance()
                


        member this.compileReturn()=
            writer.WriteLine("<keyword> return </keyword>")
            Console.WriteLine("<keyword> return </keyword>")
            tokenizer.advance()
            match tokenizer.tokenType() with
                |"INT_CONSTANT" | "STRING_CONST" | "IDENTIFIER" -> 
                    writer.WriteLine("<expression>")
                    Console.WriteLine("<expression>")
                    this.compileExpression()
                    writer.WriteLine("</expression>")
                    Console.WriteLine("</expression>")
                | "KEYWORD" ->
                    match tokenizer.keyWord() with
                        |"TRUE" |"FALSE"|"NULL" |"THIS" ->     
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                        |_->()
                | "SYMBOL" ->
                    match tokenizer.symbol() with
                        |"(" | "-" | "~" -> 
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                        |_->()
                | _->()
            match tokenizer.tokenType() with
                |"SYMBOL" ->
                    match tokenizer.symbol() with
                        | ";" ->  
                            writer.WriteLine("<symbol> ; </symbol>")
                            Console.WriteLine("<symbol> ; </symbol>")
                        |_ -> failwith("incorrect return")
                |_ -> failwith("incorrect return")
            tokenizer.advance()



        member this.compileExpression()=
            let mutable stop = true
            writer.WriteLine("<term>")
            Console.WriteLine("<term>")
            this.compileTerm()
            writer.WriteLine("</term>")
            Console.WriteLine("</term>")
            while tokenizer.tokenType() = "SYMBOL" && stop do
                match tokenizer.symbol() with 
                    | "+" | "-" |"*" | "/" | "&amp;" |"|"|"&lt;"|"&gt;"|"=" ->
                        writer.WriteLine("<symbol> " + tokenizer.symbol() + " </symbol>")
                        Console.WriteLine("<symbol> " + tokenizer.symbol() + " </symbol>")
                        tokenizer.advance()
                        writer.WriteLine("<term>")
                        Console.WriteLine("<term>")
                        this.compileTerm()
                        writer.WriteLine("</term>")
                        Console.WriteLine("</term>")
                    |_->
                        stop <- false
                   


        member this.compileTerm() =
            let rec termRec() =
                match tokenizer.tokenType() with
                    |"INT_CONST" -> 
                        writer.WriteLine("<integerConstant> " + string(tokenizer.intVal()) + " </integerConstant>")
                        Console.WriteLine("<integerConstant> " + string(tokenizer.intVal()) + " </integerConstant>")
                        tokenizer.advance()
                    | "STRING_CONST" -> 
                        writer.WriteLine("<stringConstant> " + string(tokenizer.stringVal()) + " </stringConstant>")
                        Console.WriteLine("<stringConstant> " + string(tokenizer.stringVal()) + " </stringConstant>")
                        tokenizer.advance()
                    | "KEYWORD" ->
                        match tokenizer.keyWord() with
                            |"TRUE" -> 
                                writer.WriteLine("<keyword> true </keyword>")
                                Console.WriteLine("<keyword> true </keyword>")
                                tokenizer.advance()
                            |"FALSE" -> 
                                writer.WriteLine("<keyword> false </keyword>")
                                Console.WriteLine("<keyword> false </keyword>")
                                tokenizer.advance()
                            |"NULL" -> 
                                writer.WriteLine("<keyword> null </keyword>")
                                Console.WriteLine("<keyword> null </keyword>")
                                tokenizer.advance()
                            |"THIS" -> 
                                writer.WriteLine("<keyword> this </keyword>")
                                Console.WriteLine("<keyword> this </keyword>")
                                tokenizer.advance()
                            |_ -> failwith("Incorrect term")
                    |"IDENTIFIER" -> 
                        writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                        tokenizer.advance()
                        if tokenizer.tokenType() = "SYMBOL" then
                            match tokenizer.symbol() with
                                | "[" -> 
                                    writer.WriteLine("<symbol> [ </symbol>")
                                    Console.WriteLine("<symbol> [ </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    match tokenizer.symbol() with
                                        | "]" -> 
                                            writer.WriteLine("<symbol> ] </symbol>")
                                            Console.WriteLine("<symbol> ] </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                | "(" -> 
                                    writer.WriteLine("<symbol> ( </symbol>")
                                    Console.WriteLine("<symbol> ( </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expressionList>")
                                    Console.WriteLine("<expressionList>")
                                    this.compileExpressionList()
                                    writer.WriteLine("</expressionList>")
                                    Console.WriteLine("</expressionList>")
                                    match tokenizer.symbol() with
                                        | ")" -> 
                                            writer.WriteLine("<symbol> ) </symbol>")
                                            Console.WriteLine("<symbol> ) </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                | "." ->
                                    writer.WriteLine("<symbol> . </symbol>")
                                    Console.WriteLine("<symbol> . </symbol>")
                                    tokenizer.advance()
                                    match tokenizer.tokenType() with
                                        |"IDENTIFIER" -> 
                                            writer.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                            Console.WriteLine("<identifier> " + tokenizer.identifier() + " </identifier>")
                                        |_->failwith("incorrect term")
                                    tokenizer.advance()
                                    match tokenizer.symbol() with
                                        | "(" -> 
                                            writer.WriteLine("<symbol> ( </symbol>")
                                            Console.WriteLine("<symbol> ( </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()
                                    writer.WriteLine("<expressionList>")
                                    Console.WriteLine("<expressionList>")
                                    this.compileExpressionList()
                                    writer.WriteLine("</expressionList>")
                                    Console.WriteLine("</expressionList>")
                                    match tokenizer.symbol() with
                                        | ")" -> 
                                            writer.WriteLine("<symbol> ) </symbol>")
                                            Console.WriteLine("<symbol> ) </symbol>")
                                        |_ -> failwith("incorrect term")
                                    tokenizer.advance()

                                |_ -> ()
                    |"SYMBOL" ->
                        match tokenizer.symbol() with
                            |"(" -> 
                                writer.WriteLine("<symbol> ( </symbol>")
                                Console.WriteLine("<symbol> ( </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<expression>")
                                Console.WriteLine("<expression>")
                                this.compileExpression()
                                writer.WriteLine("</expression>")
                                Console.WriteLine("</expression>")
                                match tokenizer.symbol() with
                                    | ")" -> 
                                        writer.WriteLine("<symbol> ) </symbol>")
                                        Console.WriteLine("<symbol> ) </symbol>")
                                    |_ -> failwith("incorrect term")
                                tokenizer.advance()
                            |"-" ->
                                writer.WriteLine("<symbol> - </symbol>")
                                Console.WriteLine("<symbol> - </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<term>")
                                Console.WriteLine("<term>")
                                termRec()
                                writer.WriteLine("</term>")
                                Console.WriteLine("</term>")
                            |"~" ->
                                writer.WriteLine("<symbol> ~ </symbol>")
                                Console.WriteLine("<symbol> ~ </symbol>")
                                tokenizer.advance()
                                writer.WriteLine("<term>")
                                Console.WriteLine("<term>")
                                termRec()
                                writer.WriteLine("</term>")
                                Console.WriteLine("</term>")
                            |_ -> failwith("Incorrect term")
                    |_-> failwith("Incorrect term")
            termRec()
        
        
        member this.compileExpressionList() : int =
            let mutable counter = 0
            let mutable stop = true
            if tokenizer.tokenType() = "INT_CONST" || tokenizer.tokenType() = "STRING_CONST" || tokenizer.tokenType() = "IDENTIFIER" then 
                writer.WriteLine("<expression>")
                Console.WriteLine("<expression>")
                this.compileExpression()
                writer.WriteLine("</expression>")
                Console.WriteLine("</expression>")
                while tokenizer.tokenType() = "SYMBOL" & stop do
                    match tokenizer.symbol() with
                        | "," -> 
                            writer.WriteLine("<symbol> , </symbol>")
                            Console.WriteLine("<symbol> , </symbol>")
                            tokenizer.advance()
                            writer.WriteLine("<expression>")
                            Console.WriteLine("<expression>")
                            this.compileExpression()
                            writer.WriteLine("</expression>")
                            Console.WriteLine("</expression>")
                            counter <- counter + 1
                        |_ -> stop <- false
            if tokenizer.tokenType() = "KEYWORD" then 
                match tokenizer.keyWord() with
                    | "TRUE" | "FALSE" | "NULL" | "THIS" ->
                        writer.WriteLine("<expression>")
                        Console.WriteLine("<expression>")
                        this.compileExpression()
                        writer.WriteLine("</expression>")
                        Console.WriteLine("</expression>")
                        while tokenizer.tokenType() = "SYMBOL" & stop do
                            match tokenizer.symbol() with
                                | "," -> 
                                    writer.WriteLine("<symbol> , </symbol>")
                                    Console.WriteLine("<symbol> , </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    counter <- counter + 1
                                |_ -> stop <- false
                    |_-> ()
            if tokenizer.tokenType() = "SYMBOL" then 
                match tokenizer.symbol() with
                    | "~" | "-" | "("  ->
                        writer.WriteLine("<expression>")
                        Console.WriteLine("<expression>")
                        this.compileExpression()
                        writer.WriteLine("</expression>")
                        Console.WriteLine("</expression>")
                        while tokenizer.tokenType() = "SYMBOL" & stop do
                            match tokenizer.symbol() with
                                | "," -> 
                                    writer.WriteLine("<symbol> , </symbol>")
                                    Console.WriteLine("<symbol> , </symbol>")
                                    tokenizer.advance()
                                    writer.WriteLine("<expression>")
                                    Console.WriteLine("<expression>")
                                    this.compileExpression()
                                    writer.WriteLine("</expression>")
                                    Console.WriteLine("</expression>")
                                    counter <- counter + 1
                                |_ -> stop <- false
                    |_-> ()
            counter



let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")
let FileName(fpath:string) = Path.GetFileNameWithoutExtension(fpath)    


type jackAnalyzer() =
    member this.analyze() =
        let files = Directory.GetFiles(INPUT_DIR, "*.jack")
        let XmlWriter(outputDir:string, className:string, tokens:List<Token>) =
            let xmlFilePath = Path.Combine(outputDir, $"{className}Tokenized.xml")
            use xmlFile = new StreamWriter(xmlFilePath)
            xmlFile.WriteLine("<tokens>")
            for token in tokens do
                xmlFile.Write("\t")
                xmlFile.WriteLine($"<{token.Type}> {token.Lexeme} </{token.Type}>")
            xmlFile.WriteLine("</tokens>")
            xmlFile.Flush()
        
        for file in files do
            let className = FileName(file)
            XmlWriter(OUTPUT_DIR, className, Tokenizer(file).Tokenize()) 
            CompilationEngine(file, $"{OUTPUT_DIR}{className}.xml").compileClass()
            printfn $"End of input file: {file}"  
        
        printfn "Output files are ready"



//[<EntryPoint>]
let main _ =
    let analyzer = new jackAnalyzer()
    analyzer.analyze()
    0

                    
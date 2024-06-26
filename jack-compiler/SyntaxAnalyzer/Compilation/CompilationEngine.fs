module CompilationEngine
open Tokenizer
open System.IO
open System.Collections.Generic 


let rep(times,text) = 
    String.replicate times text

type CompilationEngine(inputFile: string, outputFile: string) =
    let _writer = new StreamWriter(outputFile)
    let _tokenizer = new Tokenizer(inputFile)
    let _indentLevel = ref 0

    member _.Write(str: string) =   
        let mutable indents = ""
        match str with 
            | "<class>" 
            | "<classVarDec>"
            | "<subroutineDec>"
            | "<subroutineBody>"
            | "<subroutineCall>"
            | "<varDec>"
            | "<parameterList>"
            | "<statements>"
            | "<letStatement>"
            | "<doStatement>"
            | "<whileStatement>"
            | "<ifStatement>"
            | "<returnStatement>"
            | "<expression>"
            | "<term>" -> 
                indents <- String.replicate _indentLevel.Value " "   
                _indentLevel += 3
            | "</class>"
            | "</classVarDec>"
            | "</subroutineDec>"
            | "</subroutineBody>"
            | "</subroutineCall>"
            | "</varDec>"
            | "</parameterList>"
            | "</statements>"
            | "</letStatement>"
            | "</doStatement>"
            | "</whileStatement>"
            | "</ifStatement>"
            | "</returnStatement>"
            | "</expression>"
            | "</term>" ->  
                _indentLevel -= 3
                indents <- String.replicate _indentLevel.Value " "   
            | _ -> 
                indents <- String.replicate _indentLevel.Value " " 

        _writer.WriteLine(indents + str)
        printfn $"{indents}{str}"

    member this.compileClass() =
        this.Write("<class>")

        _tokenizer.advance()
        match _tokenizer.keyword() with
            | "CLASS" -> this.Write("<keyword> class </keyword>")
            | _ -> failwith("Incorrect compileClass")

        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect compileClass")

        _tokenizer.advance()
        match _tokenizer.symbol() with
            | "{" -> this.Write("<symbol> { </symbol>")
            | _ -> failwith("Incorrect compileClass")

        _tokenizer.advance()
        while List.contains (_tokenizer.keyword()) ["STATIC"; "FIELD"] do
            this.Write("<classVarDec>")
            this.compileClassVarDec()
            this.Write("</classVarDec>")
            _tokenizer.advance()
  
        while List.contains (_tokenizer.keyword()) ["CONSTRUCTOR"; "FUNCTION"; "METHOD"] do
            this.Write("<subroutineDec>")
            this.compileSubroutine()
            this.Write("</subroutineDec>")
            _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "}" -> this.Write("<symbol> } </symbol>")
            | _ -> failwith("Incorrect compileClass")
        this.Write("</class>")
        
        _writer.Flush()
        _writer.Close()

    member this.compileClassVarDec() =
        if _tokenizer.keyword() = "STATIC" then
            this.Write("<keyword> static </keyword>")
        elif _tokenizer.keyword() = "FIELD" then
            this.Write("<keyword> field </keyword>")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "KEYWORD" ->
                match _tokenizer.keyword() with
                    | "INT" -> this.Write("<keyword> int </keyword>")
                    | "CHAR" -> this.Write("<keyword> char </keyword>")
                    | "BOOLEAN" -> this.Write("<keyword> boolean </keyword>")
                    | _ -> failwith("Incorrect ClassVarDec")
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect ClassVarDec")
       
        _tokenizer.advance()
        if _tokenizer.symbol() = "," then
            while _tokenizer.symbol() = "," do
                this.Write("<symbol> , </symbol>")
                _tokenizer.advance()
                match _tokenizer.tokenType() with
                    | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                    | _ -> failwith("Incorrect ClassVarDec")
                _tokenizer.advance()
        
        match _tokenizer.symbol() with
            |";" ->
                this.Write("<symbol> ; </symbol>")
            | _ -> failwith("Incorrect ClassVarDec")
    
    member this.compileSubroutine() =
        match _tokenizer.keyword() with
            |"CONSTRUCTOR" -> this.Write("<keyword> constructor </keyword>")
            |"METHOD" -> this.Write("<keyword> method </keyword>")
            |"FUNCTION" -> this.Write("<keyword> function </keyword>")
            | _ -> failwith("Incorrect SubRoutine")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "KEYWORD" ->
                match _tokenizer.keyword() with
                    | "VOID" -> this.Write("<keyword> void </keyword>")
                    | "INT" -> this.Write("<keyword> int </keyword>")
                    | "CHAR" -> this.Write("<keyword> char </keyword>")
                    | "BOOLEAN" -> this.Write("<keyword> boolean </keyword>")
                    | _ -> failwith("Incorrect SubRoutine")
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect SubRoutine")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect SubRoutine")
        
        _tokenizer.advance()
        match _tokenizer.symbol() with
            | "(" -> this.Write("<symbol> ( </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        
        _tokenizer.advance()
        this.Write("<parameterList>")
        this.compileParameterList()
        this.Write("</parameterList>")
        
        match _tokenizer.symbol() with
            | ")" -> this.Write("<symbol> ) </symbol>")
            | _ -> failwith("Incorrect SubRoutine")
        
        _tokenizer.advance()
        this.Write("<subroutineBody>")
        this.compileSubroutineBody()
        this.Write("</subroutineBody>")
        
    member this.compileParameterList()=
        if List.contains (_tokenizer.tokenType()) ["IDENTIFIER"; "VOID"; "INT"; "CHAR"; "BOOLEAN"] then
            match _tokenizer.tokenType() with
            | "KEYWORD" ->
                match _tokenizer.keyword() with
                    | "VOID" -> this.Write("<keyword> void </keyword>")
                    | "INT" -> this.Write("<keyword> int </keyword>")
                    | "CHAR" -> this.Write("<keyword> char </keyword>")
                    | "BOOLEAN" -> this.Write("<keyword> boolean </keyword>")
                    | _ -> failwith("Incorrect paramList")
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            
            _tokenizer.advance()
            match _tokenizer.tokenType() with
                | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                | _ -> failwith("Incorrect ParamList")
            
            _tokenizer.advance()
            while _tokenizer.symbol() = "," do
                this.Write("<symbol> , </symbol>")
                
                _tokenizer.advance()
                match _tokenizer.tokenType() with
                    | "KEYWORD" ->
                        match _tokenizer.keyword() with
                            | "VOID" -> this.Write("<keyword> void </keyword>")
                            | "INT" -> this.Write("<keyword> int </keyword>")
                            | "CHAR" -> this.Write("<keyword> char </keyword>")
                            | "BOOLEAN" -> this.Write("<keyword> boolean </keyword>")
                            | _ -> failwith("Incorrect paramList")
                    | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                    |_ -> failwith("Incorrect paramList")
                    
                _tokenizer.advance()
                match _tokenizer.tokenType() with
                    | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                    | _ -> failwith("Incorrect ParamList")
                _tokenizer.advance()
      
    member this.compileSubroutineBody()=
        match _tokenizer.symbol() with
            | "{" -> this.Write("<symbol> { </symbol>")
            | _ -> failwith("Incorrect subRoutineBody")
        
        _tokenizer.advance()
        while _tokenizer.keyword() = "VAR" do
            this.Write("<varDec>")
            this.compileVarDec()
            this.Write("</varDec>")
        
        this.Write("<statements>")
        this.compileStatements()
        this.Write("</statements>")
        
        match _tokenizer.symbol() with
            | "}" -> this.Write("<symbol> } </symbol>")
            | _ -> failwith("Incorrect subRoutineBody")

    member this.compileVarDec() =
        this.Write("<keyword> var </keyword>")
        _tokenizer.advance()

        match _tokenizer.tokenType() with
            | "KEYWORD" ->
                    match _tokenizer.keyword() with
                        | "INT" -> this.Write("<keyword> int </keyword>")
                        | "CHAR" -> this.Write("<keyword> char </keyword>")
                        | "BOOLEAN" -> this.Write("<keyword> boolean </keyword>")
                        | _ -> failwith("Incorrect varDec")
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect varDec")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect varDec")
        
        _tokenizer.advance()
        while _tokenizer.symbol() = "," do
            this.Write("<symbol> , </symbol>")
            _tokenizer.advance()
            match _tokenizer.tokenType() with
                | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                | _ -> failwith("Incorrect varDec")
            _tokenizer.advance()
        
        match _tokenizer.tokenType() with
            | "SYMBOL" ->
                match _tokenizer.symbol() with
                | ";" -> this.Write("<symbol> ; </symbol>")
                | _ -> failwith("Incorrect varDec")
            | _ -> failwith("Incorrect varDec")
        _tokenizer.advance()


    member this.compileStatements() =
        if List.contains (_tokenizer.keyword()) ["LET"; "IF"; "WHILE"; "DO"; "RETURN"] then
            while List.contains (_tokenizer.keyword()) ["LET"; "IF"; "WHILE"; "DO"; "RETURN"]  do
                match _tokenizer.keyword() with
                    | "LET" ->
                        this.Write("<letStatement>")
                        this.compileLet()
                        this.Write("</letStatement>")
                    | "IF" ->
                        this.Write("<ifStatement>")
                        this.compileIf()
                        this.Write("</ifStatement>")
                    | "WHILE" ->
                        this.Write("<whileStatement>")
                        this.compileWhile()
                        this.Write("</whileStatement>")
                    | "DO" ->
                        this.Write("<doStatement>")
                        this.compileDo()
                        this.Write("</doStatement>")
                    | "RETURN" ->
                        this.Write("<returnStatement>")
                        this.compileReturn()
                        this.Write("</returnStatement>")
                    // TODO: may need to check for errors?
        else ()

    member this.compileLet() =
        this.Write("<keyword> let </keyword>")
        
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
            | _ -> failwith("Incorrect let")
        
        _tokenizer.advance()
        if _tokenizer.symbol() = "[" then
            this.Write("<symbol> [ </symbol>")
            _tokenizer.advance()
            this.Write("<expression>")
            this.compileExpression()
            this.Write("</expression>")
            match _tokenizer.symbol() with
                | "]" -> this.Write("<symbol> ] </symbol>")
                | _ -> failwith("Incorrect let")
            _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "=" -> this.Write("<symbol> = </symbol>")
            | _ -> failwith("Incorrect let")
        
        _tokenizer.advance()
        
        this.Write("<expression>")
        this.compileExpression()
        this.Write("</expression>")
        
        match _tokenizer.symbol() with
            | ";" -> this.Write("<symbol> ; </symbol>")
            | _ -> failwith("Incorrect let")
        
        _tokenizer.advance()

    member this.compileIf() =
        this.Write("<keyword> if </keyword>")
        
        _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "(" -> this.Write("<symbol> ( </symbol>")
            |_ -> failwith("Incorrect if")
        
        _tokenizer.advance()
        
        this.Write("<expression>")
        this.compileExpression()
        this.Write("</expression>")
        
        match _tokenizer.symbol() with
            | ")" -> this.Write("<symbol> ) </symbol>")
            | _ -> failwith("Incorrect if")
        
        _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "{" -> this.Write("<symbol> { </symbol>")
            | _ -> failwith("Incorrect if")
        
        _tokenizer.advance()
        
        this.Write("<statements>")
        this.compileStatements()
        this.Write("</statements>")
        
        match _tokenizer.symbol() with
            | "}" -> this.Write("<symbol> } </symbol>")
            | _ -> failwith("Incorrect if")
        
        _tokenizer.advance()
        
        if _tokenizer.keyword() = "ELSE" then
            this.Write("<keyword> else </keyword>")
            
            _tokenizer.advance()
            
            match _tokenizer.symbol() with
                | "{" -> this.Write("<symbol> { </symbol>")
                | _ -> failwith("Incorrect if")
            
            _tokenizer.advance()
            
            this.Write("<statements>")
            this.compileStatements()
            this.Write("</statements>")
            
            match _tokenizer.symbol() with
                | "}" -> this.Write("<symbol> } </symbol>")
                | _ -> failwith("Incorrect if")
            
            _tokenizer.advance()
        

    member this.compileWhile() =
        this.Write("<keyword> while </keyword>")
        
        _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "(" -> this.Write("<symbol> ( </symbol>")
            | _ -> failwith("Incorrect while")
        
        _tokenizer.advance()
        
        this.Write("<expression>")
        this.compileExpression()
        this.Write("</expression>")
        
        match _tokenizer.symbol() with
            | ")" -> this.Write("<symbol> ) </symbol>")
            | _ -> failwith("Incorrect while")
        
        _tokenizer.advance()
        
        match _tokenizer.symbol() with
            | "{" -> this.Write("<symbol> { </symbol>")
            | _ -> failwith("Incorrect while")
        
        _tokenizer.advance()
        
        this.Write("<statements>")
        this.compileStatements()
        this.Write("</statements>")

        match _tokenizer.symbol() with
            | "}" -> this.Write("<symbol> } </symbol>")
            | _ -> failwith("Incorrect while")
       
        _tokenizer.advance()

        
    member this.compileDo() =
        this.Write("<keyword> do </keyword>")
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "IDENTIFIER" -> 
                this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                _tokenizer.advance()
                if _tokenizer.tokenType() = "SYMBOL" then
                    match _tokenizer.symbol() with
                        | "(" ->
                            this.Write("<symbol> ( </symbol>")
                            _tokenizer.advance()
                            
                            this.Write("<expressionList>")
                            this.compileExpressionList()
                            this.Write("</expressionList>")

                            match _tokenizer.symbol() with
                                | ")" -> this.Write("<symbol> ) </symbol>")
                                | _ -> failwith("incorrect do")
                            
                            _tokenizer.advance()
                            match _tokenizer.symbol() with
                                | ";" -> this.Write("<symbol> ; </symbol>")
                                | _ -> failwith("incorrect do")
                        | "." ->
                            this.Write("<symbol> . </symbol>")
                            _tokenizer.advance()
                            
                            match _tokenizer.tokenType() with
                                | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                                | _ -> failwith("incorrect do")
                            
                            _tokenizer.advance()
                            match _tokenizer.symbol() with
                                | "(" -> this.Write("<symbol> ( </symbol>")
                                | _ -> failwith("incorrect do")
                            
                            _tokenizer.advance() 
                            this.Write("<expressionList>")
                            this.compileExpressionList()
                            this.Write("</expressionList>")

                            match _tokenizer.symbol() with
                                | ")" -> this.Write("<symbol> ) </symbol>")
                                | _ -> failwith("incorrect do")
                            
                            _tokenizer.advance()
                            match _tokenizer.symbol() with
                                | ";" -> this.Write("<symbol> ; </symbol>")
                                | _ -> failwith("incorrect do")                                
                        | _ -> failwith("Incorrect do")
            | _ -> failwith("Incorrect do")
        
        _tokenizer.advance()
                
    member this.compileReturn()=
        this.Write("<keyword> return </keyword>")
        _tokenizer.advance()
        match _tokenizer.tokenType() with
            | "INT_CONSTANT" | "STRING_CONST" | "IDENTIFIER" ->
                this.Write("<expression>")
                this.compileExpression()
                this.Write("</expression>")
            | "KEYWORD" ->
                match _tokenizer.keyword() with
                    | "TRUE" | "FALSE"| "NULL" | "THIS" ->    
                        this.Write("<expression>")
                        this.compileExpression()
                        this.Write("</expression>")
                    | _ -> ()
            | "SYMBOL" ->
                match _tokenizer.symbol() with
                    | "(" | "-" | "~" ->
                        this.Write("<expression>")
                        this.compileExpression()
                        this.Write("</expression>")
                    | _ -> ()
            | _ -> ()
        match _tokenizer.tokenType() with
            |"SYMBOL" ->
                match _tokenizer.symbol() with
                    | ";" -> this.Write("<symbol> ; </symbol>")
                    | _ -> failwith("incorrect return")
            | _ -> failwith("incorrect return")
        _tokenizer.advance()


    member this.compileExpression() =
        this.Write("<term>")
        this.compileTerm()
        this.Write("</term>")

        let mutable stop = false
        while _tokenizer.tokenType() = "SYMBOL" && stop <> true do
            match _tokenizer.symbol() with
                | "+" | "-" | "*" | "/" | "|" |  "=" | "&amp;" | "&lt;" | "&gt;" ->
                    this.Write($"<symbol> {_tokenizer.symbol()} </symbol>")
                    _tokenizer.advance()
                    this.Write("<term>")
                    this.compileTerm()
                    this.Write("</term>")
                | _ -> 
                    stop <- true
    
    member this.compileTerm() =
        let rec termRec() =
            match _tokenizer.tokenType() with
                | "INT_CONST" ->
                    this.Write($"<integerConstant> {string(_tokenizer.intVal())} </integerConstant>")  // TODO: change formatting
                    _tokenizer.advance()
                | "STRING_CONST" ->
                    this.Write($"<stringConstant> {string(_tokenizer.stringVal())} </stringConstant>")  // TODO: change formatting
                    _tokenizer.advance()
                | "KEYWORD" ->
                    match _tokenizer.keyword() with
                        | "TRUE" ->
                            this.Write("<keyword> true </keyword>")
                            _tokenizer.advance()
                        | "FALSE" ->
                            this.Write("<keyword> false </keyword>")
                            _tokenizer.advance()
                        | "NULL" ->
                            this.Write("<keyword> null </keyword>")
                            _tokenizer.advance()
                        | "THIS" ->
                            this.Write("<keyword> this </keyword>")
                            _tokenizer.advance()
                        | _ -> failwith("Incorrect term")
                | "IDENTIFIER" ->
                    this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                    _tokenizer.advance()
                    if _tokenizer.tokenType() = "SYMBOL" then
                        match _tokenizer.symbol() with
                            | "[" ->
                                this.Write("<symbol> [ </symbol>")
                                _tokenizer.advance()
                                this.Write("<expression>")
                                this.compileExpression()
                                this.Write("</expression>")
                                match _tokenizer.symbol() with
                                    | "]" -> this.Write("<symbol> ] </symbol>")
                                    | _ -> failwith("incorrect term")
                                _tokenizer.advance()
                            | "(" ->
                                this.Write("<symbol> ( </symbol>")
                                _tokenizer.advance()
                                this.Write("<expressionList>")
                                this.compileExpressionList()
                                this.Write("</expressionList>")
                                match _tokenizer.symbol() with
                                    | ")" -> this.Write("<symbol> ) </symbol>")
                                    | _ -> failwith("incorrect term")
                                _tokenizer.advance()
                            | "." ->
                                this.Write("<symbol> . </symbol>")
                                _tokenizer.advance()
                                match _tokenizer.tokenType() with
                                    | "IDENTIFIER" -> this.Write($"<identifier> {_tokenizer.identifier()} </identifier>")
                                    | _ -> failwith("incorrect term")
                                _tokenizer.advance()
                                match _tokenizer.symbol() with
                                    | "(" -> this.Write("<symbol> ( </symbol>")
                                    | _ -> failwith("incorrect term")
                                _tokenizer.advance()
                                this.Write("<expressionList>")
                                this.compileExpressionList()
                                this.Write("</expressionList>")
                                match _tokenizer.symbol() with
                                    | ")" -> this.Write("<symbol> ) </symbol>")
                                    | _ -> failwith("incorrect term")
                                _tokenizer.advance()
                            | _ -> ()
                | "SYMBOL" ->
                    match _tokenizer.symbol() with
                        | "(" ->
                            this.Write("<symbol> ( </symbol>")
                            _tokenizer.advance()
                            this.Write("<expression>")
                            this.compileExpression()
                            this.Write("</expression>")
                            match _tokenizer.symbol() with
                                | ")" -> this.Write("<symbol> ) </symbol>")
                                | _ -> failwith("incorrect term")
                            _tokenizer.advance()
                        | "-" ->
                            this.Write("<symbol> - </symbol>")
                            _tokenizer.advance()
                            this.Write("<term>")
                            termRec()
                            this.Write("</term>")
                        | "~" ->
                            this.Write("<symbol> ~ </symbol>")
                            _tokenizer.advance()
                            this.Write("<term>")
                            termRec()
                            this.Write("</term>")
                        | _ -> failwith("Incorrect term")
                | _ -> failwith("Incorrect term")
        termRec()

        
    member this.compileExpressionList() : int =
            let mutable counter = 0
            let mutable stop = false
            match _tokenizer.tokenType() with
                | "INT_CONST" | "STRING_CONST" | "IDENTIFIER" ->
                    this.Write("<expression>")
                    this.compileExpression()
                    this.Write("</expression>")
                    while _tokenizer.tokenType() = "SYMBOL" && stop <> true do
                        match _tokenizer.symbol() with
                            | "," ->
                                this.Write("<symbol> , </symbol>")
                                _tokenizer.advance()
                                this.Write("<expression>")
                                this.compileExpression()
                                this.Write("</expression>")
                                counter <- counter + 1
                            | _ -> stop <- true
                | "KEYWORD" ->
                    match _tokenizer.keyword() with
                        | "TRUE" | "FALSE" | "NULL" | "THIS" ->
                            this.Write("<expression>")
                            this.compileExpression()
                            this.Write("</expression>")
                            while _tokenizer.tokenType() = "SYMBOL" && stop <> true do
                                match _tokenizer.symbol() with
                                    | "," ->
                                        this.Write("<symbol> , </symbol>")
                                        _tokenizer.advance()
                                        this.Write("<expression>")
                                        this.compileExpression()
                                        this.Write("</expression>")
                                        counter <- counter + 1
                                    | _ -> stop <- true
                        | _ -> ()
                | "SYMBOL" ->
                    match _tokenizer.symbol() with
                        | "~" | "-" | "("  ->
                            this.Write("<expression>")
                            this.compileExpression()
                            this.Write("</expression>")
                            while _tokenizer.tokenType() = "SYMBOL" && stop <> true do
                                match _tokenizer.symbol() with
                                    | "," ->
                                        this.Write("<symbol> , </symbol>")
                                        _tokenizer.advance()
                                        this.Write("<expression>")
                                        this.compileExpression()
                                        this.Write("</expression>")
                                        counter <- counter + 1
                                    | _ -> stop <- true
                        | _ -> ()
            counter




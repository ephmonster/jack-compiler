module XmlReader
open XElements
open System.IO
open System.Collections.Generic
open Globals

type XmlReader(filepath:string) =
    let _filepath = filepath
    let _reader = new StreamReader(filepath)
    let mutable _peeker = new StreamReader(filepath)
    let mutable _curLine = _reader.ReadLine().Trim()
    let mutable _peekLine = _peeker.ReadLine().Trim()
    let mutable _lineNum = ref 0
    let mutable _peekNum = ref 0
    let mutable _prevNesting = "class"
    let mutable _currNesting = "class"

    /// Increments ``_curLine`` by one
    member _.NextLine() =
        _curLine <- _reader.ReadLine().Trim()
        _lineNum += 1
        printfn $"[read] {_curLine}"

    member _.PeekLine() =
        _peekLine <- _peeker.ReadLine().Trim()
        _peekNum += 1

    member this.Exists(tag:string) =
        _peeker <- new StreamReader(_filepath)
        _peekNum := 0
        while !_peekNum <> !_lineNum do
            this.PeekLine()

        let mutable exists = false
        
        while not(_peeker.EndOfStream) && not(exists) do
            if this.PeekTagName() = tag then
                exists <- true
            else 
                this.PeekLine()
        
        if _peeker.EndOfStream then
            _peeker.Close()
        
        exists
        
    /// Moves ``_curLine`` to first occurence of ``tag`` name
    member this.SkipTo(str) =
        if this.Exists(str) then 
            while this.notEOF() && this.TagName() <> str do
                this.NextLine()
            
    /// Moves ``_curLine`` one line after given ``tag``
    member this.SkipPast(tag) =
        this.SkipTo(tag)
        this.NextLine()
 
    member _.notEOF() =
        not(_reader.EndOfStream)

    member _.PeekTagName() =
        let mutable tag = ""
        let i = ref 0
        let mutable stop = false
   
        if _peekLine[!i] = '<' then 
            while not stop do
                i += 1
                if _peekLine[!i] = '>' then 
                    stop <- true
                else 
                    tag <- tag + string _peekLine[!i]
        tag
    

    /// Extract the ``value`` name from the current line
    /// * Example: ``<keyword> var </keyword>`` -> ``var``
    member _.TagValue() =
        let mutable value = ""
        let i = ref 0
        let mutable char = ' '
         
        char <- _curLine[!i]
        if char = '<' then
            while char <> '>' do
                i += 1
                char <- _curLine[!i]
        i += 1
        char <- _curLine[!i]
        while char <> '<' do
            value <- value + string char
            i += 1
            char <- _curLine[!i]
            
        value.Trim()

    /// Extract the ``tag`` name from the current line
    /// * Example: ``<keyword> var </keyword>`` -> ``keyword``
    member _.TagName() =
        let mutable tag = ""
        let i = ref 0
        let mutable stop = false
      
        if _curLine[!i] = '<' then 
            while not stop do
                i += 1
                if _curLine[!i] = '>' then 
                    stop <- true
                else tag <- tag + string _curLine[!i]
        tag
 
    member this.Deserialize() =
        this.parseClass() 
    
    member this.parseClass() =
        this.SkipTo("identifier")
        let xRoot = XClass(this.TagValue())
        this.SkipPast("symbol")

        while _curLine = "<classVarDec>" do
            this.parseClassVarDec() |> xRoot.VarDecs.AddRange

        while _curLine = "<subroutineDec>" do
            this.parseSubroutineDec() |> xRoot.Subroutines.Add

        xRoot

    member this.parseClassVarDec() =
        this.SkipPast("classVarDec")
        let scope = this.TagValue()
        this.NextLine()
        let varType = this.TagValue()
        this.NextLine()
        let name = this.TagValue()
        this.NextLine()

        let xList = new List<XClassVarDec>()
        new XClassVarDec(scope, varType, name) |> xList.Add  

        if this.TagValue() = "," then
            this.NextLine()
            XClassVarDec(scope, varType, this.TagValue()) |> xList.Add
            this.NextLine()

        this.SkipPast("/classVarDec")
        xList
        

    member this.parseSubroutineDec() =
        this.SkipPast("subroutineDec")
        let routine_type = this.TagValue()
        this.NextLine()
        let return_type = this.TagValue()
        this.NextLine()
        let name = this.TagValue()

        let xRoot = XSubroutineDec(routine_type, return_type, name)
        
        this.SkipTo("parameterList")
        xRoot.Parameters <- this.parseParameterList()
        this.SkipTo("subroutineBody")
        xRoot.Body <- this.parseSubroutineBody()
        this.SkipPast("/subroutineDec")
        xRoot


    member this.parseParameterList() =
        let xList = List<XParameter>()
        this.SkipPast("parameterList")

        // either <keyword> or </paramterList>

        while _curLine <> "</parameterList>" do
            let argType = this.TagValue()
            this.NextLine()
            let argName = this.TagValue()
            this.NextLine()
            XParameter(argType,argName) |> xList.Add
            if this.TagName() = "symbol" && this.TagValue() = "," then
                this.NextLine()
        
        this.SkipPast("/parameterList")
        xList


    member this.parseSubroutineBody() =
        this.SkipPast("symbol")
        let xRoot = XSubroutineBody()

        while _curLine = "<varDec>" do
            this.parseVarDecs() |> xRoot.VarDecs.AddRange
        
        this.parseStatements() |> xRoot.Statements.AddRange     

        this.SkipPast("/subroutineBody")
        xRoot
    

    member this.parseVarDecs() =
        this.SkipPast("keyword")
        let vars = new List<XVarDec>()
        
        let varType = this.TagValue()
        this.NextLine()
        let varName = this.TagValue()
        
        XVarDec(varType,varName) |> vars.Add

        this.NextLine() // expect ';' or ','
        while this.TagValue() = "," do
            this.NextLine()
            XVarDec(varType, this.TagValue()) |> vars.Add
            this.NextLine()

        this.SkipPast("/varDec")
        vars


    member this.parseStatements() =
        this.SkipPast("statements")
        let statements = ResizeArray<XElement>()
        while _curLine <> "</statements>" do
            match this.TagName() with 
                | "letStatement"    ->  this.parseLetStatement() |> statements.Add
                | "ifStatement"     ->  this.parseIfStatement() |> statements.Add
                | "whileStatement"  ->  this.parseWhileStatement() |> statements.Add
                | "doStatement"     ->  this.parseDoStatement() |> statements.Add
                | "returnStatement" ->  this.parseReturnStatement() |> statements.Add
                | _                 ->  failwith("Statement error")
        this.SkipPast("/statements")
        statements
    
    /// ``let`` ``varName`` (``[`` ``expression`` ``]``)? ``=`` ``expression`` ``;``
    member this.parseLetStatement() =
        this.SkipPast("keyword")
        let xRoot = XLetStatement(this.TagValue()) // <identifier>
        this.NextLine()
        let symbol = this.TagValue() // <symbol>
        if symbol = "[" then
            xRoot.Subscript <- this.parseExpression()
                
        this.NextLine()
        xRoot.Assigment <- this.parseExpression()
        this.SkipTo("/letStatement")
        this.NextLine()
        for x:XSubTerm in Xml.Extensions.GetSubterms(xRoot.Assigment) do
            printfn $"{x.Value}"
        xRoot

    /// ``if`` ``(`` ``expression`` ``)`` ``{`` ``statements`` ``}``  
    /// ``else`` ``{`` ``statements`` ``}`` 
    member this.parseIfStatement() =
        let xRoot = XIfStatement()
        this.SkipTo("expression")
        this.NextLine()
        xRoot.IfCondition <- this.parseExpression()      
        this.SkipTo("statements")
        xRoot.IfStatements <- this.parseStatements()
        this.NextLine()
        this.NextLine() 
        if this.TagName() = "keyword" && this.TagValue() = "else" then
            this.NextLine()
            this.NextLine()
            xRoot.ElseStatements <- this.parseStatements()
            this.NextLine()
            this.NextLine() 
        
        this.NextLine() 
        xRoot
        

    member this.parseWhileStatement() =
        this.SkipPast("keyword")
        let xRoot = XWhileStatement()
        
        xRoot.Condition <- this.parseExpression()  
        xRoot.Statements <- this.parseStatements()
        
        this.SkipPast("/whileStatement")
        xRoot

    /// ``do`` ``subroutineName`` ``(`` ``expressionList`` ``)`` \
    /// ``do`` ``objname`` ``.`` ``subroutineName`` ``(`` ``expressionList`` ``)``
    member this.parseDoStatement() =
        this.SkipPast("keyword")

        let identifier = this.TagValue()
        this.NextLine()
        let symbol = this.TagValue()

        let xRoot = XDoStatement()
        
        match symbol with 
            | "(" -> 
                xRoot.SubroutineName <- identifier
                xRoot.Expression <- this.parseExpressionList()
            | "." ->
                xRoot.ObjName <- identifier
                this.NextLine()
                xRoot.SubroutineName <- this.TagValue()
                xRoot.Expression <- this.parseExpressionList()
            | _ -> failwith("Incorrect do statement")
        
        this.SkipPast("/doStatement")
        xRoot


    member this.parseReturnStatement() =
        this.SkipPast("keyword") // skip: <returnStatement>
        let xRoot = XReturnStatement()
 
        if this.TagName() = "expression" then
            xRoot.RetValue <- this.parseExpression()
            
        this.SkipPast("/returnStatement")
        xRoot

    
    /// `EXPLIST` -> `EXP` (**,** `EXP`)* | **ε** \
    /// \
    /// **FIRST**(`EXPLIST`) = {**int**, **string**, **keyword**, **objName**, **(**, **unOp**, **ε**} \
    /// **FOLLOW**(`EXPLIST`) = { **)** }
    member this.parseExpressionList() =    
        this.SkipPast("expressionList")   
        let mutable xRoot = XExpression()
        
        // line may be: </expressionList> or <expression>
        if _curLine = "<expression>" then
            xRoot <- this.parseExpression() 
            
        this.SkipPast("/expressionList")
        xRoot
        

    /// `EXP` -> `TERM` (**op** `TERM`)* \
    /// \
    /// **FIRST**(`EXP`) = {**int**, **string**, **keyword**, **objName**, **(**, **unOp**} \
    /// **FOLLOW**(`EXP`) = {**]**, **,**, **)**, ***$*** }
    member this.parseExpression() =
        this.SkipPast("expression")
         
        let xRoot = XExpression()
        this.parseTerm() |> xRoot.Add
        
        while this.TagName() = "symbol" do
            XSubTerm("symbol", this.TagValue()) |> xRoot.Add
            this.NextLine() // ?
            this.parseTerm() |> xRoot.Add  
                
        this.SkipPast("/expression")
        xRoot


    member this.parseTerm() =   
        this.SkipPast("term")
        let xRoot = XTerm()
        let tagName = this.TagName()
        
        match tagName with 
        | "integerConstant" | "stringConstant" | "keyword" ->
            XSubTerm(tagName, this.TagValue()) |> xRoot.Add
        | "identifier" -> 
            XSubTerm(tagName, this.TagValue()) |> xRoot.Add
            this.NextLine()
            if this.TagName() = "symbol" then
                XSubTerm("symbol", this.TagValue()) |> xRoot.Add
                match this.TagValue() with
                | "[" -> 
                    this.parseExpression() |> xRoot.Add
                | "." -> 
                    this.NextLine()
                    XSubTerm(this.TagName(), this.TagValue()) |> xRoot.Add // identifier
                    this.NextLine()
                    XSubTerm(this.TagName(), this.TagValue()) |> xRoot.Add // '('
                    this.parseExpressionList() |> xRoot.Add
                | "(" ->
                    this.parseExpressionList() |> xRoot.Add
                | _ -> ()
                XSubTerm("symbol", this.TagValue()) |> xRoot.Add // ')' or ']'
        | "symbol" -> 
            match this.TagValue() with
            | "("  ->
                XSubTerm(tagName, "(") |> xRoot.Add
                this.parseExpressionList() |> xRoot.Add
                XSubTerm(this.TagName(), this.TagValue()) |> xRoot.Add // ')'
            | "-" | "~" ->
                XSubTerm(tagName, this.TagValue()) |> xRoot.Add
                this.parseTerm() |> xRoot.Add
            | _ -> failwith("Bad symbol")
        this.SkipPast("/term")
        xRoot


    
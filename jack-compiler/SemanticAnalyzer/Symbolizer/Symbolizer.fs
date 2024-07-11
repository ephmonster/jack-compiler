module Symbolizer
open Symbol
open System.Collections.Generic
open XElements
open Globals
open System.IO
open Term

/// Creates a new empty symbol table
type public SymbolTable(xRoot:XClass) as self =
    let _path = $"{OUTPUT_DIR}VM\\{xRoot.Name}.vm"
    let _writer = new StreamWriter(_path)
    let mutable _curFunction = XSubroutineDec()
    let mutable _classTable = new Dictionary<string, Symbol>()
    let mutable _functionTable = new Dictionary<string, Symbol>()  
    let mutable _xRoot = xRoot
    let mutable _className = xRoot.Name
    let _staticIndex = ref 0 
    let _fieldIndex = ref 0 
    let _argIndex = ref 0 
    let _varIndex = ref 0
    let mutable _nesting = ref 0
    let mutable _indent = ""
    let mutable _isIndented = false
    let _code = ref []
    let _lcnt = ref 0
    do 
        for x in _xRoot.VarDecs do
            self.define(x.Name, x.Type, x.Scope)
        self.PrintClassTable()

    member this.ConverToVm() =
        this.Write($"// {_className}.jack\n")
        for x in _xRoot.Subroutines do
            _curFunction <- x
            this.PrintFuncTable()  
            this.compileSubroutine()
        _writer.Close()
 
    member _.PrintClassTable() =
        printfn $"CLASS TABLE: {_className}"
        for sym in _classTable.Values do
            let mutable kind = ""
            if sym.Kind = Field then kind <- "field"
            else kind <- $"{sym.Kind}" 
            printfn $"[{sym.Index,2}] Name: {sym.Name, -20} Type: {sym.Type, -10} Kind: {kind, -10}"
        

    member _.PrintFuncTable() =
        printfn $"METHOD TABLE: {_curFunction.Name}"
        for sym in _functionTable.Values do
            let mutable kind = ""
            if sym.Kind = Field then kind <- "field"
            else kind <- $"{sym.Kind}" 
            printfn $"[{sym.Index,2}] Name: {sym.Name, -20} Type: {sym.Type, -10} Kind: {sym.Kind, -10}"
        

    member _.Write(str:string) =
        if _isIndented then
            _indent <- "    " 
        else _indent <- ""
        
        _writer.WriteLine($"{_indent}{str}")
        printfn $"{_indent}{str}"

        if str.StartsWith("function") then
            _isIndented <- true
        elif str.StartsWith("return") then
            _isIndented <- false
        elif str.Trim() = "" then
            _isIndented <- false
    
    
    member this.Write(lines:list<string>) =
        for line in lines do
            this.Write(line) // calls the above method


    /// Starts a new subroutine scope (i.e. resets the subroutine's symbol table)
    /// The same as ``startSubroutine()``
    member this.compileSubroutine() =
        _argIndex := 0 
        _varIndex := 0 
        _functionTable <- new Dictionary<string,Symbol>()
        
        if _curFunction.Type = "method" then
            this.define("this", _className, "arg")
        
        // Add all `arguments` to the table
        for arg in _curFunction.Parameters do
            this.define(arg.Name, arg.Type, "arg")
        // Add all `var declarations` to the table
        for var in _curFunction.Body.VarDecs do
            this.define(var.Name, var.Type, "local")
        
        // Check argument total
        let arg_count = ref(this.varCount(Arg))
      
        match _curFunction.Type with
        | "method" -> 
            this.Write "// Method"
            this.Write($"function {_className}.{_curFunction.Name} {!arg_count}")
            this.Write ["push argument 0";
                        "pop pointer 0"]
            this.compileStatements(_curFunction.Body.Statements)
        | "function" -> 
            this.Write "// Function"
            this.Write($"function {_className}.{_curFunction.Name} {!arg_count}")
            this.compileStatements(_curFunction.Body.Statements)
        | "constructor" ->
            this.Write "// Constructor"
            this.Write [$"function {_className}.{_curFunction.Name} 0";
                         "// allocating blocks"
                        $"push constant {this.varCount(Field)}";
                         "call Memory.alloc 1";
                         "pop pointer 0"]
            this.compileStatements(_curFunction.Body.Statements)
        
        this.Write ""
            
    
    member this.compileStatements(statements) =
        for s in statements do
            match s.XType with
            | LETSTATEMENT -> // let varName [EXPR]? = EXPR
                this.Write "// let"
                // cast
                let letStatement = s :?> XLetStatement

                // VARNAME
                let varName = letStatement.VarName
                // EXPR
                if letStatement.hasSubscript() then
                    this.compileExpression(letStatement.Subscript)
                // EXPR
                this.compileExpression(letStatement.Assigment)
                // ASSIGN TO VAR
                this.Write($"pop {this.kindOf(varName)} {this.indexOf(varName)}")

            | DOSTATEMENT -> 
                this.Write "// do"
                let doStatement = s :?> XDoStatement
                // EXP
                this.compileExpression(doStatement.Expression)
                let func_exp = XExpression()
                if doStatement.ObjName.Length = 0 then
                    XSubTerm("identifier", doStatement.SubroutineName) |> func_exp.Add 
                else 
                    XSubTerm("identifier", doStatement.ObjName) |> func_exp.Add
                    XSubTerm("symbol",".") |> func_exp.Add
                    XSubTerm("identifier", doStatement.SubroutineName) |> func_exp.Add
                this.compileExpression(func_exp)
                this.Write "pop temp 0"
                
            | IFSTATEMENT -> 
                this.Write "// if"
                let ifStatement = s :?> XIfStatement
                let label1, label2 = $"L{!_lcnt+1}", $"L{!_lcnt+2}" 
                _lcnt += 2
                this.compileExpression(ifStatement.IfCondition)
                this.Write "not"
                this.Write $"if-goto {label1}"
                this.compileStatements(ifStatement.IfStatements)
                this.Write $"goto {label2}"
                this.Write $"label {label1}"
                this.compileStatements(ifStatement.ElseStatements)
                this.Write $"label {label2}"

            | WHILESTATEMENT -> 
                this.Write "// while"
                let whileStatement = s :?> XWhileStatement
                let label1, label2 = $"L{!_lcnt+1}", $"L{!_lcnt+2}" 
                _lcnt += 2
                this.Write $"label {label1}"
                this.compileExpression(whileStatement.Condition)
                this.Write "not"
                this.Write $"if-goto {label2}"
                this.compileStatements(whileStatement.Statements)
                this.Write $"goto {label1}"
                this.Write $"label {label2}"
                
            | RETURNSTATEMENT -> 
                this.Write "// return"
                let returnStatement = s :?> XReturnStatement

                if returnStatement.RetValue.isNull() then
                    this.Write "push constant 0"
                else 
                    this.compileExpression(returnStatement.RetValue)
                this.Write "return"
 

    member this.Execute(stack:byref<List<Term>>, operator:Term) =
        let inline (@=) (a:ref<list<string>>) (b:string) = a := a.Value @ [b]

        for term in stack do
            match term.TermType with
            | EXTERNAL_POINTER -> // push `kind` `index`
                _code @= $"""push {term.Value.Split(" ")[0]} {term.Value.Split(" ")[1]}"""
            | METHOD_POINTER ->  
                _code @= $"push pointer 0" 
            | IDENTIFIER ->
                _code @= $"push {this.kindOf(term.Value)} {this.indexOf(term.Value)}"
            | INTEGER_CONSTANT -> 
                _code @= $"push constant {term.Value}"  
            | STRING_CONSTANT ->
                _code @= $"push string {term.Value}"    // TODO: FIX
            | KEYWORD ->
                if term.Value = "this" then
                    _code @= $"push pointer 0"
                else _code @= $"push {term.Value}"    
            | PROPERTY ->
                _code @= $"push {term.Value}"         // TODO: FIX
            | _ -> ()     

        let args = operator.Args
        
        match operator.TermType with 
        | SYMBOL ->
            match char(operator.Value) with
            | '+' -> _code @= "add"
            | '-' -> _code @= "sub"
            | '*' -> _code @= "call Math.multiply 2"
            | '/' -> _code @= "call Math.divide 2"
            | '&' -> _code @= "and"
            | '|' -> _code @= "or"
            | '<' -> _code @= "lt"
            | '>' -> _code @= "gt"
            | '=' -> _code @= "eq"
            | _ -> ()   
        | UNARY ->
            match char(operator.Value) with
            | '~' -> _code @= "not"
            | '-' -> _code @= "neg"
            | _ -> ()   
        | INTERNAL_METHOD -> _code @= $"call {_className}.{operator.Value} {args}" // Func(..)
        | EXTERNAL_METHOD -> _code @= $"call {operator.Value} {args}" // Obj.Func(..)
        | STATIC_FUNCTION -> _code @= $"call {operator.Value} {args}" // MyClass.Func(..) or OtherClass.Func(..)
        | ASSIGNMENT -> ()

        this.Write(!_code)
        _code := []
        stack.Clear()

    member this.compileExpression(exp:XExpression) =
        let tokens = XmlExtensions.Postfix(exp).Tokens()
        let mutable stack = List<Term>()
         
        for i in 0..tokens.Length - 1 do
            let current:Term = !tokens[i]
            match current.TermType with
            | UNARY | INTERNAL_METHOD | EXTERNAL_METHOD | STATIC_FUNCTION -> // Call is now any of these 3
                this.Execute(&stack, current)
            | INTEGER_CONSTANT | IDENTIFIER | KEYWORD | STRING_CONSTANT | PROPERTY  ->
                current |> stack.Add
                if tokens.Length = 1 then
                    this.Execute(&stack, Term(ASSIGNMENT)) 
            | SYMBOL ->
                let curSymbol = char(current.Value)
                if BinaryOps.Contains(curSymbol) then
                    this.Execute(&stack, current)
                elif curSymbol = '$' then
                    // Find the future function call
                    let args = ref 0
                    let j = ref i
                    let start = ref (!j + 1)
                    let dollars = ref 0 
                    let funcs = ref 0
                    let mutable Break = false
                    let mutable first_comma = true
 
                    while !j < tokens.Length - 1 && not(Break) do
                        j += 1
                        let lookahead = !tokens[!j]
                        if lookahead.Value = "," then
                            if !dollars - !funcs = 0 && first_comma then
                                args += 2
                                first_comma <- false
                            elif !dollars - !funcs = 0 && not(first_comma) then
                                args += 1  
                        elif lookahead.Value = "$" then
                            dollars += 1
                        elif lookahead.isFunc() && (!funcs + 1) > !dollars then
                            Break <- true
                            // if token is not right after starting '$'
                            if !j - !start = 0 || !args = 0 then
                                args := 1
                            // elif args = 0 then
                            //     args := 1
                            let future_call = (!tokens[!j]).Value
                            if not(future_call.Contains(".")) then // Func(x,y)
                                Term(METHOD_POINTER) |> stack.Add
                                tokens[!j].Value <- Term(INTERNAL_METHOD, lookahead.Value, !args)
                            else
                                let obj = future_call.Split(".")[0]
                                if this.kindOf(obj) <> NONE then // Obj.Func(x,y)
                                    Term(EXTERNAL_POINTER, $"{this.kindOf(obj)} {this.indexOf(obj)}") |> stack.Add
                                    tokens[!j].Value <- Term(EXTERNAL_METHOD, lookahead.Value, !args)
                                else 
                                    tokens[!j].Value <- Term(STATIC_FUNCTION, lookahead.Value, !args)
                        elif lookahead.isFunc() then
                            funcs += 1


    /// Defines a new identifier of a given ``name``, ``type``, and ``kind`` and assigns it a running
    /// index. ``STATIC`` and ``FIELD`` identifiers have a class scope, while ``ARG`` and ``VAR`` identifiers
    /// have a subroutine scope
    member _.define(name:string, typ:string, kind:string) =
        match kind with 
        | "static" ->
            _classTable[name] <- Symbol(name, typ, Static, _staticIndex.Value)
            _staticIndex += 1
        | "field" ->
            _classTable[name] <- Symbol(name, typ, Field, _fieldIndex.Value)
            _fieldIndex += 1
        | "arg" ->
            _functionTable[name] <- Symbol(name, typ, Arg, _argIndex.Value)
            _argIndex += 1
        | "local" ->
            _functionTable[name] <- Symbol(name, typ, Local, _varIndex.Value)
            _varIndex += 1
        | _ -> failwith("error")


    /// Returns the number of variables of the given ``kind`` already defined in the current scope
    member _.varCount(kind:SymbolKind) =
        match kind with
        | Static -> _staticIndex.Value
        | Field -> _fieldIndex.Value
        | Arg -> _argIndex.Value
        | Local -> _varIndex.Value
        | NONE -> -1

    /// Returns the ``kind`` of the named identifier in the current scope. If the identifier is unknown, returns
    /// ``NONE``
    member _.kindOf(name:string) : SymbolKind =
        if _functionTable.Keys.Contains(name) then
            _functionTable[name].Kind
        elif _classTable.Keys.Contains(name) then
            _classTable[name].Kind
        else NONE       
    
    /// Returns the ``type`` of the named identifier in the current scope
    member _.typeOf(name:string) : string =
        if _functionTable.Keys.Contains(name) then
            _functionTable[name].Type.ToString()
        elif _classTable.Keys.Contains(name) then
            _classTable[name].Type.ToString()
        else "NONE"
    
    /// Returns the ``index`` assigned to the named identifier
    member _.indexOf(name:string) =
        if _functionTable.Keys.Contains(name) then
            _functionTable[name].Index
        elif _classTable.Keys.Contains(name) then
            _classTable[name].Index
        else -1    
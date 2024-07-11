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
    let _code = ref []
    let _lcnt = ref 0
    do 
        for x in _xRoot.VarDecs do
            self.define(x.Name, x.Type, x.Scope)
        self.PrintClassTable()

    member this.ConverToVm() =
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
        if str.StartsWith("function") || str.StartsWith("label") then 
            _indent <- ""
        else 
            _indent <- "    " 
        
        _writer.WriteLine($"{_indent}{str}")
        printfn $"{_indent}{str}"
    
    
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
            this.Write($"function {_className}.{_curFunction.Name} {!arg_count}")
            this.Write ["push argument 0";
                        "pop pointer 0"]
            this.compileStatements(_curFunction.Body.Statements)
        | "function" -> 
            this.Write($"function {_className}.{_curFunction.Name} {!arg_count}")
            this.compileStatements(_curFunction.Body.Statements)
        | "constructor" ->
            this.Write [$"function {_className}.{_curFunction.Name} 0";
                        $"push constant {this.varCount(Field)}";
                         "call Memory.alloc 1";
                         "pop pointer 0"]
            this.compileStatements(_curFunction.Body.Statements)
            
    
    member this.compileStatements(statements) =
        for s in statements do
            match s.XType with
            | LETSTATEMENT -> // let varName [EXPR]? = EXPR
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
                this.Write $"lable {label2}"

            | WHILESTATEMENT -> 
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
                let returnStatement = s :?> XReturnStatement

                if returnStatement.RetValue.isNull() then
                    this.Write "push constant 0"
                else 
                    this.compileExpression(returnStatement.RetValue)
                this.Write "return\n"
 

    member this.Execute(stack:byref<List<Term>>, operator:Term) =
        let inline (@=) (a:ref<list<string>>) (b:string) = a := a.Value @ [b]

        for term in stack do
            match term.TermType with
            | ExternalPointer -> // push `kind` `index`
                _code @= $"""push {term.Value.Split(" ")[0]} {term.Value.Split(" ")[1]}"""
            | MethodPointer ->  
                _code @= $"push pointer 0" 
            | Identifier ->
                _code @= $"push {this.kindOf(term.Value)} {this.indexOf(term.Value)}"
            | IntegerConstant -> 
                _code @= $"push constant {term.Value}"  
            | StringConstant ->
                _code @= $"push string {term.Value}"    // TODO: FIX
            | Keyword ->
                if term.Value = "this" then
                    _code @= $"push pointer 0"
                else _code @= $"push {term.Value}"    
            | Property ->
                _code @= $"push {term.Value}"         // TODO: FIX
            | _ -> ()     

        let args = operator.Args
        
        match operator.TermType with 
        | Symbol ->
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
        | Unary ->
            match char(operator.Value) with
            | '~' -> _code @= "not"
            | '-' -> _code @= "neg"
            | _ -> ()   
        | InternalMethod -> _code @= $"call {_className}.{operator.Value} {args+1}" // Func(..)
        | ExternalMethod -> _code @= $"call {operator.Value} {args+1}" // Obj.Func(..)
        | StaticFunction -> _code @= $"call {operator.Value} {args}" // MyClass.Func(..) or OtherClass.Func(..)
        | Assignment -> ()

        this.Write(!_code)
        _code := []
        stack.Clear()

    member this.compileExpression(exp:XExpression) =
        let queue = XmlExtensions.Postfix(exp).Tokens()

        let mutable stack = List<Term>()
        let mutable args = 0

        for idx in 0..queue.Length - 1 do
            let term:Term = !queue[idx]
            match term.TermType with
            | IntegerConstant | Identifier | Keyword | StringConstant | Property  ->
                term |> stack.Add
                if queue.Length = 1 then
                    this.Execute(&stack, Term(Assignment)) 
            | Symbol ->
                let curSymbol = char(term.Value)
                if BinaryOps.Contains(curSymbol) then
                    this.Execute(&stack, term)
                elif curSymbol = '$' then
                    // Find the future function call
                    args <- 0
                    let idj = ref idx
                    let commas = ref 0
                    let dollars = ref 0
                    let funcs = ref 0
                    let mutable Break = false

                    let mutable min = 1

                    if (!queue[!idj+1]).TermType = Call then
                        min <- 0
                 
                    while !idj < queue.Length - 1 && not(Break) do
                        idj += 1
                        let curr = !queue[!idj]
                        if curr.Value = "," then
                            commas += 1
                        elif curr.Value = "$" then
                            dollars += 1
                        elif curr.isFunc() && (!funcs + 1) > !dollars then
                            Break <- true
                            args <- (!commas + 1) - (!dollars) + min
                            let future_call = (!queue[!idj]).Value
                            if not(future_call.Contains(".")) then // Func(x,y)
                                Term(MethodPointer) |> stack.Add
                                queue[!idj].Value <- Term(InternalMethod, curr.Value, args)
                            else
                                let obj = future_call.Split(".")[0]
                                if this.kindOf(obj) <> NONE then // Obj.Func(x,y)
                                    Term(ExternalPointer, $"{this.kindOf(obj)} {this.indexOf(obj)}") |> stack.Add
                                    queue[!idj].Value <- Term(ExternalMethod, curr.Value, args)
                                else 
                                    queue[!idj].Value <- Term(StaticFunction, curr.Value, args)
                        elif curr.isFunc() then
                            funcs += 1
            | Unary ->          
                this.Execute(&stack, term)
            | InternalMethod | ExternalMethod | StaticFunction -> // Call is now any of these 3
                this.Execute(&stack, term)

    

    /// Defines a new identifier of a given ``name``, ``type``, and ``kind`` and assigns it a running
    /// index. ``STATIC`` and ``FIELD`` identifiers have a class scope, while ``ARG`` and ``VAR`` identifiers
    /// have a subroutine scope
    member _.define(name:string, typ:string, kind:string) =
        let mutable index = 0
        let mutable symKind = NONE
        match kind with 
            | "static" ->
                index <- _staticIndex.Value
                symKind <- Static
                _staticIndex += 1
            | "field" ->
                index <- _fieldIndex.Value
                symKind <- Field
                _fieldIndex += 1
            | "arg" ->
                index <- _argIndex.Value
                symKind <- Arg
                _argIndex += 1
            | "local" ->
                index <- _varIndex.Value
                symKind <- Local
                _varIndex += 1
            | _ -> failwith("error")

        let symbol = Symbol.Symbol(name, typ, symKind, index)
        match symKind with 
            | Static | Field -> _classTable[symbol.Name] <- symbol
            | Arg | Local -> _functionTable[symbol.Name] <- symbol
            | _ -> failwith("error")

    /// Returns the number of variables of the given ``kind`` already defined in the current scope
    member _.varCount(kind:SymbolKind) =
        match kind with
            | Static -> _staticIndex.Value
            | Field -> _fieldIndex.Value
            | Arg -> _argIndex.Value
            | Local -> _varIndex.Value
            | _ -> -1

    /// Returns the ``kind`` of the named identifier in the current scope. If the identifier is unknown, returns
    /// ``NONE``
    member _.kindOf(name:string) : SymbolKind =
        let mutable symbol = Symbol.Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _functionTable.Keys.Contains(name) then
            symbol <- _functionTable[name]
        
        symbol.Kind
    
    /// Returns the ``type`` of the named identifier in the current scope
    member _.typeOf(name:string) : string =
        let mutable symbol = Symbol.Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _functionTable.Keys.Contains(name) then
            symbol <- _functionTable[name]
        
        symbol.Type.ToString()
    
    /// Returns the ``index`` assigned to the named identifier
    member _.indexOf(name:string) =
        let mutable symbol = Symbol.Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _functionTable.Keys.Contains(name) then
            symbol <- _functionTable[name]

        symbol.Index
    
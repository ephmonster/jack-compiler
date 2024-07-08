module Symbolizer
open Symbol
open System.Collections.Generic
open XElements
open Globals
open System.IO

let centerText (text: string) (width: int) =
    let spaces = width - text.Length
    let leftPadding = spaces / 2
    let rightPadding = spaces - leftPadding
    sprintf "%s%s%s" (String.replicate leftPadding " ") text (String.replicate rightPadding " ")


/// Creates a new empty symbol table
type public SymbolTable(_xRoot:XClass) as self =
    let mutable _curSubroutine = XSubroutineDec("","","")
    let mutable _curSubNum = ref 0
    let mutable _classTable:Dictionary<string, Symbol> = new Dictionary<string, Symbol>()
    let mutable _subroutineTable:Dictionary<string, Symbol> = new Dictionary<string, Symbol>()  
    let mutable _xRoot = _xRoot
    let mutable _className = _xRoot.Name
    let _path = $"{OUTPUT_DIR}{_xRoot.Name}.vm"
    let _writer = new StreamWriter(_path)
    let mutable _staticIndex = ref 0 
    let mutable _fieldIndex = ref 0 
    let mutable _argIndex = ref 0 
    let mutable _varIndex = ref 0
    let mutable _nesting = ref 0
    let mutable _tabs = ""


    // Constructor to init class symbols
    do 
        for x in _xRoot.VarDecs do
            self.define(x.Name, x.Type, x.Scope)
        self.PrintClassTable()

    member this.ConverToVm() =
        for i in 0.._xRoot.Subroutines.Count do
            this.NextSubroutine()
        _writer.Close()

    member this.NextSubroutine() =
        if _curSubNum.Value >= _xRoot.Subroutines.Count then
            _curSubroutine <- XSubroutineDec("","","")
        else 
            _curSubroutine <- _xRoot.Subroutines[_curSubNum.Value]
            _curSubNum += 1
            this.startSubroutine()
            this.PrintSubTable()   


    member _.PrintClassTable() =
        printfn $"CLASS TABLE: {_className}"
        printfn "┌───────────────┬───────────────┬───────────────┬─────┐"
        printfn "│      Name     │      Type     │      Kind     │  #  │"
        printfn "├───────────────┼───────────────┼───────────────┼─────┤"
        for sym in _classTable.Values do
            printfn $"│\t{sym.Name}\t│\t{sym.Type}\t│\t{sym.Kind}\t│  {sym.Index}  │"
        printfn "└───────────────┴───────────────┴───────────────┴─────┘"

    member _.PrintSubTable() =
        printfn $"METHOD TABLE: {_curSubroutine.Name}"
        printfn "┌───────────────┬───────────────┬───────────────┬─────┐"
        printfn "│      Name     │      Type     │      Kind     │  #  │"
        printfn "├───────────────┼───────────────┼───────────────┼─────┤"
        for sym in _subroutineTable.Values do
            printfn $"│\t{sym.Name}\t│\t{sym.Type}\t│\t{sym.Kind}\t│  {sym.Index}  │"
        printfn "└───────────────┴───────────────┴───────────────┴─────┘"

    member _.Write(str:string) =
        if str.StartsWith("function") then 
            _nesting <- ref 0
            _writer.WriteLine(str)
            _nesting += 1
        else
            _tabs <- String.replicate _nesting.Value "   " 
            _writer.WriteLine($"{_tabs}{str}")


    /// Starts a new subroutine scope (i.e. resets the subroutine's symbol table)
    member this.startSubroutine() =
        _staticIndex <- ref 0 
        _fieldIndex <- ref 0  
        _argIndex <- ref 0 
        _varIndex <- ref 0 
        _subroutineTable <- new Dictionary<string,Symbol>()
        match _curSubroutine.Type with  
            | "method" ->
                this.define("this", _className, "arg")
            | _ -> ()
        
        for arg in _curSubroutine.Parameters do
            this.define(arg.Name, arg.Type, "arg")
        
        for var in _curSubroutine.Body.VarDecs do
            this.define(var.Name, var.Type, "local")
        
        this.Write($"function {_className}.{_curSubroutine.Name} {this.varCount(Arg)}")
        if _curSubroutine.Type = "method" then // TODO : CHECK IF CORRECT
            this.Write("push argument 0")
            this.Write("pop pointer 0")

        this.translateSubroutine()

            
        if _curSubroutine.Type = "method" then // TODO : CHECK IF CORRECT
            this.Write("push 0")
            this.Write("return")
    
    member this.translateSubroutine() =
        let statements = _curSubroutine.Body.Statements
        for s in statements do
            match s.XType with
                | LETSTATEMENT -> 
                    let letStatement = s :?> XLetStatement
                    let varName = letStatement.VarName
                    if letStatement.hasSubscript() then
                        this.handleExpression(letStatement.Subscript)
                    
                    this.handleExpression(letStatement.Assigment)
                    this.Write($"pop {this.kindOf(varName)} {this.indexOf(varName)}")
                    ()
                | DOSTATEMENT _ -> 
                    let statement = s :?> XDoStatement
                    ()
                | IFSTATEMENT _ -> 
                    let statement = s :?> XIfStatement
                    ()
                | WHILESTATEMENT _ -> 
                    let statement = s :?> XWhileStatement
                    ()
                | RETURNSTATEMENT _ -> 
                    let statement = s :?> XReturnStatement
                    ()
        
    member this.handleExpression(exp:XExpression) =
        let x:List<XSubTerm> = Xml.Extensions.GetSubterms(exp)
        for s in x do
            printfn $"<{s.Subtype}.{s.Value}>"

        let lNode = XSubTerm()
        let rNode = XSubTerm()
        let op = XSubTerm()
        
        for s in x do
            printfn $"<{s.Subtype}.{s.Value}>"
            match s.Subtype with
            | "keyword" 
            | "integerConstant"
            | "symbol"
            | ""  -> ()
         

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

        let symbol = Symbol(name, typ, symKind, index)
        match symKind with 
            | Static | Field -> _classTable[symbol.Name] <- symbol
            | Arg | Local -> _subroutineTable[symbol.Name] <- symbol
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
    member _.kindOf(name:string) =
        let mutable symbol = Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _subroutineTable.Keys.Contains(name) then
            symbol <- _subroutineTable[name]
        
        symbol.Kind.ToString().ToLower()
    
    /// Returns the ``type`` of the named identifier in the current scope
    member _.typeOf(name:string) =
        let mutable symbol = Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _subroutineTable.Keys.Contains(name) then
            symbol <- _subroutineTable[name]
        
        symbol.Type.ToString()
    
    /// Returns the ``index`` assigned to the named identifier
    member _.indexOf(name:string) =
        let mutable symbol = Symbol()
        if _classTable.Keys.Contains(name) then
            symbol <- _classTable[name]
        elif _subroutineTable.Keys.Contains(name) then
            symbol <- _subroutineTable[name]

        symbol.Index
    
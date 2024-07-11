module XmlExtensions
open XElements
open System.Collections.Generic
open Globals
open Term


type XmlExtensions() =
    let mutable _terms = List<Term>()
    let _index = ref -1


    member _.PeekTerm() =
        if _index.Value + 1 >= _terms.Count then
            Term(TERM_ERROR,"")
        else _terms[_index.Value + 1]


    member _.NextTerm() =
        if _index.Value + 1 >= _terms.Count then
            Term(TERM_ERROR, "")
        else 
            _index += 1
            _terms[!_index]


    member this.GetSubterms(xelement:XElement) =
        let rec rtraverse(x:XElement) =
            match x with
                | :? XSubTerm as xsubterm ->
                        let mutable termtype = KEYWORD
                        match xsubterm.Subtype with 
                        | "integerConstant" -> termtype <- INTEGER_CONSTANT
                        | "identifier" -> termtype <- IDENTIFIER
                        | "stringConstant" -> termtype <- STRING_CONSTANT
                        | "symbol" -> termtype <- SYMBOL
                        | "keyword" -> termtype <- KEYWORD
                        | _ -> failwith("Err")
                        Term(termtype, xsubterm.Value) |> _terms.Add
                | :? XTerm as xterm ->
                    for elem in xterm.SubElems do
                        rtraverse(elem)
                | :? XExpression as xexpression ->
                    for elem in xexpression.SubElems do
                        rtraverse(elem)
                | _ -> ()
        
        rtraverse(xelement) 

        printfn "Terms: "
        for x in _terms do
            printfn $"{x}"

        let mod_terms = List<Term>()
        let mutable next = Term()

        let rec modify() = 
            while _index.Value < _terms.Count - 1 do
                let x = this.NextTerm()
                match x.TermType with
                | IDENTIFIER ->
                    let mutable func_args = 0
                    // PEEK NEXT 
                    match this.PeekTerm().Value with 
                    | "(" -> // CASE: f(E)$
                        Term(CALL, $"{x.Value}") |> mod_terms.Add // FUNC
                        this.NextTerm() |> mod_terms.Add // LPARENS
                        modify() // EXPR          
                    | "." -> // CASE: x.f(E)$
                        let objName = x.Value
                        next <- this.NextTerm()
                        next <- this.NextTerm()
                        let propName = next.Value             
                        if this.PeekTerm().Value = "(" then
                            Term(CALL, $"{objName}.{propName}") |> mod_terms.Add // FUNC
                            this.NextTerm() |> mod_terms.Add // LPARENS
                            modify() // EXPR
                        else // CASE: x.f$
                            Term(PROPERTY, $"{objName}.{propName}") |> mod_terms.Add
                    | _ -> x |> mod_terms.Add // CASE: x $
                | SYMBOL ->
                    if x.Value = ")" then
                        x |> mod_terms.Add // RPARENS
                    else x |> mod_terms.Add
                | TERM_ERROR -> ()
                | _ -> x |> mod_terms.Add // CASE: Complement
        
        modify()

        printfn "\nModified Terms:"
        for x in mod_terms do
            printf $"`{x.Value}` "
        printfn ""
        mod_terms


/// Reverse Polish Notation (Postfix)
/// * Example: `3 + 4 * 2` -> `3 4 + 2 *`
type Postfix(exp:XExpression) as self =
    let _terms = XmlExtensions().GetSubterms(exp)
    let _index = ref -1
    let mutable tokens:list<ref<Term>> = []
    do 
        let mutable output: list<ref<Term>> = []
        let mutable stack= new Stack<Term>()  

        let printStack() =
            printf "Stack: "
            for x in stack do
                printf $"`{x.Value}` "
            printfn "\r"

        let printOutput() =
            printf "Ouput: "
            for x in output do
                printf $"`{x.Value}` "
            printfn "\r"

        let opPush(term) =
            stack.Push(term)

        let opPop() =
            let popped = stack.Pop()
            popped

        let result_add(term) = 
            output <- output @ [ref<Term>(term)]
        
        let stackEmpty() =
            stack.Count = 0

        let operatorOnStack() = 
            stack.Count > 0 && (stack.Peek().Value |> self.isOperator || stack.Peek().TermType = CALL) && stack.Peek().Value <> "("

        let isLast() =
            _index.Value + 1 = _terms.Count

        let PeekTerm() =
            stack.Peek().Value


        while _index.Value < _terms.Count - 1 do
            let mutable curr:Term = self.NextTerm()
            match curr.TermType with
            | SYMBOL ->
                // LPARENS
                if curr.Value |= ["("; "["] then 
                    curr |> opPush
                // RPARENS
                elif curr.Value |= [")"; "]"] then
                    // PEEK = "("
                    if PeekTerm() |= ["("; "["] then
                        opPop() |> ignore
                    // PEEK != "("
                    if not(stackEmpty()) && PeekTerm() =|= ["("; "["] then
                        // push all operators to result
                        while not(stackEmpty()) && PeekTerm() =|= ["("; "["] do
                            opPop() |> result_add
                        // opPop() |> ignore // remove ")"
                        if not(stackEmpty()) && PeekTerm() =|= ["("; "["] then
                            opPop() |> result_add // function call 
                            if not(stackEmpty()) && not(isLast()) then
                                opPop() |> result_add
                elif curr.Value = "," then
                    if PeekTerm() =|= ["(";"[";] then
                        opPop() |> result_add
                    curr |> result_add
                elif curr.Value |> self.isOperator then 
                    // OPERATOR
                    let mutable op = Term(SYMBOL, curr.Value)
                    if !_index = 0 then 
                        op <- Term(UNARY, curr.Value)
                    if operatorOnStack() then 
                         // ANOTHER OPERATOR
                        if stack.Peek().TermType <> UNARY then
                            op <- Term(UNARY, curr.Value)
                        else opPop() |> result_add
                    op |> opPush
            | KEYWORD | INTEGER_CONSTANT | STRING_CONSTANT | PROPERTY | IDENTIFIER ->
                curr |> result_add
                if operatorOnStack() then
                    opPop() |> result_add
            | CALL ->
                curr |> opPush // FUNCTION
                Term(SYMBOL, "$") |> result_add // ARG SYMBOL
            
            printStack()
            printOutput() 
            printfn ""
                
        while stack.Count > 0 do
            stack.Pop() |> result_add

        printfn "REVERSE POLISH:"
        output |> List.iter (fun term -> printf $"{term.Value} " )
        tokens <- output

    member _.Tokens() =
        tokens

    member _.isOperand(str:string) =
        str.Length <> 1 || not(Ops.Contains(char(str)))
        
    member _.isOperator(str:string) =
        str.Length = 1 && Ops.Contains(char(str))

    member this.isUnary(str:string) =
        str.Length = 1 && this.isOperator(str) && UnaryOps.Contains(char(str))

    member this.isBinary(str:string) =
        not(this.isUnary(str))

    member _.PeekTerm() =
        _terms[_index.Value + 1]

    member _.NextTerm() =
        printfn $"Current: {_terms[_index.Value + 1]}"
        _index += 1
        _terms[!_index]
     
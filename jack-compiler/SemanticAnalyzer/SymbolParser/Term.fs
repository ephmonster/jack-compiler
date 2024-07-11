module Term


type TermType = | IntegerConstant | Identifier | StringConstant | Symbol | Unary | Keyword 
                | Call | Property | InternalMethod | ExternalPointer | ExternalMethod | StaticFunction
                | MethodPointer | Assignment | Error 


type Term(typ:TermType, value:string, args:int) =
    member val TermType = typ with get,set
    member val Value = value with get,set
    member val Args = args with get, set
    new() = Term(Property,"", 0)
    new(typ) = Term(typ, "", 0)
    new(typ, value) = Term(typ, value, 0)
    override this.ToString() =
        $"{this.Value}"

    member this.isFunc() =
        match this.TermType with
        | Call | InternalMethod | ExternalMethod | StaticFunction -> true
        | _ -> false
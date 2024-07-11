module Term


type TermType = | INTEGER_CONSTANT | IDENTIFIER | STRING_CONSTANT | SYMBOL | UNARY | KEYWORD 
                | CALL | PROPERTY | EXTERNAL_POINTER | INTERNAL_METHOD | EXTERNAL_METHOD | STATIC_FUNCTION
                | METHOD_POINTER | ASSIGNMENT | TERM_ERROR 


type Term(typ:TermType, value:string, args:int) =
    member val TermType = typ with get,set
    member val Value = value with get,set
    member val Args = args with get, set
    new() = Term(PROPERTY,"", 0)
    new(typ) = Term(typ, "", 0)
    new(typ, value) = Term(typ, value, 0)
    override this.ToString() =
        $"{this.Value}"

    member this.isFunc() =
        match this.TermType with
        | CALL | INTERNAL_METHOD | EXTERNAL_METHOD | STATIC_FUNCTION -> true
        | _ -> false
module Token
[<StructuredFormatDisplay("<{Type}>{Lexeme}</{Type}>")>]
type Token(typ:string, lex:string) = 
    let mutable _type = typ
    let mutable _lexeme = lex
    member _.Type
        with get() =
            _type
        and set(value) =
            _type <- value
    member _.Lexeme
        with get() =
            _lexeme
        and set(value) =
            _lexeme <- value
    
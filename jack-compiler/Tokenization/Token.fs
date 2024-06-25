module Token
[<StructuredFormatDisplay("<{Type}>{Lexeme}</{Type}>")>]
type public Token(typ:string, lex:string) = 
    member val Type = typ 
    member val Lexeme = lex
    new() = Token(null, null)
    
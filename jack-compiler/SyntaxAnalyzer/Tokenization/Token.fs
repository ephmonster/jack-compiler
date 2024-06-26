module Token
[<StructuredFormatDisplay("<{Type}>{Lexeme}</{Type}>")>]
type public Token(typ:string, lex:string) = 
    let xmlTok typ lex = 
        if typ = "symbol" then
            match lex with 
                | "<" -> "&lt;"
                | ">" -> "&gt;"
                | "&" -> "&amp;"
                | _ -> lex
            else
                lex
    member val Type = typ 
    member val Lexeme = lex
    new() = Token(null, null)
    
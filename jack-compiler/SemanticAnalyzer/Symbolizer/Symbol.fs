module Symbol

type SymbolKind = NONE | Static | Field | Arg | Local

type SymbolKind with
    override this.ToString() =
        match this with
        | Field   -> "this"
        | Static  -> "static"
        | Arg     -> "argument"
        | Local   -> "local"
        | NONE    -> "NONE"


type public Symbol(name:string, typ:string, kind:SymbolKind, index:int) = 
    member val Name = name
    member val Type = typ 
    member val Kind = kind
    member val Index = index
    new() = Symbol(null, null, NONE, -1)
    
    
        
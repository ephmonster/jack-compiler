module Symbol

type SymbolKind = NONE | Static | Field | Arg | Local

type public Symbol(name:string, typ:string, kind:SymbolKind, index:int) = 
    member val Name = name
    member val Type = typ 
    member val Kind = kind
    member val Index = index
    new() = Symbol(null, null, NONE, -1)
 
        
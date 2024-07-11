module XElements
open System.Collections.Generic

type XType = | CLASS | CLASSVARDEC | PARAMETER | VARDEC | SUBROUTINEDEC | SUBROUTINEBODY 
             | SUBTERM | EXPRESSION | TERM | DOSTATEMENT | IFSTATEMENT | WHILESTATEMENT 
             | LETSTATEMENT | RETURNSTATEMENT


type XElement(xtype) =
    member _.XType:XType = xtype

type XClassVarDec(s,t,n) =
    inherit XElement(CLASSVARDEC)
    member val Scope:string = s with get,set
    member val Type:string = t with get,set
    member val Name:string = n with get,set 
    override x.ToString() =
        $"ClassVarDec({x.Scope} {x.Type} {x.Name})"

type XParameter(typ,name) =
    inherit XElement(PARAMETER)
    member val Type:string = typ with get,set
    member val Name:string = name with get,set
    override x.ToString() =
        $"Parameter({x.Type} {x.Name})"

type XVarDec(t,n) =
    inherit XElement(VARDEC)
    member val Type:string = t with get,set
    member val Name:string = n with get,set
    override x.ToString() =
        $"VarDec({x.Type} {x.Name})"

type XSubroutineBody() =
    inherit XElement(SUBROUTINEBODY)
    member val VarDecs = List<XVarDec>()
    member val Statements = List<XElement>()
    override x.ToString() =
        $"SubroutineBody(Vars: {x.VarDecs.Count} Stats: {x.Statements.Count})"

type XSubroutineDec(t,r,n) =
    inherit XElement(SUBROUTINEDEC)
    member val Type:string = t with get,set
    member val RetType:string = r with get,set
    member val Name:string = n with get,set 
    member val Parameters = List<XParameter>() with get,set
    member val Body = XSubroutineBody() with get,set
    override x.ToString() =
        $"SubroutineDec({x.Type} {x.RetType} {x.Name} n={x.Parameters.Count})"
    new() = XSubroutineDec("", "", "")

type XClass(name) = 
    inherit XElement(CLASS)
    member val Name:string = name with get,set 
    member val VarDecs = List<XClassVarDec>()
    member val Subroutines = List<XSubroutineDec>()
    new() = XClass("")
    override x.ToString() =
        $"SubroutineDec({x.Name} Vars: {x.VarDecs.Count} Subs: {x.Subroutines.Count})"

type XSubTerm(subtype, value) =
    inherit XElement(SUBTERM)
    member val Subtype:string = subtype with get,set
    member val Value:string = value with get,set
    new() = XSubTerm("","")
    override x.ToString() =
        $"{x.Subtype.ToUpper()}({x.Value})"

type XExpression() =
    inherit XElement(EXPRESSION)
    member val SubElems = List<XElement>()
    member this.Add(x) =
        this.SubElems.Add x
    override x.ToString() =
        $"Expression(Elems: {x.SubElems.Count})"
    member this.isNull() =
        this.SubElems.Count = 0


type XTerm() =
    inherit XElement(TERM)
    member val SubElems = List<XElement>()
    member this.Add(x) =
        this.SubElems.Add x
    override x.ToString() =
        $"Term(Elems: {x.SubElems.Count})"

type XDoStatement() =
    inherit XElement(DOSTATEMENT)
    member val ObjName = "" with get,set
    member val SubroutineName = "" with get,set
    member val Expression = XExpression() with get, set

type XLetStatement(varName) =
    inherit XElement(LETSTATEMENT)
    member val VarName:string = varName with get,set 
    member val Subscript = XExpression() with get,set
    member val Assigment = XExpression() with get,set
    member this.hasSubscript() =
        this.Subscript.SubElems.Count > 0

type XIfStatement() =
    inherit XElement(IFSTATEMENT)
    member val IfCondition = XExpression() with get,set
    member val IfStatements = List<XElement>() with get,set
    member val ElseStatements = List<XElement>() with get,set

type XWhileStatement() =
    inherit XElement(WHILESTATEMENT)
    member val Condition = XExpression() with get,set
    member val Statements = List<XElement>() with get,set

type XReturnStatement() =
    inherit XElement(RETURNSTATEMENT)
    member val RetValue = XExpression() with get,set

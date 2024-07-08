module TokenNode


// type NodeType = | CLASS | CLASSVARDEC | SUBROUTINEDEC | SUBROUTINEBODY | SUBROUTINECALL
//                 | VARDEC |  PARAMETERLIST | STATEMENTS | LETSTATEMENT | IFSTATEMENT
//                 | ELSESTATEMENT | WHILESTATEMENT | DOSTATEMENT | RETURNSTATEMENT | EXPRESSION 
//                 | TERM | EXPRESSIONLIST | ARGUMENT | UNDEFINED 

// type Scope = | STATIC | FIELD | UNDEFINED

// type Primitive = | INT | CHAR | BOOLEAN | UNDEFINED

// type DecType = | CONSTRUCTOR | FUNCTION | METHOD | UNDEFINED

// type public Node(ntyp:NodeType, scope:Scope, id:string, 
//                  varType:string, retType:string, decType: DecType, nodes:ResizeArray<Node>) =
//     member val Scope = scope with get, set
//     member val NodeType = ntyp with get, set
//     member val Name = id with get, set
//     member val VarType = varType with get, set
//     member val Nodes = nodes with get, set  
//     member val RetType = retType with get, set 
//     member val DecType = decType with get, set
//     member this.AddNode(node:Node) =
//         this.Nodes.Add(node)

//     member this.AddNodes(array:ResizeArray<Node>) =
//         this.Nodes.AddRange(array)

//     new() = Node(NodeType.UNDEFINED, Scope.UNDEFINED, "", "", "", DecType.UNDEFINED, ResizeArray())
//     new(ntyp) = Node(ntyp, Scope.UNDEFINED, "", "", "", DecType.UNDEFINED, ResizeArray())
//     new(ntyp,scope,id,vartype) = Node(ntyp, scope, id, vartype, "", DecType.UNDEFINED, ResizeArray())
 
//     override this.ToString() =
//         let mutable str = $"{this.NodeType}("
//         match this.NodeType with 
//             | CLASS -> str <- str + $"{this.Name}, n={this.Nodes.Count}" 
//             | CLASSVARDEC -> str <- str + $"{this.Name}, {this.Scope}, {this.VarType}" 
//             | ARGUMENT -> str <- str + $"{this.Name}, {this.VarType}"
//             | SUBROUTINEDEC -> str <- str + $"{this.Name}, {this.DecType}, {this.RetType}, args={this.Nodes.Count}"
//             | SUBROUTINEBODY -> str <- str + $"{this.Name}"
//             | VARDEC -> str <- str + $"{this.Name}, {this.VarType}"
//             | _ -> str <- str + $"{this.Scope}, {this.Name}, {this.RetType}, n={this.Nodes.Count}" 
//         str <- str + ")"
//         str

// [<AbstractClass>]
// type Node(name, ntype) =
//     member _.Name: string = name
//     member _.NodeType: NodeType = ntype
//     member _.Nodes: ResizeArray<Node> = ResizeArray()

// type ArgumentNode(name) =
//     inherit Node(name, NodeType.Argument)
//     member val Scope = Scope.UNDEFINED with get,set
//     member val VarType = "" with get,set

// type ClassVarDecNode(name) =
//     inherit Node(name, ClassVarDec)
//     // let mutable name = ""
//     // override _.NodeType 
//     //     with get() = ClassVarDec
//     // override _.Name 
//     //     with get() = name
//     //     and set(value) = 
//     //         name <- value
//     member val Scope = Scope.UNDEFINED with get,set
//     member val VarType = "" with get,set
//     new(scope, name, varType) as this =
//         new ClassVarDecNode(name) then 
//         this.Scope <- scope
//         this.VarType <- varType

// type SubroutineDecNode(name) =
//     inherit Node(name, SubroutineDec)
//     // let mutable name = ""
//     // override _.NodeType 
//     //     with get() = SubroutineDec
//     // override _.Name 
//     //     with get() = name
//     //     and set(value) = 
//     //         name <- value
//     member val SubType = "" with get,set
//     member val RetVal = "" with get,set
//     member this.AddNode(argNode:ArgumentNode) =
//         this.Nodes.Add(argNode)

// type ClassNode(name) =
//     inherit Node(name, NodeType.Class)
//     // let mutable name = ""
//     // override _.NodeType 
//     //     with get() = Class
//     // override _.Name 
//     //     with get() = name
//     //     and set(value) = 
//     //         name <- value
//     member this.AddNodes(decs:ResizeArray<Node>) =
//         this.Nodes.AddRange(decs)
//     // member this.AddSubDec(decs:ResizeArray<SubroutineDecNode>) =
//     //     this.SubroutineDecs.AddRange(decs)
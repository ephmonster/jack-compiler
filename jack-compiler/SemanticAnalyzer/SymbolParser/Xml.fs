module Xml
open XElements
open System.Collections.Generic

type Extensions() = 
    static member GetSubterms(xelement:XElement) =
        let subterms = List<XSubTerm>()
        let rec rtraverse(x:XElement) =
            match x with
                | :? XSubTerm as xsubterm ->
                        subterms.Add xsubterm
                | :? XTerm as xterm ->
                    for elem in xterm.SubElems do
                        rtraverse(elem)
                | :? XExpression as xexpression ->
                    for elem in xexpression.SubElems do
                        rtraverse(elem)
                | _ -> ()
        rtraverse(xelement)
        subterms
module XmlWriter
open Token
open System.IO
open System.Collections.Generic

let XmlWriter(outputDir:string, className:string, tokens:List<Token>) =
    let xmlFile = new StreamWriter(outputDir + $"\\{className}T.xml")
    
    xmlFile.WriteLine      "<tokens>"
    for token in tokens do
        xmlFile.WriteLine $"    <{token.Type}> {token.Lexeme} </{token.Type}>"
    xmlFile.WriteLine      "</tokens>"
    
    xmlFile.Close()

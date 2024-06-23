module XmlWriter
open Token
open System.IO
open System.Collections.Generic

let XmlWriter(outputDir:string, className:string, tokens:List<Token>) =
    let xmlFile = new StreamWriter(outputDir + $"\\{className}T.xml")
    xmlFile.WriteLine("<tokens>")
    for token in tokens do
        xmlFile.Write("\t")
        match token.Type with
            | "symbol" -> 
                match token.Lexeme with
                    | "<" -> xmlFile.WriteLine("<symbol> &lt </symbol>")
                    | ">" -> xmlFile.WriteLine("<symbol> &gt </symbol>")                 
                    | "\"" -> xmlFile.WriteLine("<symbol> &quot </symbol>")
                    | "&" -> xmlFile.WriteLine("<symbol> &amp </symbol>")
                    | _ -> xmlFile.WriteLine($"<symbol> {token.Lexeme} </symbol>")
            | _ -> xmlFile.WriteLine($"<{token.Type}> {token.Lexeme} </{token.Type}>")
    xmlFile.Write("</tokens>")
    xmlFile.Flush()
    xmlFile.Close()

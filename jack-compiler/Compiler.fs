module Compiler
open System.IO
open CompilationEngine
open Globals

[<EntryPoint>]
let main _ =
    let files = Directory.GetFiles(INPUT_DIR, "*.jack")
    for file in files do
        let className = FileName(file)
        CompilationEngine(file, $"{OUTPUT_DIR}{className}.xml").compileClass()
        let root = XmlReader.XmlReader($"{OUTPUT_DIR}{className}.xml").Deserialize()
        let x = Symbolizer.SymbolTable(root)
        x.ConverToVm()

        printfn $"End of input file: {file}"  
    
    printfn "Output files are ready"
    0
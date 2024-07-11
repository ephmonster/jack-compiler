module Compiler
open System.IO
open CompilationEngine
open Globals
open XmlReader
open Symbolizer


[<EntryPoint>]
let main _ =
    for file in Directory.GetFiles(INPUT_DIR, "*.jack") do
        printfn $"Starting analysis of: {file}"  
     
        // Tokenizer and Syntax Analyzer
        CompilationEngine(file, $"{XML_DIR}\\{FileName(file)}.xml").compileClass()
        // Parse Semantic Tokens
        let xRoot = TokenParser($"{XML_DIR}\\{FileName(file)}.xml").Parse()
        // Symbolizer
        SymbolTable(xRoot).ConverToVm()
        
        printfn $"Done analyzing: {file}"  
    0
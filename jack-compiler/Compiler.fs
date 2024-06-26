open System.IO
open XmlWriter
open Tokenizer
open CompilationEngine

let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")
let FileName(fpath:string) = Path.GetFileNameWithoutExtension(fpath)    

[<EntryPoint>]
let main _ =
    let files = Directory.GetFiles(INPUT_DIR, "*.jack")
    for file in files do
        let className = FileName(file)
        XmlWriter(OUTPUT_DIR, className, Tokenizer(file).Tokenize()) 
        CompilationEngine(file, $"{OUTPUT_DIR}{className}.xml").compileClass()
        printfn $"End of input file: {file}"  
    
    printfn "Output files are ready"
    0
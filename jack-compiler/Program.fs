open System.IO
open Tokenizer
open XmlWriter
  
let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")

let FileName(filepath : string) = Path.GetFileNameWithoutExtension(filepath)    

[<EntryPoint>]
let main _ =
    let files = Directory.GetFiles(INPUT_DIR, "*.jack")
    for file in files do
        let className = FileName(file)
        let tokens = Tokenizer(file).Tokenize()
        XmlWriter(OUTPUT_DIR, className, tokens)         
        printfn $"End of input file: {file}"  
    printfn "Output files are ready"
    0
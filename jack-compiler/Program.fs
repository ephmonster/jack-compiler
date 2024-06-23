open System.IO
open Tokenizer
  
let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")

let FileName(filepath : string) = Path.GetFileNameWithoutExtension(filepath)    

[<EntryPoint>]
let main argv =
    let files = Directory.GetFiles(INPUT_DIR, "*.jack")
    for file in files do
        use reader = new StreamReader(file)
        let className = FileName(file)
        let tokenizer = new Tokenizer(file)
        tokenizer.Tokenize()
          
        printf "End of input file: %s\n" file 
    printf "Output files are ready"

    0
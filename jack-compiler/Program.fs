// Learn more about F# at http://fsharp.org

open System
open System.IO
  
let output = new StreamWriter("C:\\Users\\Ephy's Acer\\Downloads\\nand2tetris\\nand2tetris\\projects\\8\\FunctionCalls\\FibonacciElement\\FibonacciElement.asm")
let mutable counter = 0

    

let doNothing()=()

let GetFileName(filepath : string) = Path.GetFileNameWithoutExtension(filepath)
    

[<EntryPoint>]
let main argv =
    printfn "Please enter the path: "
    let path = Console.ReadLine()
    let files = Directory.GetFiles(path, "*.jack")
    counter <- 0
    for file in files do
        use reader =new StreamReader(file)
        let className = GetFileName(file)
        let mutable line = ""
        while not (reader.EndOfStream) do
            line <- reader.ReadLine()
            let words = line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            if words.Length > 0 then
               
        let currentFileName = file
        printf "End of input file: %s\n" currentFileName 
    output.Flush()
    output.Close() 
    printf "Output files are ready"

    0
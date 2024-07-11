module Globals
open System.IO

let Keywords =
    ["class"; "constructor"; "function"; "method"; "field"; 
     "static"; "var"; "int"; "char"; "boolean"; "void"; "true"; 
     "false"; "null"; "this"; "let"; "do"; "if"; "else"; 
     "while"; "return"] |> Set

let Symbols = 
    ['{'; '}'; '('; ')'; '['; ']'; '.'; ','; ';'; '+'; '-'; '–'; // Last one is an EN-Dash
     '*'; '/'; '&'; '|'; '<'; '>'; '='; '~'] |> Set

let Ops = 
    ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '='; '~'] |> Set

let BinaryOps =
    ['+'; '-'; '*'; '/'; '&'; '|'; '<'; '>'; '=';] |> Set

let UnaryOps = 
    ['-'; '~'] |> Set 

let inline (>=<) a (b,c) = a >= b && a <= c
let inline (+=) a b = a := a.Value + b 
let inline (-=) a b = a := a.Value - b

/// `true` if list `b` contains `a`
let inline (|=) (a:string) b = List.contains a b
let inline (=|=) (a:string) b = not(List.contains a b)

let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")
let FileName(fpath:string) = Path.GetFileNameWithoutExtension(fpath)    

let XML_DIR = $"{OUTPUT_DIR}\\XML"


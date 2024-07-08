module Globals
open System.IO

let inline (>=<) a (b,c) = a >= b && a <= c
let inline (+=) a b = a := a.Value + b 
let inline (-=) a b = a := a.Value - b

/// `true` if list `b` contains `a`
let inline (|=) (a:string) b = List.contains a b

let INPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Input\\")
let OUTPUT_DIR = Path.Combine(Directory.GetParent(__SOURCE_DIRECTORY__).FullName, "jack-compiler\\Output\\")
let FileName(fpath:string) = Path.GetFileNameWithoutExtension(fpath)    
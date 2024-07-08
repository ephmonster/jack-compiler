module CompErrors

let CompError(exceptID:string) =
    match exceptID with
    | "0001" -> $"JK0001: Expected ;"
    | _ -> "Compilation error"
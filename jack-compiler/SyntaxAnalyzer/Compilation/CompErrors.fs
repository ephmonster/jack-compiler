module CompErrors

let CompError(exceptID:int) =
    match exceptID with
    | 1 -> "JK0001: Expected `;`"
    | 2 -> "JK0002: Expected `{`"
    | 3 -> "JK0003: Expected closing `}`"
    | 100 -> "JK0100: Class declaration is missing a `class` keyword"
    | 101 -> "JK0101: Class `name` missing from declaration"
    | 102 -> "JK0102: Class var declaration missing `scope` (`field` or `static`)"
    | _ -> "Compilation error"
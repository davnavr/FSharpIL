module internal FSharpIL.Utilities.Result

let get item =
    match item with
    | Ok result -> result
    | Error err -> err.ToString() |> invalidArg "item"

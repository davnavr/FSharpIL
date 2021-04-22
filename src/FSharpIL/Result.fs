[<RequireQualifiedAccess>]
module internal FSharpIL.Result

let inline any (result: Result<'T, 'T>) =
    match result with
    | Ok i
    | Error i -> i

module FSharpIL.Test

open System

let private (|SelectExample|_|) (success: bool, selection: uint32) =
    match selection with
    | _ when not success -> None
    | 0u -> Some HelloWorld.example
    | 1u -> Some CustomCollections.example
    | _ -> None

[<EntryPoint>]
let main _ =
    let input = stdin.ReadLine()

    match UInt32.TryParse input with
    | SelectExample example ->
        System.Diagnostics.Debugger.Break()
        example() |> ignore
        0
    | _ ->
        eprintfn "Invalid input: %s" input
        -1

module FSharpIL.Result

let get =
    function
    | Ok item -> item
    | Error err -> failwithf "%A" err

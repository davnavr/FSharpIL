namespace FSharpIL.Documentation

open System.IO

[<RequireQualifiedAccess>]
module internal Article =
    let write (file: FileInfo) =
        let stream = file.OpenWrite()
        html stream [] [|
            
        |]

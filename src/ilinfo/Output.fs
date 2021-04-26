[<RequireQualifiedAccess>]
module ILInfo.Output

open Microsoft.FSharp.Core.Printf

open FSharpIL.Reading

let console =
    { MetadataReader.empty with
        HandleError =
            fun offset state error _ ->
                eprintfn "Error occured at offset (0x%X) while %s: %s" offset state.Description error.Message }

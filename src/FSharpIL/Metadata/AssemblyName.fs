namespace FSharpIL.Metadata

open System

// II.22.2
type AssemblyName =
    internal
    | AssemblyName of string

[<RequireQualifiedAccess>]
module AssemblyName =
    let ofStr (str: string) =
        if String.IsNullOrEmpty str || str.IndexOfAny [| ':'; '\\'; '/' |] > -1
        then None
        else AssemblyName str |> Some

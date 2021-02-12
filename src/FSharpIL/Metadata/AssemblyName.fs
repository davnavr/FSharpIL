namespace FSharpIL.Metadata

open System

// II.22.2
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type AssemblyName =
    private
    | AssemblyName of string

    override this.ToString() = let (AssemblyName name) = this in name

[<RequireQualifiedAccess>]
module AssemblyName =
    let tryOfStr (str: string) =
        if String.IsNullOrEmpty str || str.IndexOfAny [| ':'; '\\'; '/' |] > -1
        then None
        else AssemblyName str |> Some

    let ofStr (str: string) =
        match tryOfStr str with
        | Some name -> name
        | None -> invalidArg "str" "The assembly name was empty or contains invalid characters."

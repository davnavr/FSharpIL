namespace FSharpIL.Metadata

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type Identifier = // TODO: Rename to Identifier.
    internal
    | Identifier of string

    override this.ToString() = let (Identifier name) = this in name

[<RequireQualifiedAccess>]
module Identifier =
    let tryOfStr str =
        match str with
        | null
        | "" -> None
        | _ -> Identifier str |> Some

    let ofStr str =
        match tryOfStr str with
        | Some name -> name
        | None -> invalidArg "str" "The name cannot be empty."

[<AutoOpen>]
module IdentifierPatterns =
    let (|Identifier|) (Identifier name) = name

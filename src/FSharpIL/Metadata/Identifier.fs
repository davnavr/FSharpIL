namespace FSharpIL.Metadata

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type Identifier =
    internal
    | Identifier of string // TODO: Make this a record to avoid generation of Tag member.

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

namespace FSharpIL.Metadata

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type NonEmptyName =
    internal
    | NonEmptyName of string

    override this.ToString() =
        let (NonEmptyName name) = this in name

[<RequireQualifiedAccess>]
module NonEmptyName =
    let tryOfStr str =
        match str with
        | null
        | "" -> None
        | _ -> NonEmptyName str |> Some

    let ofStr str =
        match tryOfStr str with
        | Some name -> name
        | None -> invalidArg "str" "The name cannot be empty."

[<AutoOpen>]
module NonEmptyNamePatterns =
    let (|NonEmptyName|) (NonEmptyName name) = name

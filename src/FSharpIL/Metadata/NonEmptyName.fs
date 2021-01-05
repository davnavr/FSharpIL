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
    let ofStr str =
        match str with
        | "" -> None
        | _ -> NonEmptyName str |> Some

[<AutoOpen>]
module NonEmptyNamePatterns =
    let (|NonEmptyName|) (NonEmptyName name) = name

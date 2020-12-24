namespace FSharpIL.Metadata

[<StructuralComparison; StructuralEquality>]
type ModuleName =
    internal
    | ModuleName of string

    override this.ToString() =
        let (ModuleName name) = this in name

[<RequireQualifiedAccess>]
module ModuleName =
    let ofString str =
        match str with
        | "" -> None
        | _ -> ModuleName str |> Some

namespace FSharpIL.PortableExecutable

open System.ComponentModel

[<StructuralComparison; StructuralEquality>]
type SectionName =
    internal
    | SectionName of string // Maybe use System.Text.Encoding.UTF8.GetBytes, unless UTF8 is not the encoding of the text.

    override this.ToString() =
        let (SectionName name) = this in name

[<RequireQualifiedAccess>]
module SectionName =
    let ofString str =
        match str with
        | ""
        | _ when str.Length > 8 -> None
        | _ -> SectionName str |> Some

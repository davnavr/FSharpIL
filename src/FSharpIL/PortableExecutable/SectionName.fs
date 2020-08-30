namespace FSharpIL.PortableExecutable

open System.ComponentModel

[<RequireQualifiedAccess>]
module SectionName =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Name =
        private
        | SectionName of string // Maybe use System.Text.Encoding.UTF8.GetBytes, unless UTF8 is not the encoding of the text.

        override this.ToString() =
            let (SectionName name) = this in name

    let ofString str =
        match str with
        | ""
        | _ when str.Length > 8 -> None
        | _ -> SectionName str |> Some

type SectionName = SectionName.Name

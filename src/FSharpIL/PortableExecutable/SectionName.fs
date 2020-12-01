namespace FSharpIL.PortableExecutable

[<StructuralComparison; StructuralEquality>]
type SectionName =
    internal
    | SectionName of byte[]

[<RequireQualifiedAccess>]
module SectionName =
    let ofBytes (bytes: byte[]) =
        if bytes.Length <= 8 then
            Array.init
                8
                (fun i ->
                    if i >= bytes.Length
                    then 0uy
                    else bytes.[i])
            |> SectionName
            |> Some
        else None
    let toArray (SectionName bytes) = bytes.Clone() :?> byte[]

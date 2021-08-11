namespace FSharpIL.PortableExecutable

open System.Runtime.CompilerServices

open FSharpIL.Utilities

/// Specifies the name of a section in its section header (II.25.3).
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type SectionName =
    internal { SectionName: byte[] }
    /// The length of the section name, including null padding.
    member this.Length = this.SectionName.Length
    static member internal Encoding = System.Text.Encoding.ASCII
    override this.ToString() = SectionName.Encoding.GetString(this.SectionName).TrimEnd '\000'

[<RequireQualifiedAccess>]
module SectionName =
    // TODO: Figure out if null padding bytes are included or not
    let text = { SectionName = ".text\000\000\000"B }
    let rsrc = { SectionName = ".rsrc\000\000\000"B }
    let reloc = { SectionName = ".reloc\000\000"B }

    let asSpan { SectionName = bytes } = System.ReadOnlySpan bytes

    let tryOfBytes (bytes: byte[]) =
        if bytes.Length <= 8 then
            let name = Array.zeroCreate<byte> 8
            for i = 0 to name.Length - 1 do
                name.[i] <-
                    if i >= bytes.Length
                    then 0uy
                    else bytes.[i]
            ValueSome { SectionName = name }
        else ValueNone

    let tryOfStr (str: string) = tryOfBytes(SectionName.Encoding.GetBytes str)

    let ofStr str =
        match tryOfStr str with
        | ValueSome name -> name
        | ValueNone ->
            invalidArg
                (nameof str)
                (sprintf "The section name \"%s\" must not be longer than 8 bytes and cannot contain any null characters" str)

    let toBlock { SectionName = bytes } = Convert.unsafeTo bytes

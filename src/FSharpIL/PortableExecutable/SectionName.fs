namespace FSharpIL.PortableExecutable

open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type SectionName =
    internal { SectionName: byte[] }
    override this.ToString() = Encoding.UTF8.GetString(this.SectionName).TrimEnd '\000'

[<RequireQualifiedAccess>]
module SectionName =
    let text = { SectionName = ".text"B }
    let rsrc = { SectionName = ".rsrc"B }
    let reloc = { SectionName = ".reloc"B }
    let ofBytes (bytes: byte[]) =
        if bytes.Length <= 8 then
            let name = Array.zeroCreate<byte> 8
            for i = 0 to name.Length - 1 do
                name.[i] <-
                    if i >= bytes.Length
                    then 0uy
                    else bytes.[i]
            ValueSome { SectionName = name }
        else ValueNone
    let toBlock { SectionName = bytes } = Unsafe.As<_, ImmutableArray<byte>>(&Unsafe.AsRef &bytes)

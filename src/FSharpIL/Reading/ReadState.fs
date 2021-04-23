namespace FSharpIL.Reading

[<Struct>]
type ReadState =
    | ReadPEMagic
    | MoveToLfanew
    | ReadLfanew
    | MoveToPEFileHeader
    | ReadCoffHeader
    | EndRead

    override this.ToString() =
        match this with
        | ReadPEMagic -> "reading Portable Executable magic"
        | MoveToLfanew -> "moving to lfanew field"
        | ReadLfanew -> "reading lfanew field"
        | EndRead -> "finishing read"

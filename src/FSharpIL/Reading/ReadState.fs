namespace FSharpIL.Reading

[<Struct>]
type ReadState =
    | ReadPEMagic
    | MoveToLfanew
    | EndRead

    override this.ToString() =
        match this with
        | ReadPEMagic -> "reading Portable Executable magic"
        | MoveToLfanew -> "moving to lfanew field"
        | EndRead -> "finishing read"

namespace FSharpIL.Reading

open FSharpIL

[<NoComparison; NoEquality>]
type ReadError =
    | InvalidPEMagic of byte[]
    | UnexpectedEndOfFile

    member this.Message =
        match this with
        | InvalidPEMagic magic ->
            sprintf
                "The file was not a Portable Executable, expected magic %s, but got %s"
                (Bytes.print Magic.PESignature)
                (Bytes.print magic)
        | UnexpectedEndOfFile -> "The end of the file was unexpectedly reached"

exception ReadException of ReadError with override this.Message = this.Data0.Message

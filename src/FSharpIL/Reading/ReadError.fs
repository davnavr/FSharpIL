namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Text

open FSharpIL

[<NoComparison; NoEquality>]
type ReadError =
    | InvalidMagic of expected: ImmutableArray<byte> * actual: byte[]
    | OptionalHeaderTooSmall of size: uint16
    | TooFewDataDirectories of count: uint32
    | NoCliMetadata
    | RvaNotInTextSection of rva: uint32
    | CliHeaderTooSmall of size: uint32
    | MetadataVersionHasNoNullTerminator of version: byte[]
    | UnexpectedEndOfFile

    member this.Message =
        match this with
        | InvalidMagic(expected, Bytes.ReadOnlySpan actual) ->
            sprintf
                "expected magic %s, but got %s"
                (Bytes.print(expected.AsSpan()))
                (Bytes.print actual)
        | OptionalHeaderTooSmall size -> sprintf "the specified optional header size (%i) is too small." size
        | TooFewDataDirectories count -> sprintf "the number of data directories (%i) is too small" count
        | NoCliMetadata -> "the Portable Executable file does not contain any CLI metadata"
        | RvaNotInTextSection rva ->
            sprintf "the Relative Virtual Address (0x%08X) does not point into the \".text\" section" rva
        | CliHeaderTooSmall size -> sprintf "the specified CLI header size is too small (%i)" size
        | MetadataVersionHasNoNullTerminator version ->
            sprintf
                "the metadata version in the CLI metadata root \"%s\" does not end in a null terminator"
                (Encoding.UTF8.GetString version)
        | UnexpectedEndOfFile -> "the end of the file was unexpectedly reached"

[<RequireQualifiedAccess>]
module ReadError =
    let message (state: ReadState) (error: ReadError) (offset: uint64) =
        sprintf "Error occured while %s at offset 0x%X: %s" state.Description offset error.Message

exception ReadException
    of state: ReadState * error: ReadError * offset: uint64
    with override this.Message = ReadError.message this.state this.error this.offset

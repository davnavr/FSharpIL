﻿namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL

[<NoComparison; NoEquality>]
type ReadError =
    | InvalidMagic of expected: ImmutableArray<byte> * actual: byte[]
    | OptionalHeaderTooSmall of size: uint16
    | TooFewDataDirectories of count: uint32
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
        | UnexpectedEndOfFile -> "the end of the file was unexpectedly reached"

[<RequireQualifiedAccess>]
module ReadError =
    let message (state: ReadState) (error: ReadError) (offset: uint64) =
        sprintf "Error occured while %s at offset %i: %s" state.Description offset error.Message

exception ReadException
    of state: ReadState * error: ReadError * offset: uint64
    with override this.Message = ReadError.message this.state this.error this.offset

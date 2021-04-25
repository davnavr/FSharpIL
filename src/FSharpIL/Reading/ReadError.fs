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
                "Expected magic %s, but got %s"
                (Bytes.print(expected.AsSpan()))
                (Bytes.print actual)
        | OptionalHeaderTooSmall size -> sprintf "The specified optional header size (%i) is too small." size
        | TooFewDataDirectories count -> sprintf "The number of data directories (%i) is too small" count
        | UnexpectedEndOfFile -> "The end of the file was unexpectedly reached"

exception ReadException
    of offset: uint64 * state: ReadState * error: ReadError
    with
        override this.Message =
            sprintf "Error occured at offset %i while %s. %s" this.offset this.state.Description this.error.Message

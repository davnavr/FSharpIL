module internal FSharpIL.Parser

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<Sealed>]
type ByteStream(name: string, stream: Stream) =
    let mutable disposed = false
    let mutable pos = 0L

    member _.Name = name
    member _.Position = pos

    member _.ReadByte() =
        match stream.ReadByte() with
        | -1 -> None
        | next ->
            pos <- pos + 1L
            byte next |> Some

    interface IDisposable with
        member _.Dispose() = if not disposed then stream.Close()

[<IsReadOnly; Struct>]
type ParseResult<'Result> =
    | Success of result: 'Result
    | Error of msg: string

// TODO: Maybe parsing using Span and ReadOnlySpan is possible?
type Parser<'Result> = ByteStream -> ParseResult<'Result>

let eof: Parser<_> =
    fun stream ->
        match stream.ReadByte() with
        | None -> Success()
        | Some b -> sprintf "Expected end of file, but got 0x%X" b |> Error
let nothing = fun _ -> ()
let fail msg = fun _ -> Error msg
let preturn item = fun _ -> Success item

let pmany (parsers: seq<Parser<_>>): Parser<ImmutableList<_>> =
    fun stream ->
        use enumerator = parsers.GetEnumerator()
        let mutable err = None
        let results = ImmList.builder()
        while enumerator.MoveNext() && err.IsNone do
            match enumerator.Current stream with
            | Success result -> results.Add result
            | Error msg -> err <- Some msg
        match err with
        | None -> results.ToImmutable() |> Success
        | Some msg -> Error msg

let pbyte b: Parser<_> =
    fun stream ->
        match stream.ReadByte() with
        | Some read when read = b -> Success read
        | Some actual -> sprintf "Expected 0x%02X at 0x%02X, got 0x%02X" b stream.Position actual |> Error
        | None -> sprintf "Expected 0x%02X at 0x%02X, but got end of file" b stream.Position |> Error
let pbytes (bytes: byte[]) =
    fun stream ->
        let mutable pos = 0
        let mutable err = None
        while err.IsNone && pos < bytes.Length do
            let exp = Array.item pos bytes
            match pbyte exp stream with
            | Success _ -> pos <- pos + 1
            | Error msg -> err <- Some msg
        match err with
        | None -> Success bytes
        | Some msg -> Error msg

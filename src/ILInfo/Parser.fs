module internal ILInfo.Parser

open System
open System.Collections.Immutable
open System.IO

open ILInfo.Utilities.Collections

[<Sealed>]
type ByteStream(name: string, stream: Stream) =
    let mutable disposed = false
    let mutable pos = 0L

    member _.Name = name
    member _.Position = pos

    member _.Read() =
        match stream.ReadByte() with
        | -1 -> None
        | next -> byte next |> Some

    interface IDisposable with
        member _.Dispose() = if not disposed then stream.Close()

[<Struct>]
type ParseResult<'Result> =
    | Success of result: 'Result
    | Error of msg: string

type Parser<'Result> = ByteStream -> ParseResult<'Result>

let nothing = fun _ -> ()
let fail msg: Parser<_> = fun _ -> Error msg

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
        match stream.Read() with
        | Some read when read = b -> Success read
        | Some actual -> sprintf "Expected %X, got %X" b actual |> Error
        | None -> sprintf "Expected %X, but got end of file" b |> Error
let pbytes: seq<byte> -> Parser<_> = Seq.map pbyte >> pmany

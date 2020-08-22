namespace FSharpIL.Reader

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open FSharpIL
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

// TODO: Maybe parsing using Span and ReadOnlySpan is possible?
type Reader<'Result> = ByteStream -> Result<'Result, ReadError>

[<AutoOpen>]
module Primitives =
    let tuple2 p1 p2: Reader<_> =
        fun stream ->
            match p1 stream with
            | Ok r1 ->
                match p2 stream with
                | Ok r2 -> Ok(r1, r2)
                | Error err -> Error err // TODO: Factor out all of the | Error err -> Error err
            | Error err -> Error err

    let (.>>) p1 p2: Reader<_> =
        fun stream ->
            match p1 stream with
            | Ok result ->
                match p2 stream with
                | Ok _ -> Ok result
                | err -> err
            | err -> err
    let (>>.) p1 p2: Reader<_> =
        fun stream ->
            match p1 stream with
            | Ok _ -> p2 stream
            | Error err -> Error err
    let inline (.>>.) p1 p2 = tuple2 p1 p2
    let (|>>) p f: Reader<_> =
        fun stream ->
            match p stream with
            | Ok result -> f result |> Ok
            | Error err -> Error err
    let (>>=) p f: Reader<_> =
        fun stream ->
            match p stream with
            | Ok result ->
                stream |> f result
            | Error err -> Error err

    let fail err: Reader<_> = fun _ -> Error err
    let retn value: Reader<_> = fun _ -> Ok value

    let toPos pos err: Reader<_> =
        fun stream ->
            if pos < stream.Position then
                err stream.Position |> Error
            else
                while stream.Position < pos do
                    stream.ReadByte() |> ignore
                Ok()

[<AutoOpen>]
module ByteReaders =
    let array (bytes: byte[]) error: Reader<_> =
        fun stream ->
            let mutable pos = 0
            let mutable err = None
            while err.IsNone && pos < bytes.Length do
                let exp = Array.item pos bytes
                match stream.ReadByte() with
                | Some byte when byte = exp ->
                    pos <- pos + 1
                | result ->
                    err <- error result |> Some
            match err with
            | None -> Ok bytes
            | Some msg -> Error msg
    let count num error: Reader<_> =
        fun stream ->
            let results = Array.zeroCreate<byte> num
            let mutable counter = 0
            let mutable err = None
            while err.IsNone && counter < num do
                match stream.ReadByte() with
                | Some b ->
                    Array.set results counter b
                    counter <- counter + 1
                | None -> err <- error results |> Some
            match err with
            | None -> Ok results
            | Some msg -> Error msg

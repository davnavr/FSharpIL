namespace FSharpIL

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
    let mutable pos = 0u

    member _.Name = name
    member _.Position = pos

    member _.ReadByte() =
        match stream.ReadByte() with
        | -1 -> None
        | next ->
            pos <- pos + 1u
            byte next |> Some
    member this.ReadBytes(count) =
        let results = Array.zeroCreate<byte> count
        let mutable i = 0
        let mutable ok = true
        while ok && i < count do
            match this.ReadByte() with
            | Some value ->
                Array.set results i value
                i <- i + 1
            | None -> ok <- false
        if ok
        then results
        else Array.take i results
    member this.TryMove offset =
        let mutable error = None
        while Option.isNone error && pos < offset do
            match this.ReadByte() with
            | Some a -> ()
            | None -> error <- Some pos
        match error with
        | None -> Ok()
        | Some err -> Error err

    member this.ReadUInt32() =
        match this.ReadBytes 4 with
        | [| b1; b2; b3; b4 |] ->
            uint b1 + (uint b2 <<< 8) + (uint b3 <<< 16) + (uint b4 <<< 24) |> Ok
        | err -> Error err

    interface IDisposable with
        member _.Dispose() = if not disposed then stream.Close()

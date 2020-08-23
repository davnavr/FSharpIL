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
    member _.ReadBytes(count) =
        invalidOp "bad"
    member this.TryMove offset =
        let mutable result = Ok()
        while Result.isOk result && pos < offset do
            match this.ReadByte() with
            | Some _ -> ()
            | None -> result <- Error pos
        result

    interface IDisposable with
        member _.Dispose() = if not disposed then stream.Close()

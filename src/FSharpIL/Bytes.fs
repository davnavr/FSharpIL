module FSharpIL.Bytes

open System

[<AbstractClass>]
type ByteWriter<'Result>() =
    abstract member GetResult: unit -> 'Result
    abstract member Write: currentPos: uint64 * byte -> unit
    abstract member Write: currentPos: uint64 * byte[] -> unit
    interface IDisposable with member _.Dispose() = ()

[<Sealed>]
type Writer<'Result>(writer: ByteWriter<'Result>) =
    let mutable pos = 0UL

    member _.Position = pos

    member _.GetResult() = writer.GetResult()

    member _.Write(bytes: byte[]) =
        if bytes.Length > 0 then
            try writer.Write(pos, bytes)
            with
            | ex ->
                let msg = sprintf "Exception thrown while writing a byte array of length %i at position %i" bytes.Length pos
                InvalidOperationException(msg, ex) |> raise

            // The position is updated afterward, which means that the first byte written is always at position zero.
            pos <- pos + uint64 bytes.Length

    member _.Write(byte: byte) =
        try writer.Write(pos, byte)
        with
        | ex ->
            let msg = sprintf "Exception thrown while writing byte at position %i" pos
            InvalidOperationException(msg, ex) |> raise

        pos <- pos + 1UL

    interface IDisposable with member _.Dispose() = (writer :> IDisposable).Dispose()

type WriteExpr<'Result> = Writer<'Result> -> unit

[<Sealed>]
type ByteBuilder() =
    member inline _.Combine(one: WriteExpr<_>, two: WriteExpr<_>) = fun writer -> one writer; two writer
    member inline _.Delay(f: unit -> WriteExpr<_>) = fun writer -> f () writer
    member inline _.For(items: seq<'T>, body: 'T -> WriteExpr<_>) =
        fun writer -> for item in items do body item writer
    member inline _.Yield(value: byte) = fun (writer: Writer<_>) -> writer.Write value
    member inline _.Yield(bytes: byte[]) = fun (writer: Writer<_>) -> writer.Write bytes
    member inline _.Yield(bytes: seq<byte>) = fun (writer: Writer<_>) -> bytes |> Seq.iter writer.Write
    member inline _.Yield(value: uint16) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFus) |> writer.Write
            (value >>> 8) &&& 0xFFus |> byte |> writer.Write
    member inline _.Yield(value: uint32) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFu) |> writer.Write
            (value >>> 8) &&& 0xFFu |> byte |> writer.Write
            (value >>> 16) &&& 0xFFu |> byte |> writer.Write
            (value >>> 24) &&& 0xFFu |> byte |> writer.Write
    member inline _.Yield(value: uint64) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFUL) |> writer.Write
            (value >>> 8) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 16) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 24) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 32) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 40) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 48) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 56) &&& 0xFFUL |> byte |> writer.Write
    member inline _.Yield(expr: WriteExpr<_>) = expr
    member inline _.Zero() = ignore<Writer<_>>

let bytes = ByteBuilder()

let withLength (len: uint64) (expr: WriteExpr<_>) =
    fun (printer: Writer<_>) ->
        let startPos = printer.Position
        expr printer
        let currentPos = printer.Position
        let len' = currentPos - startPos
        if len' <> len then
            sprintf "Expected final length to be %i while writing bytes, but the actual length was %i" len len'
            |> internalExn
                [
                    "Writer", printer :> obj
                    "StartPosition", startPos :> obj
                    "CurrentPosition", currentPos :> obj
                    "ExpectedLength", len :> obj
                    "ActualLength", len' :> obj
                ]

let empty amt (writer: Writer<_>) =
    let mutable i = 0UL
    while i < amt do
        writer.Write 0uy
        i <- i + 1UL

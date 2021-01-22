namespace FSharpIL

open Microsoft.FSharp.Core.Operators.Checked

open System

[<Sealed>]
type internal Writer private (previous: Writer option, bytes: byte[]) =
    let mutable previous = previous
    let mutable pos = 0
    let mutable bytes = bytes

    new (previous: Writer option, capacity) =
        Writer(previous, Array.zeroCreate<byte> capacity)

    member _.Capacity = bytes.Length
    member _.Previous = previous
    member _.FreeBytes = bytes.Length - pos
    member private _.UnderlyingArray = bytes

    /// Appends the current writer to the end of another writer.
    member _.AppendToEnd(other: Writer) = previous <- Some other

    member _.WriteU1 value =
        if pos >= bytes.Length then
            previous <- Writer(previous, bytes) |> Some
            pos <- 0
            bytes <- Array.zeroCreate<byte> bytes.Length
        bytes.[pos] <- value
        pos <- pos + 1

    /// Writes an unsigned 2-byte integer in little-endian format.
    member this.WriteU2 value =
        this.WriteU1(value &&& 0xFFus |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFus |> byte)

    member inline this.WriteU2 value = this.WriteU2(uint16 value)

    /// Writes an unsigned 4-byte integer in little-endian format.
    member this.WriteU4 value =
        this.WriteU1(value &&& 0xFFu |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFu |> byte)

    member inline this.WriteU4 value = this.WriteU4(uint32 value)

    /// Writes an unsigned 8-byte integer in little-endian format.
    member this.WriteU8 value =
        this.WriteU1(value &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 32) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 40) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 48) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 56) &&& 0xFFUL |> byte)

    member inline this.WriteU8 value = this.WriteU8(uint64 value)

    member this.WriteBytes(bytes: byte[]) =
        if this.FreeBytes <= bytes.Length then
            Span(bytes).CopyTo(Span(bytes, pos, bytes.Length))
        else
            Seq.ofArray bytes |> this.WriteBytes

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    member _.MoveToEnd() = pos <- bytes.Length

    member this.ToArray() =
        let rec inner (writer: Writer) len cont =
            let len' = len + this.Capacity
            match writer.Previous with
            | Some prev ->
                let cont' i content =
                    Span(writer.UnderlyingArray).CopyTo(Span(content, i, writer.Capacity))
                    let i' = i + writer.Capacity
                    cont i' content
                inner prev len' cont'
            | None ->
                let content = Array.zeroCreate<byte> len'
                Span(writer.UnderlyingArray).CopyTo(Span content)
                cont writer.Capacity content
        inner this 0 (fun _ -> id)

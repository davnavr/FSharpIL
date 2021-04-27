namespace FSharpIL.Reading

open System

open FSharpIL

[<Sealed>]
type internal ChunkReader (chunks: byte[][]) =
    let mutable chunki, pos, read = 0, 0, 0UL

    member _.BytesRead = read

    member private _.FreeBytes = chunks.[chunki].Length - pos

    member private _.Advance count =
        let current = chunks.[chunki]
        let remaining = count
        while remaining > 0 do
            if pos >= current.Length then
                pos <- 0
                chunki <- chunki + 1
            pos <- pos + (min current.Length remaining)

    member this.ReadU1() =
        let value = chunks.[chunki].[pos]
        this.Advance 1
        value

    member this.ReadBytes(count, buffer: byref<Span<byte>>) =
        if this.FreeBytes = count then
            buffer <- Span(chunks.[chunki], pos, count)
            true
        elif this.FreeBytes = 0 then false
        else
            let buffer' = Array.zeroCreate count
            for i = 0 to count - 1 do
                buffer'.[i] <- this.ReadU1()
            buffer <- Span buffer'
            true

    member this.ReadU2(value: byref<uint16>) =
        let mutable buffer = Span()
        let parsed = this.ReadBytes(2, &buffer)
        if parsed then value <- Bytes.readU2 0 buffer
        parsed

    member this.ReadU4(value: byref<uint32>) =
        let mutable buffer = Span()
        let parsed = this.ReadBytes(4, &buffer)
        if parsed then value <- Bytes.readU4 0 buffer
        parsed

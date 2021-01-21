namespace FSharpIL

open Microsoft.FSharp.Core.Operators.Checked

open System

type internal ByteWriter (chunkCount: int32, chunkSize: int32) =
    do
        if chunkCount <= 0 then
            invalidArg (nameof chunkCount) "The number of chunks must be positive"
        if chunkSize <= 0 then
            invalidArg (nameof chunkSize) "The size of chunks must be positive"

    let mutable bytes: byte[][] = Array.zeroCreate chunkSize |> Array.replicate chunkCount
    let mutable pos = 0UL

    member _.ChunkSize = chunkSize
    member _.ChunkCount = uint64 bytes.Length
    member _.Position = pos

    /// Gets an index into the chunk array.
    member private this.ChunkIndex = pos / this.ChunkCount |> int32
    member private this.ChunkPosition = pos % this.ChunkCount |> int32
    member private this.Chunk = bytes.[this.ChunkIndex]

    member this.WriteBytes(bytes: byte[]) =
        match bytes with
        | [||] -> ()
        | [| value |] -> this.WriteU1 value
        | _ ->
            try
                let bytes' = Span bytes
                let destination = Span(this.Chunk, this.ChunkPosition, bytes.Length)
                bytes'.CopyTo destination
            with
            | ex ->
                InvalidOperationException(sprintf "Unable to write array of length %i" bytes.Length, ex) |> raise

    member this.WriteU1(value: byte) =
        let index = this.ChunkIndex
        if index >= bytes.Length then
            sprintf "Cannot write byte %i, reached the end of the byte array." value |> invalidOp
        this.Chunk.[this.ChunkPosition] <- value
        pos <- pos + 1UL

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

    /// Advances the writer to the next chunk.
    member this.NextChunk() =
        pos <- pos + 1UL |> Round.upTo this.ChunkCount

    member this.ToArray() =
        let result = bytes.Length * this.ChunkSize |> Array.zeroCreate<byte>
        for i = 0 to bytes.Length - 1 do
            let chunk' = Span bytes.[i]
            let destination = Span(result, i * this.ChunkSize, this.ChunkSize)
            chunk'.CopyTo destination
        result

    member this.SpanCurrentChunk length =
        let chunk = Span this.Chunk
        let i = this.ChunkIndex
        if i + length >= this.ChunkSize then
            sprintf
                "Unable to create span of length %i as there are only %i bytes left in the current chunk."
                length
                (this.ChunkSize - i)
            |> invalidOp
        chunk.Slice(this.ChunkPosition, length)

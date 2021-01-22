namespace FSharpIL.Bytes

open Microsoft.FSharp.Core.Operators.Checked

open System

open FSharpIL

[<AbstractClass>]
type internal ByteWriter() =
    abstract Position: uint64

    abstract WriteBytes: byte[] -> unit

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    abstract WriteU1: byte -> unit

    /// Writes an unsigned 2-byte integer in little-endian format.
    abstract WriteU2: uint16 -> unit
    default this.WriteU2 value =
        this.WriteU1(value &&& 0xFFus |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFus |> byte)

    member inline this.WriteU2 value = this.WriteU2(uint16 value)

    /// Writes an unsigned 4-byte integer in little-endian format.
    abstract WriteU4: uint32 -> unit
    default this.WriteU4 value =
        this.WriteU1(value &&& 0xFFu |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFu |> byte)

    member inline this.WriteU4 value = this.WriteU4(uint32 value)

    /// Writes an unsigned 8-byte integer in little-endian format.
    abstract WriteU8: uint64 -> unit
    default this.WriteU8 value =
        this.WriteU1(value &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 32) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 40) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 48) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 56) &&& 0xFFUL |> byte)

    member inline this.WriteU8 value = this.WriteU8(uint64 value)

[<Sealed>]
type internal ChunkedByteWriter (chunkCount: int32, chunkSize: int32) =
    inherit ByteWriter()

    do
        if chunkCount <= 0 then
            invalidArg (nameof chunkCount) "The number of chunks must be positive"
        if chunkSize <= 0 then
            invalidArg (nameof chunkSize) "The size of chunks must be positive"

    let bytes: byte[][] = Array.zeroCreate chunkSize |> Array.replicate chunkCount
    let mutable pos = 0UL
    
    member _.ChunkCount = uint64 bytes.Length
    /// The remaining number of bytes left in the current chunk.
    member this.ChunkRemaining = this.ChunkSize - this.ChunkIndex
    member _.ChunkSize = chunkSize
    override _.Position = pos

    /// Gets an index into the chunk array.
    member private this.ChunkIndex = pos / this.ChunkCount |> int32
    member private this.ChunkPosition = pos % this.ChunkCount |> int32
    member private this.Chunk = bytes.[this.ChunkIndex]

    override this.WriteBytes(bytes: byte[]) =
        try
            match bytes with
            | [||] -> ()
            | [| value |] -> this.WriteU1 value
            | _  when this.ChunkRemaining <= bytes.Length ->
                let bytes' = Span bytes
                let destination = Span(this.Chunk, this.ChunkPosition, bytes.Length)
                bytes'.CopyTo destination
            | _ -> Seq.ofArray bytes |> this.WriteBytes
        with
        | ex ->
            InvalidOperationException(sprintf "Unable to write array of length %i" bytes.Length, ex) |> raise

    override this.WriteU1(value: byte) =
        let index = this.ChunkIndex
        if index >= bytes.Length then
            sprintf "Cannot write byte %i, reached the end of the byte array." value |> invalidOp
        this.Chunk.[this.ChunkPosition] <- value
        pos <- pos + 1UL

    /// Advances the writer to the next chunk.
    member this.NextChunk() =
        pos <- pos + 1UL |> Round.upTo this.ChunkCount

    member this.SkipBytes count =
        if count > this.ChunkRemaining then
            sprintf
                "Unable to skip %i bytes as there are only %i remaining in the current chunk."
                count
                this.ChunkRemaining
            |> invalidArg (nameof count)
        pos <- pos + uint64 count

    member this.ToArray() =
        let result = bytes.Length * this.ChunkSize |> Array.zeroCreate<byte>
        for i = 0 to bytes.Length - 1 do
            let chunk' = Span bytes.[i]
            let destination = Span(result, i * this.ChunkSize, this.ChunkSize)
            chunk'.CopyTo destination
        result

    member this.SpanCurrentChunk length =
        let chunk = Span this.Chunk
        if length > this.ChunkRemaining then
            sprintf
                "Unable to create span of length %i as there are only %i bytes left in the current chunk."
                length
                this.ChunkRemaining
            |> invalidOp
        chunk.Slice(this.ChunkPosition, length)

    member this.SpanCurrentChunk() = Span this.Chunk

[<Sealed>]
type internal ResizeByteWriter (capacityIncr: int32) =
    inherit ByteWriter()

    let mutable bytes = Array.zeroCreate<byte> capacityIncr
    let mutable pos = 0

    do
        if capacityIncr <= 0 then
            invalidArg (nameof capacityIncr) "The capacity value must be positive"

    override _.Position = uint64 pos

    member _.Capacity = bytes.Length

    override this.WriteBytes bytes' =
        let remaining = bytes'.Length - pos
        if remaining < bytes'.Length then
            let n = (bytes'.Length - remaining + capacityIncr) / capacityIncr
            this.IncreaseCapacity n
        Span(bytes').CopyTo(Span(bytes, pos, bytes.Length - pos))

    override this.WriteU1 value =
        if pos >= bytes.Length then this.IncreaseCapacity 1
        bytes.[pos] <- value
        pos <- pos + 1

    member private _.IncreaseCapacity n =
        let replacement = bytes.Length + (n * capacityIncr) |> Array.zeroCreate
        Span(bytes).CopyTo(Span(replacement, 0, bytes.Length))
        bytes <- replacement

    member internal _.UnderlyingArray() = bytes

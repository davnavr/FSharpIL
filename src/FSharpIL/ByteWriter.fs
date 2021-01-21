namespace FSharpIL

open Microsoft.FSharp.Core.Operators.Checked

open System

[<System.Runtime.CompilerServices.IsByRefLike; Struct>]
type internal ByteWriter =
    struct
        val mutable private bytes: byte[][]
        val mutable private pos: uint64
        val ChunkSize: int32

        new (chunkCount: int32, chunkSize: int32) =
            if chunkCount <= 0 then
                invalidArg (nameof chunkCount) "The number of chunks must be positive"
            if chunkSize <= 0 then
                invalidArg (nameof chunkSize) "The size of chunks must be positive"

            { bytes = Array.zeroCreate chunkSize |> Array.replicate chunkCount
              pos = 0UL
              ChunkSize = chunkSize }
    end

type ByteWriter with
    member this.ChunkCount = uint64 this.bytes.Length
    member this.Position = this.pos

    /// Gets an index into the chunk array.
    member private this.ChunkIndex = this.pos / this.ChunkCount |> int32
    member private this.ChunkPosition = this.pos % this.ChunkCount |> int32

    member this.WriteBytes(bytes: byte[]) =
        match bytes with
        | [||] -> ()
        | [| value |] -> this.WriteU1 value
        | _ ->
            let bytes' = Span bytes
            let destination = Span(this.bytes.[this.ChunkIndex], this.ChunkPosition, bytes.Length)
            bytes'.CopyTo destination

    member this.WriteU1(value: byte) =
        let index = this.ChunkIndex
        if index >= this.bytes.Length then
            sprintf "Cannot write byte %i, reached the end of the byte array." value |> invalidOp
        this.bytes.[index].[this.ChunkPosition] <- value
        this.pos <- this.pos + 1UL

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
        this.pos <- this.pos + 1UL |> Round.upTo this.ChunkCount

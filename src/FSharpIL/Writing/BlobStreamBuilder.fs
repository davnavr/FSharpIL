namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type internal BlobEntry = { Offset: BlobOffset; Length: uint32 }

type BlobWriter = delegate of byref<ChunkedMemoryBuilder> -> unit

/// <summary>Builds the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type BlobStreamBuilder (capacity: int32) =
    let entries = List<BlobEntry> capacity
    let mutable offset = 1u
    let mutable content = ChunkedMemoryBuilder capacity
    do content.Write 0uy
    do entries.Add Unchecked.defaultof<_>

    member _.IsEmpty = entries.Count = 1

    /// <summary>The length of the <c>#US</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset

    member private _.AddEntry start =
        let length = offset - start
        let entry = { Offset = { BlobOffset = start }; Length = BlobWriter.compressedUnsignedSize length + length }
        entries.Add entry
        entry.Offset

    // TODO: Add overload that accepts function pointer for adding byte blob when available.
    member this.Add(writer: BlobWriter) =
        let start = offset
        writer.Invoke &content
        this.AddEntry start

    member this.Add(bytes: ReadOnlySpan<byte>) =
        let start = offset
        content.Write bytes
        this.AddEntry start

    member inline this.Add(bytes: ReadOnlyMemory<byte>) = this.Add bytes.Span
    member inline this.Add(bytes: byte[]) = this.Add(ReadOnlySpan bytes)
    member inline this.Add(bytes: ImmutableArray<byte>) = this.Add(bytes.AsSpan())

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.blob
        member _.Serialize wr =
            let mutable offset', content' = 0u, content.MapChunksUnsafe Convert.unsafeTo<_, ImmutableArray<byte>>
            for { Length = length } in entries do
                BlobWriter.compressedUnsigned length &wr
                wr.Write(content'.Slice(0u, length))
                content' <- content'.Slice length
                offset' <- offset' + length

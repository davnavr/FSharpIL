namespace FSharpIL.Reading

open System
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL

/// <summary>Represents an offset into the <c>#Strings</c> metadata heap (II.24.2.3).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedString (offset: uint32) =
    member _.Offset = offset
    member _.IsNull = offset = 0u

[<AutoOpen>]
module ParsedString = let inline (|ParsedString|) (offset: ParsedString) = offset.Offset

/// <summary>Represents the <c>#Strings</c> metadata heap, which contains null-terminated UTF-8 strings (II.24.2.3).</summary>
[<NoComparison; StructuralEquality>]
type ParsedStringsHeap =
    private
        { Chunk: ChunkReader
          mutable StringBuffer: byte[]
          StringsOffset: uint64
          Size: uint64 }

    member private this.AppendByte(i, value) =
        if i >= this.StringBuffer.Length then
            let buffer' = Array.zeroCreate<byte>(this.StringBuffer.Length * 2)
            Span(this.StringBuffer).CopyTo(Span buffer')
            this.StringBuffer <- buffer'
        this.StringBuffer.[i] <- value

    member this.IsValidOffset(Convert.U8 offset) = this.Chunk.IsValidOffset offset && offset < this.Size

    member private this.TryGet(ParsedString offset) =
        let rec inner i =
            let offset' = uint64 offset + uint64 i
            if offset' >= this.Size
            then Error(MissingNullTerminator(Encoding.UTF8.GetString this.StringBuffer))
            else
                match this.Chunk.ReadU1(this.StringsOffset + offset') with
                | 0uy -> Ok i
                | value ->
                    this.AppendByte(i, value)
                    inner(i + 1)
        if this.IsValidOffset offset
        then inner 0
        else Error(InvalidStringIndex(offset, this.Size))

    member private this.TryGetSpan(offset, buffer: outref<Span<byte>>) =
        match this.TryGet offset with
        | Ok i ->
            buffer <- Span(this.StringBuffer).Slice(0, i - 1)
            Ok()
        | Error err -> Error err

    member this.TryGetBytes offset =
        let mutable buffer = Span()
        match this.TryGetSpan(offset, &buffer) with
        | Ok() -> Ok(buffer.ToArray())
        | Error err -> Error err

    member this.TryGetString offset =
        let mutable buffer = Span()
        match this.TryGetSpan(offset, &buffer) with
        | Ok() -> Ok(Encoding.UTF8.GetString(Span.asReadOnly buffer))
        | Error err -> Error err

[<RequireQualifiedAccess>]
module ParsedStringsHeap =
    let [<Literal>] private DefaultBufferSize = 32;

    let internal ofHeader (chunk: ChunkReader) header =
        match header with
        | ValueSome { ParsedStreamHeader.Offset = Convert.U8 offset; Size = Convert.U8 size }
            when chunk.HasFreeBytes(offset, size) ->
            ValueSome { Chunk = chunk; StringsOffset = offset; Size = size; StringBuffer = Array.zeroCreate DefaultBufferSize }
        | _ -> ValueNone

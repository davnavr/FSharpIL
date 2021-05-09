namespace FSharpIL.Reading

open System
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL

/// <summary>Represents an offset into the <c>#Strings</c> metadata heap (II.24.2.3).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedString =
    internal { StringOffset: uint32 }
    override this.ToString() = sprintf "0x%08X" this.StringOffset
    static member op_Implicit { StringOffset = offset } = offset

/// <summary>Represents the <c>#Strings</c> metadata heap, which contains null-terminated UTF-8 strings (II.24.2.3).</summary>
[<Sealed>]
type ParsedStringsStream internal (stream: ParsedMetadataStream) =
    member _.Size = stream.StreamSize
    member _.IsValidOffset offset = stream.IsValidOffset offset

    member private _.TryGet { StringOffset = offset } =
        let rec inner i =
            let offset' = uint64 offset + uint64 i
            if offset' >= stream.StreamSize
            then Error(MissingNullTerminator(Encoding.UTF8.GetString stream.Buffer))
            else
                match stream.Chunk.ReadU1(stream.StreamOffset + offset') with
                | 0uy -> Ok i
                | value ->
                    stream.AppendByte(i, value)
                    inner(i + 1)
        if stream.IsValidOffset offset
        then inner 0
        else Error(InvalidStringIndex(offset, stream.StreamSize))

    member private this.TryGetSpan(offset, buffer: outref<Span<byte>>) =
        match this.TryGet offset with
        | Ok i ->
            buffer <- Span(stream.Buffer).Slice(0, i - 1)
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

    member this.GetString offset =
        match this.TryGetString offset with
        | Ok str -> str
        | Error err -> invalidArg "offset" (sprintf "Error, %s" err.Message)

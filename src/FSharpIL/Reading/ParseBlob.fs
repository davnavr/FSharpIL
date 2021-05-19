namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata

[<IsReadOnly; Struct>]
type ParsedCustomMod = { ModifierRequired: bool; ModifierType: ParsedTypeDefOrRefOrSpec }

// TODO: Try not to make blob item structs by ref like.
/// <summary>Represents a type parsed from a signature in the <c>#Blob</c> metadata heap (II.23.2.12).</summary>
[<IsReadOnly; Struct>]
type ParsedType =
    internal { TypeTag: ElementType; Chunk: ChunkedMemory } // TODO: Figure out a more efficient way to store type information.
    member this.Tag = this.TypeTag

[<IsReadOnly; Struct>]
type ParsedFieldSig =
    { CustomModifiers: ImmutableArray<ParsedCustomMod>
      FieldType: ParsedType }

[<RequireQualifiedAccess>]
module internal ParseBlob =
    let inline ensureLastItem result =
        match result with
        | Ok(chunk: ChunkedMemory, value) when chunk.IsEmpty -> Ok value
        | Ok _ -> Error(ExpectedEndOfBlob(invalidOp "TODO: Add error information for expected blob end", invalidOp "", invalidOp ""))
        | Error err -> Error err

    // TODO: Since the size needed to store the value can be determined, consider returning Result<uint32, BlobError> without an outref for the parsed value instead.
    /// Reads a variable-width, unsigned big-endian integer (II.23.2).
    let tryReadUnsigned offset (chunk: inref<ChunkedMemory>) (value: outref<uint32>) = // TODO: Move bit shifting logic to ByteParser.fs
        let parsed = chunk.[offset]
        if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
            value <- uint32(parsed &&& 0b0111_1111uy)
            Ok 1uy
        elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
            if chunk.HasFreeBytes(offset, 2u) then
                value <- (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.[offset + 1u]) |> uint32
                Ok 2uy
            else Error(CompressedIntegerOutOfBounds 2u)
        elif parsed &&& 0b1110_0000uy = 0b1100_0000uy then // 4 bytes
            if chunk.HasFreeBytes(offset, 4u) then
                value <-
                    (uint32(parsed &&& 0b0001_1111uy) <<< 24)
                    + (uint32(chunk.[offset + 1u]) <<< 16)
                    + (uint32(chunk.[offset + 2u]) <<< 8)
                    + uint32(chunk.[offset + 3u])
                Ok 4uy
            else Error(CompressedIntegerOutOfBounds 4u)
        else Error(InvalidUnsignedCompressedIntegerKind parsed)

    // TODO: Figure out if it is more efficient to use -> struct(ChunkedMemory * Result<_, BlobError>), -> Result<struct(ChunkedMemory * _), BlobError>, byref<_> -> Result<ChunkedMemory, BlobError>, or byref<ChunkedMemory> -> byref<_>: BlobError option
    let typeDefOrRefOrSpec offset (chunk: inref<ChunkedMemory>) =
        let mutable value = Unchecked.defaultof<uint32>
        match tryReadUnsigned offset &chunk &value with
        | Ok size ->
            struct (
                chunk.Slice(uint32 size),
                { Tag = LanguagePrimitives.EnumOfValue(uint8(value &&& 0b11u))
                  TypeIndex = value >>> 2 }
            )
            |> Ok
        | Error err -> Error err

    let cmodifier (chunk: inref<ChunkedMemory>) =
        // TODO: Avoid code duplication for first byte of custom modifier list.
        match LanguagePrimitives.EnumOfValue chunk.[0u] with
        | ElementType.CModOpt
        | ElementType.CModReqd ->
            let modifiers = ImmutableArray.CreateBuilder<ParsedCustomMod>()
            let rec inner (chunk: ChunkedMemory) =
                match LanguagePrimitives.EnumOfValue chunk.[0u] with
                | ElementType.CModOpt
                | ElementType.CModReqd as elem' ->
                    match typeDefOrRefOrSpec 1u &chunk with
                    | Ok(chunk', mtype) ->
                        { ModifierRequired = (elem' = ElementType.CModReqd)
                          ModifierType = mtype }
                        |> modifiers.Add
                        inner chunk'
                    | Error err -> Error err
                | _ -> Ok(struct(chunk, modifiers.ToImmutable()))
            inner chunk
        | _ -> Ok(struct(chunk, ImmutableArray.Empty))

    let rec _etype (chunk, value: outref<uint32>) =
        ()

    let rec etype (chunk: inref<ChunkedMemory>) = // TODO: Also return the remaining chunk
        match LanguagePrimitives.EnumOfValue chunk.[0u] with
        | ElementType.Boolean -> Ok EncodedType.Boolean
        | ElementType.Char -> Ok EncodedType.Char
        | ElementType.I1 -> Ok EncodedType.I1
        | ElementType.U1 -> Ok EncodedType.U1
        | ElementType.I2 -> Ok EncodedType.I2
        | ElementType.U2 -> Ok EncodedType.U2
        | ElementType.I4 -> Ok EncodedType.I4
        | ElementType.U4 -> Ok EncodedType.U4
        | ElementType.I8 -> Ok EncodedType.I8
        | ElementType.U8 -> Ok EncodedType.U8
        | ElementType.R4 -> Ok EncodedType.R4
        | ElementType.R8 -> Ok EncodedType.R8
        | ElementType.I -> Ok EncodedType.I
        | ElementType.U -> Ok EncodedType.U
        | ElementType.Object -> Ok EncodedType.Object
        | ElementType.String -> Ok EncodedType.String
        | _ -> Ok EncodedType.String // TEMPORARY to get things to work
        // TODO: Read compressed integer for Var and Mvar

    let rec fieldSig (chunk: inref<ChunkedMemory>) =
        match chunk.[0u] with
        | 0x6uy ->
            let modifiers = chunk.Slice 1u
            match cmodifier &modifiers with
            | Ok(chunk', modifiers') ->
                match ensureLastItem(invalidOp "Read type") with
                | Ok ftype -> Ok { CustomModifiers = modifiers'; FieldType = ftype }
                | Error err -> Error err
            | Error err -> Error err
        | bad -> Error(InvalidFieldSignatureMagic bad)

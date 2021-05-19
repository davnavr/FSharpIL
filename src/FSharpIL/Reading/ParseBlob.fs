namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata

[<IsReadOnly; Struct>]
type ParsedCustomMod = { ModifierRequired: bool; ModifierType: ParsedTypeDefOrRefOrSpec }

/// <summary>Represents a type parsed from a signature in the <c>#Blob</c> metadata heap (II.23.2.12).</summary>
[<IsReadOnly; Struct>]
type ParsedType =
    internal { TypeTag: ElementType; Chunk: ChunkedMemory }
    member this.Tag = this.TypeTag

[<IsReadOnly; Struct>]
type ParsedFieldSig = { CustomModifiers: ImmutableArray<ParsedCustomMod>; FieldType: ParsedType }

[<RequireQualifiedAccess>]
module internal ParseBlob =
    let inline ensureLastItem (chunk: inref<ChunkedMemory>) result =
        match result with
        | Ok value when chunk.IsEmpty -> Ok value
        | Ok _ -> Error(ExpectedEndOfBlob(invalidOp "TODO: Add error information for expected blob end", invalidOp "", invalidOp ""))
        | Error err -> Error err

    // TODO: How to return size as well?
    let compressedUnsigned (chunk: byref<ChunkedMemory>) =
        let parsed = chunk.[0u]
        if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
            chunk <- chunk.Slice 1u
            Ok(struct(1uy, uint32(parsed &&& 0b0111_1111uy)))
        elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
            match chunk.TrySlice 2u with
            | true, chunk' ->
                let value = (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.[1u])
                chunk <- chunk'
                Ok(struct(2uy, uint32 value))
            | false, _ -> Error(CompressedIntegerOutOfBounds 2u)
        elif parsed &&& 0b1110_0000uy = 0b1100_0000uy then // 4 bytes
            match chunk.TrySlice 4u with
            | true, chunk' ->
                let value =
                    (uint32(parsed &&& 0b0001_1111uy) <<< 24)
                    + (uint32(chunk.[1u]) <<< 16)
                    + (uint32(chunk.[2u]) <<< 8)
                    + uint32(chunk.[3u])
                chunk <- chunk'
                Ok(struct(4uy, value))
            | false, _ -> Error(CompressedIntegerOutOfBounds 4u)
        else Error(InvalidUnsignedCompressedIntegerKind parsed)

    let typeDefOrRefOrSpec (chunk: byref<ChunkedMemory>) =
        match compressedUnsigned &chunk with
        | Ok(_, value) ->
            { Tag = LanguagePrimitives.EnumOfValue(uint8(value &&& 0b11u))
              TypeIndex = value >>> 2 }
            |> Ok
        | Error err -> Error err

    let isModifierElem (chunk: byref<ChunkedMemory>) =
        let value = LanguagePrimitives.EnumOfValue chunk.[0u]
        value = ElementType.CModOpt || value = ElementType.CModReqd

    let rec customMod (chunk: byref<ChunkedMemory>) (modifiers: ImmutableArray<ParsedCustomMod>.Builder) =
        match LanguagePrimitives.EnumOfValue chunk.[0u] with
        | ElementType.CModOpt
        | ElementType.CModReqd as elem ->
            let modifiers' =
                match modifiers with
                | null -> ImmutableArray.CreateBuilder()
                | _ -> modifiers
            match typeDefOrRefOrSpec &chunk with
            | Ok mtype ->
                { ModifierRequired = (elem = ElementType.CModReqd)
                  ModifierType = mtype }
                |> modifiers'.Add
                customMod &chunk modifiers'
            | Error err -> Error err
        | _ when isNull modifiers -> Ok ImmutableArray.Empty
        | _ -> Ok(modifiers.ToImmutable())

    let rec etype (chunk: byref<ChunkedMemory>) =
        // TODO: Check that chunk is not empty.
        let elem = LanguagePrimitives.EnumOfValue chunk.[0u]
        chunk <- chunk.Slice 1u
        match elem with
        | ElementType.Boolean
        | ElementType.Char
        | ElementType.I1
        | ElementType.U1
        | ElementType.I2
        | ElementType.U2
        | ElementType.I4
        | ElementType.U4
        | ElementType.I8
        | ElementType.U8
        | ElementType.R4
        | ElementType.R8
        | ElementType.I
        | ElementType.U
        | ElementType.Object
        | ElementType.String -> Ok { TypeTag = elem; Chunk = ChunkedMemory.empty }
        | ElementType.Class
        | ElementType.ValueType ->
            match typeDefOrRefOrSpec &chunk with
            | Ok t ->
                Ok { TypeTag = elem; Chunk = ChunkedMemory.empty } // TODO: How to store type?
            | Error err -> Error err
        | ElementType.Var
        | ElementType.MVar ->
            // TODO: Avoid calculating value of generic param index to be more efficient.
            // Maybe consider storing index in special chunk instead? Sum all bytes in chunk to get index?
            match compressedUnsigned &chunk with
            | Ok(Convert.U4 size, _) ->
                match chunk.TrySlice(0u, size) with
                | true, num ->
                    chunk <- chunk.Slice size
                    Ok { TypeTag = elem; Chunk = num }
                | false, _ -> Error(CompressedIntegerOutOfBounds size)
            | Error err -> Error err
        | _ -> Error(InvalidElementType elem)

    let rec fieldSig (chunk: inref<ChunkedMemory>) =
        // TODO: Check if chunk is not empty.
        match chunk.[0u] with
        | 0x6uy ->
            let mutable chunk' = chunk.Slice 1u
            match customMod &chunk' null with
            | Ok modifiers ->
                match etype &chunk' with
                | Ok ftype -> Ok { CustomModifiers = modifiers; FieldType = ftype }
                | Error err -> Error err
            | Error err -> Error err
        | bad -> Error(InvalidFieldSignatureMagic bad)

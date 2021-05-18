[<RequireQualifiedAccess>]
module internal FSharpIL.Reading.ParseBlob

open System
open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata

/// Reads a variable-width, unsigned big-endian integer (II.23.2).
let tryReadUnsigned offset (chunk: inref<ChunkedMemory>) (value: outref<uint32>) = // TODO: Move bit shifting logic to ByteParser.fs
    let parsed = chunk.[offset]
    if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
        value <- uint32(parsed &&& 0b0111_1111uy)
        Ok 1uy
    elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
        value <- (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.[offset + 1u]) |> uint32
        Ok 2uy
    elif parsed &&& 0b1110_0000uy = 0b1100_0000uy then // 4 bytes
        value <-
            (uint32(parsed &&& 0b0001_1111uy) <<< 24)
            + (uint32(chunk.[offset + 1u]) <<< 16)
            + (uint32(chunk.[offset + 2u]) <<< 8)
            + uint32(chunk.[offset + 3u])
        Ok 4uy
    else Error parsed

let private cmodifier (chunk: inref<ChunkedMemory>) =
    match LanguagePrimitives.EnumOfValue chunk.[0u] with
    | ElementType.CModOpt
    | ElementType.CModReqd ->
        let modifiers = ImmutableArray.CreateBuilder<CustomModifier>()
        let rec inner (chunk: ChunkedMemory) i =
            match LanguagePrimitives.EnumOfValue chunk.[i] with
            | ElementType.CModOpt
            | ElementType.CModReqd as elem' ->
                modifiers.Add(CustomModifier((elem' = ElementType.CModReqd), failwith "TODO: read modifying type"))
                inner chunk (i + failwith "length of type")
            | _ -> struct(chunk.Slice i, modifiers.ToImmutable())
        inner chunk 0u
    | _ -> struct(chunk, ImmutableArray.Empty)

let etype (chunk: inref<ChunkedMemory>) = // TODO: Also return the remaining chunk
    match LanguagePrimitives.EnumOfValue chunk.[0u] with
    | ElementType.Boolean -> EncodedType.Boolean
    | ElementType.Char -> EncodedType.Char
    | ElementType.I1 -> EncodedType.I1
    | ElementType.U1 -> EncodedType.U1
    | ElementType.I2 -> EncodedType.I2
    | ElementType.U2 -> EncodedType.U2
    | ElementType.I4 -> EncodedType.I4
    | ElementType.U4 -> EncodedType.U4
    | ElementType.I8 -> EncodedType.I8
    | ElementType.U8 -> EncodedType.U8
    | ElementType.R4 -> EncodedType.R4
    | ElementType.R8 -> EncodedType.R8
    | ElementType.I -> EncodedType.I
    | ElementType.U -> EncodedType.U
    | ElementType.Object -> EncodedType.Object
    | ElementType.String -> EncodedType.String
    | _ -> EncodedType.String // TEMPORARY to get things to work
    // TODO: Read compressed integer for Var and Mvar

let rec fieldSig (chunk: inref<ChunkedMemory>) =
    match chunk.[0u] with
    | 0x6uy ->
        let modifiers = chunk.Slice 1u
        let struct (chunk', modifiers') = cmodifier &modifiers
        FieldSignature(modifiers', etype &chunk') |> Ok
    | bad -> Error(InvalidFieldSignatureMagic bad)

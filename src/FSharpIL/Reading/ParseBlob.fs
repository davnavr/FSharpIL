[<RequireQualifiedAccess>]
module internal FSharpIL.Reading.ParseBlob

open System
open System.Collections.Immutable

open FSharpIL.Metadata

/// Reads a variable-width, unsigned big-endian integer (II.23.2).
let tryReadUnsigned offset (chunk: ChunkReader) (value: outref<uint32>) = // TODO: Move bit shifting logic to ByteParser.fs
    let parsed = chunk.ReadU1 offset
    if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
        value <- uint32(parsed &&& 0b0111_1111uy)
        Ok 1uy
    elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
        value <- (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.ReadU1(offset + 1UL)) |> uint32
        Ok 2uy
    elif parsed &&& 0b1110_0000uy = 0b1100_0000uy then // 4 bytes
        value <-
            (uint32(parsed &&& 0b0001_1111uy) <<< 24)
            + (uint32(chunk.ReadU1(offset + 1UL)) <<< 16)
            + (uint32(chunk.ReadU1(offset + 2UL)) <<< 8)
            + uint32(chunk.ReadU1(offset + 3UL))
        Ok 4uy
    else Error parsed

let rec private cmodifier i (buffer: Span<byte>) (modifiers: ImmutableArray<CustomModifier>.Builder) =
    let elem = LanguagePrimitives.EnumOfValue buffer.[i]
    match elem with
    | ElementType.CModOpt
    | ElementType.CModReqd ->
        modifiers.Add(CustomModifier((elem = ElementType.CModReqd), failwith "TODO: read modifying type"))
        cmodifier (i + failwith "length of type") buffer modifiers
    | _ -> struct(i, modifiers.ToImmutable())

let etype i (buffer: Span<byte>) =
    match LanguagePrimitives.EnumOfValue buffer.[i] with
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

let rec fieldSig (buffer: Span<byte>) =
    match buffer.[0] with
    | 0x6uy ->
        let struct (i, modifiers) = cmodifier 1 buffer (ImmutableArray.CreateBuilder())
        FieldSignature(modifiers, etype i buffer) |> Ok
    | bad -> Error(InvalidFieldSignatureMagic bad)



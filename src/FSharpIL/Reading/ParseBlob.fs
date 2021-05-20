namespace FSharpIL.Reading

open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata

type IBlobParser<'Item when 'Item : struct> = abstract Parse: 'Item -> unit

[<IsReadOnly; Struct>]
type ParsedCustomMod = { ModifierRequired: bool; ModifierType: ParsedTypeDefOrRefOrSpec }

type ICustomModParser = IBlobParser<ParsedCustomMod>

type IEncodedTypeParser = interface
    abstract Class: ParsedTypeDefOrRefOrSpec -> unit
    abstract MVar: uint32 -> unit
    // TODO: Argument should be union type of all primitive element types.
    abstract Primitive: ElementType -> unit
    abstract ValueType: ParsedTypeDefOrRefOrSpec -> unit
    abstract Var: uint32 -> unit
end

type IFieldSigParser<'CustomModifiers, 'FieldType when 'CustomModifiers :> ICustomModParser and 'FieldType :> IEncodedTypeParser> =
    abstract CustomModifiers: 'CustomModifiers
    abstract FieldType: 'FieldType

[<RequireQualifiedAccess>]
module internal ParseBlob =
    let inline parse (parser: #IBlobParser<_>) item = parser.Parse item

    let compressedu (chunk: byref<ChunkedMemory>) =
        let parsed = chunk.[0u]
        if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
            chunk <- chunk.Slice 1u
            Ok(uint32(parsed &&& 0b0111_1111uy))
        elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
            match chunk.TrySlice 2u with
            | true, chunk' ->
                let value = (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.[1u])
                chunk <- chunk'
                Ok(uint32 value)
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
                Ok value
            | false, _ -> Error(CompressedIntegerOutOfBounds 4u)
        else Error(InvalidUnsignedCompressedIntegerKind parsed)

    let typeDefOrRefOrSpec (chunk: byref<ChunkedMemory>) =
        match compressedu &chunk with
        | Ok value ->
            { Tag = LanguagePrimitives.EnumOfValue(uint8(value &&& 0b11u))
              TypeIndex = value >>> 2 }
            |> Ok
        | Error err -> Error err

    let rec cmodifier (chunk: byref<ChunkedMemory>) (parser: #ICustomModParser) =
        match LanguagePrimitives.EnumOfValue chunk.[0u] with
        | ElementType.CModOpt
        | ElementType.CModReqd as elem ->
            match typeDefOrRefOrSpec &chunk with
            | Ok mtype ->
                { ModifierRequired = (elem = ElementType.CModReqd)
                  ModifierType = mtype }
                |> parse parser
                cmodifier &chunk parser
            | Error err -> Some err
        | _ -> None

    let rec etype (chunk: byref<ChunkedMemory>) (parser: #IEncodedTypeParser) =
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
        | ElementType.String ->
            parser.Primitive elem
            None
        | ElementType.Class
        | ElementType.ValueType ->
            match typeDefOrRefOrSpec &chunk with
            | Ok t -> 
                match elem with
                | ElementType.Class -> parser.Class t
                | ElementType.ValueType
                | _ -> parser.ValueType t
                None
            | Error err -> Some err
        | _ -> Some(InvalidElementType elem)

    let rec fieldSig (chunk: inref<ChunkedMemory>) (parser: #IFieldSigParser<_, _>) =
        // TODO: Check if chunk is not empty.
        match chunk.[0u] with
        | 0x6uy ->
            let mutable chunk' = chunk.Slice 1u
            match cmodifier &chunk' parser.CustomModifiers with
            | None -> etype &chunk' parser.FieldType
            | Some err -> Some err
        | bad -> Some(InvalidFieldSignatureMagic bad)

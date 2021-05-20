namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type CustomModKind =
    | OptionalModfier
    | RequiredModifier

[<IsReadOnly; Struct>]
type ParsedCustomMod = { ModifierRequired: CustomModKind; ModifierType: ParsedTypeDefOrRefOrSpec }

// TODO: Avoid duplicate code with ElementType used in metadata building.
/// Indicates whether a user-defined type is a reference type or a value type.
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type DefinedTypeKind =
    | Class
    | ValueType

/// <summary>Represents a type parsed from a signature in the <c>#Blob</c> metadata heap (II.23.2.12).</summary>
[<RequireQualifiedAccess>]
type ParsedType =
    | Boolean
    | Char
    | I1
    | U1
    | I2
    | U2
    | I4
    | U4
    | I8
    | U8
    | R4
    | R8
    | I
    | U
    | Array of ParsedType * ArrayShape
    | Class of ParsedTypeDefOrRefOrSpec
    //| FnPtr of 
    | GenericInst of DefinedTypeKind * ParsedTypeDefOrRefOrSpec * ImmutableArray<ParsedType>
    | MVar of number: uint32
    | Object
    | Ptr of ImmutableArray<ParsedCustomMod> * ParsedType voption
    | String
    | SZArray of ImmutableArray<ParsedCustomMod> * ParsedType
    | ValueType of ParsedTypeDefOrRefOrSpec
    | Var of number: uint32

[<IsReadOnly; Struct>]
type ParsedFieldSig = { CustomModifiers: ImmutableArray<ParsedCustomMod>; FieldType: ParsedType }

type ParsedParamOrRetType =
    | ParsedType of ParsedType
    | ParsedByRef of ParsedType
    | ParsedTypedByRef

type ParsedRetType =
    | RetType of ParsedParamOrRetType
    | RetVoid

[<IsReadOnly; Struct>]
type ParsedParam = { CustomModifiers: ImmutableArray<ParsedCustomMod>; ParamType: ParsedParamOrRetType }

// TODO: Figure out if EXPLICITTHIS implies HASTHIS
[<RequireQualifiedAccess>]
type ParsedMethodThis =
    | NoThis
    | HasThis
    | ExplicitThis

[<IsReadOnly; Struct>]
type ParsedMethodDefSig =
    { This: ParsedMethodThis
      CallingConvention: MethodCallingConventions
      ReturnType: struct(ImmutableArray<ParsedCustomMod> * ParsedRetType)
      Parameters: ImmutableArray<ParsedParam> }

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
                { ModifierRequired =
                    match elem with
                    | ElementType.CModReqd -> CustomModKind.RequiredModifier
                    | ElementType.CModOpt
                    | _ -> CustomModKind.OptionalModfier
                  ModifierType = mtype }
                |> modifiers'.Add
                customMod &chunk modifiers'
            | Error err -> Error err
        | _ when isNull modifiers -> Ok ImmutableArray.Empty
        | _ -> Ok(modifiers.ToImmutable())

    let inline customModList (chunk: byref<_>) = customMod &chunk null

    let rec etype (chunk: byref<ChunkedMemory>) =
        // TODO: Check that chunk is not empty.
        let elem = LanguagePrimitives.EnumOfValue chunk.[0u]
        chunk <- chunk.Slice 1u
        match elem with
        | ElementType.Boolean -> Ok ParsedType.Boolean
        | ElementType.Char -> Ok ParsedType.Char
        | ElementType.I1 -> Ok ParsedType.I1
        | ElementType.U1 -> Ok ParsedType.U1
        | ElementType.I2 -> Ok ParsedType.I2
        | ElementType.U2 -> Ok ParsedType.U2
        | ElementType.I4 -> Ok ParsedType.I4
        | ElementType.U4 -> Ok ParsedType.U4
        | ElementType.I8 -> Ok ParsedType.I8
        | ElementType.U8 -> Ok ParsedType.U8
        | ElementType.R4 -> Ok ParsedType.R4
        | ElementType.R8 -> Ok ParsedType.R8
        | ElementType.I -> Ok ParsedType.I
        | ElementType.U -> Ok ParsedType.U
        | ElementType.Object -> Ok ParsedType.Object
        | ElementType.String -> Ok ParsedType.String
        | ElementType.Class
        | ElementType.ValueType ->
            match typeDefOrRefOrSpec &chunk with
            | Ok t ->
                let tag =
                    match elem with
                    | ElementType.Class -> ParsedType.Class
                    | ElementType.ValueType
                    | _ -> ParsedType.ValueType
                Ok(tag t)
            | Error err -> Error err
        | ElementType.Var
        | ElementType.MVar ->
            match compressedUnsigned &chunk with
            | Ok(_, num) ->
                let tag =
                    match elem with
                    | ElementType.Var -> ParsedType.Var
                    | ElementType.MVar
                    | _ -> ParsedType.MVar
                Ok(tag num)
            | Error err -> Error err
        | ElementType.SZArray ->
            match customModList &chunk with
            | Ok modifiers ->
                match etype &chunk with
                | Ok t -> Ok(ParsedType.SZArray(modifiers, t))
                | Error err -> Error err
            | Error err -> Error err
        | ElementType.Ptr ->
            match customModList &chunk with
            | Ok modifiers ->
                match LanguagePrimitives.EnumOfValue chunk.[0u] with
                | ElementType.Void ->
                    chunk <- chunk.Slice 1u
                    Ok(ParsedType.Ptr(modifiers, ValueNone))
                | _ ->
                    match etype &chunk with
                    | Ok t -> Ok(ParsedType.Ptr(modifiers, ValueSome t))
                    | Error err -> Error err
            | Error err -> Error err
        | ElementType.GenericInst -> genericInst &chunk
        //| ElementType.Array
        | _ -> Error(InvalidElementType elem)

    and genArgList (chunk: byref<ChunkedMemory>) (args: ImmutableArray<ParsedType>.Builder) =
        if args.Capacity = args.Count
        then Ok(args.ToImmutable())
        else
            match etype &chunk with
            | Ok arg ->
                args.Add arg
                genArgList &chunk args
            | Error err -> Error err

    and genericInst (chunk: byref<ChunkedMemory>) =
        match chunk.TrySlice(0u, 1u) with
        | true, chunk' ->
            chunk <- chunk.Slice 1u
            match LanguagePrimitives.EnumOfValue chunk'.[0u] with
            | ElementType.Class
            | ElementType.ValueType as kind ->
                match typeDefOrRefOrSpec &chunk with
                | Ok t ->
                    match compressedUnsigned &chunk with
                    | Ok(_, 0u) -> Error MissingGenericArguments
                    | Ok(_, Convert.I4 count) ->
                        match genArgList &chunk (ImmutableArray.CreateBuilder count) with
                        | Ok args ->
                            let kind' =
                                match kind with
                                | ElementType.Class -> DefinedTypeKind.Class
                                | ElementType.ValueType
                                | _ -> DefinedTypeKind.ValueType
                            Ok(ParsedType.GenericInst(kind', t, args))
                        | Error err -> Error err
                    | Error err -> Error err
                | Error err -> Error err
            | kind -> Error(InvalidGenericInstantiationKind(ValueSome kind))
        | false, _ -> Error(InvalidGenericInstantiationKind ValueNone)

    let rec fieldSig (chunk: inref<ChunkedMemory>) =
        // TODO: Check if chunk is not empty.
        match chunk.[0u] with
        | 0x6uy ->
            let mutable chunk' = chunk.Slice 1u
            match customModList &chunk' with
            | Ok modifiers ->
                match etype &chunk' with
                | Ok ftype -> Ok { CustomModifiers = modifiers; FieldType = ftype }
                | Error err -> Error err
            | Error err -> Error err
        | bad -> Error(InvalidFieldSignatureMagic bad)

    let private paramOrRetType (chunk: byref<ChunkedMemory>) =
        match customModList &chunk with
        | Ok modifiers ->
            match etype &chunk with
            | Ok t -> struct(modifiers, Ok(ParsedType t))
            | Error(InvalidElementType ElementType.ByRef) ->
                match etype &chunk with
                | Ok t -> struct(modifiers, Ok(ParsedByRef t))
                | Error err -> struct(ImmutableArray.Empty, Error err)
            | Error(InvalidElementType ElementType.TypedByRef) -> struct(modifiers, Ok ParsedTypedByRef)
            | Error err -> struct(modifiers, Error err)
        | Error err -> struct(ImmutableArray.Empty, Error err)

    let retType (chunk: byref<ChunkedMemory>) =
        let struct(modifiers, t) = paramOrRetType &chunk
        match t with
        | Ok t' -> Ok(struct(modifiers, RetType t'))
        | Error(InvalidElementType ElementType.Void) -> Ok(struct(modifiers, RetVoid))
        | Error err -> Error err

    let param (chunk: byref<ChunkedMemory>) =
        match paramOrRetType &chunk with
        | modifiers, Ok ptype ->  Ok { CustomModifiers = modifiers; ParamType = ptype }
        | _, Error err -> Error err

    let private methodDefOrRefCallingConvention (chunk: byref<ChunkedMemory>) =
        ()

    //let methodRefSig chunk =
    //    failwith "TODO: Can reuse methodDefSig function, just check for var args"

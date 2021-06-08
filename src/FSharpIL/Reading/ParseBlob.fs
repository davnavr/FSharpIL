module FSharpIL.Reading.ParseBlob

open System
open System.Collections.Immutable

open FSharpIL
open FSharpIL.Utilities

open FSharpIL.Metadata.Tables

open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

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
        TypeDefOrRef(LanguagePrimitives.EnumOfValue(uint8(value &&& 0b11u)), value >>> 2 ) |> Ok
    | Error err -> Error err

let rec customMod (chunk: byref<ChunkedMemory>) (modifiers: ImmutableArray<CustomMod>.Builder) =
    match LanguagePrimitives.EnumOfValue chunk.[0u] with
    | ElementType.CModOpt
    | ElementType.CModReqd as elem ->
        let modifiers' =
            match modifiers with
            | null -> ImmutableArray.CreateBuilder()
            | _ -> modifiers
        match typeDefOrRefOrSpec &chunk with
        | Ok mtype ->
            modifiers'.Add { Required = elem = ElementType.CModReqd; ModifierType = mtype }
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
    | ElementType.Class
    | ElementType.ValueType ->
        match typeDefOrRefOrSpec &chunk with
        | Ok t ->
            match elem with
            | ElementType.Class -> EncodedType.Class t
            | ElementType.ValueType
            | _ -> EncodedType.ValueType t
            |> Ok
        | Error err -> Error err
    | ElementType.Var
    | ElementType.MVar ->
        match compressedUnsigned &chunk with
        | Ok(_, num) ->
            match elem with
            | ElementType.Var -> EncodedType.Var num
            | ElementType.MVar
            | _ -> EncodedType.MVar num
            |> Ok
        | Error err -> Error err
    | ElementType.SZArray ->
        match customModList &chunk with
        | Ok modifiers ->
            match etype &chunk with
            | Ok t -> Ok(EncodedType.SZArray(modifiers, t))
            | Error err -> Error err
        | Error err -> Error err
    | ElementType.Ptr ->
        match customModList &chunk with
        | Ok modifiers ->
            match LanguagePrimitives.EnumOfValue chunk.[0u] with
            | ElementType.Void ->
                chunk <- chunk.Slice 1u
                Pointer.Void modifiers |> EncodedType.Ptr |> Ok
            | _ ->
                match etype &chunk with
                | Ok t -> Pointer.Type(modifiers, t) |> EncodedType.Ptr |> Ok
                | Error err -> Error err
        | Error err -> Error err
    | ElementType.GenericInst -> genericInst &chunk
    //| ElementType.Array
    | _ -> Error(InvalidElementType elem)

and genArgList (chunk: byref<ChunkedMemory>) (args: ImmutableArray<EncodedType>.Builder) =
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
                        { IsValueType = kind = ElementType.ValueType
                          GenericType = t
                          GenericArguments = { GenArgs = args } }
                        |> EncodedType.GenericInst
                        |> Ok
                    | Error err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        | kind -> Error(InvalidGenericInstantiationKind(ValueSome kind))
    | false, _ -> Error(InvalidGenericInstantiationKind ValueNone)

let fieldSig (chunk: inref<ChunkedMemory>) =
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

type ParamOrRetType =
    | Type of EncodedType
    | ByRef of EncodedType
    | TypedByRef

let paramOrRetType (chunk: byref<ChunkedMemory>) =
    match customModList &chunk with
    | Ok modifiers ->
        match etype &chunk with
        | Ok t -> struct(modifiers, Ok(Type t))
        | Error(InvalidElementType ElementType.ByRef) ->
            match etype &chunk with
            | Ok t -> struct(modifiers, Ok(ByRef t))
            | Error err -> struct(ImmutableArray.Empty, Error err)
        | Error(InvalidElementType ElementType.TypedByRef) -> struct(modifiers, Ok TypedByRef)
        | Error err -> struct(modifiers, Error err)
    | Error err -> struct(ImmutableArray.Empty, Error err)

let retType (chunk: byref<ChunkedMemory>) =
    let struct(modifiers, t) = paramOrRetType &chunk
    match t with
    | Ok(Type t') -> Ok(ReturnType.Type(modifiers, t'))
    | Ok(ByRef t') -> Ok(ReturnType.ByRef(modifiers, t'))
    | Ok TypedByRef -> Ok(ReturnType.TypedByRef modifiers)
    | Error(InvalidElementType ElementType.Void) -> Ok(ReturnType.Void modifiers)
    | Error err -> Error err

let rec param (chunk: byref<ChunkedMemory>) (parameters: ImmutableArray<ParamItem>.Builder) =
    if parameters.Count < parameters.Capacity then
        match paramOrRetType &chunk with
        | modifiers, Ok ptype ->
            match ptype with
            | Type t -> ParamItem.Param(modifiers, t)
            | ByRef t -> ParamItem.ByRef(modifiers, t)
            | TypedByRef -> ParamItem.TypedByRef modifiers
            |> parameters.Add

            param &chunk parameters
        | _, Error err -> Error err
    else Ok(parameters.ToImmutable())

let paramList (chunk: byref<ChunkedMemory>) (Convert.I4 pcount) =
    match pcount with
    | 0 -> Ok ImmutableArray.Empty
    | _ -> param &chunk (ImmutableArray.CreateBuilder pcount)

let methodDefOrRefCallingConvention (chunk: byref<ChunkedMemory>) =
    match chunk.TrySlice(0u, 1u) with
    | true, cconv ->
        chunk <- chunk.Slice 1u
        let magic = cconv.[0u]
        let magic' = LanguagePrimitives.EnumOfValue magic
        let inline invalid() = Error(InvalidMethodSignatureCallingConvention(ValueSome magic))
        match LanguagePrimitives.EnumOfValue(magic &&& 0b11111uy) with
        | CallConvFlags.Default
        | CallConvFlags.VarArg
        | CallConvFlags.Generic as cconv' when magic' <= CallConvFlags.ExplicitThis ->
            let this =
                if magic'.HasFlag CallConvFlags.HasThis then
                    if magic'.HasFlag CallConvFlags.ExplicitThis
                    then Ok ExplicitThis
                    else Ok HasThis
                elif magic &&& 0b1110_0000uy = 0uy then Ok NoThis
                else invalid()
            match this with
            | Ok this' ->
                let gcount =
                    if cconv' = CallConvFlags.Generic then
                        match compressedUnsigned &chunk with
                        | Ok(_, 0u) -> Error MissingGenericArguments
                        | Ok(_, count) -> Ok count
                        | Error err -> Error err
                    else Ok 0u
                match gcount with
                | Ok 0u -> Ok(struct(this', if cconv' = CallConvFlags.VarArg then VarArg else Default))
                | Ok gcount' -> Ok(struct(this', Generic gcount'))
                | Error err -> Error err
            | Error err -> Error err
        | _ -> invalid()
    | false, _ -> Error(InvalidMethodSignatureCallingConvention ValueNone)

let methodDefSig (chunk: inref<ChunkedMemory>) =
    let mutable chunk = chunk
    match methodDefOrRefCallingConvention &chunk with
    | Ok(this, cconv) ->
        match compressedUnsigned &chunk with
        | Ok(_, pcount) ->
            match retType &chunk with
            | Ok ret ->
                match paramList &chunk pcount with
                | Ok parameters ->
                    { HasThis = this
                      CallingConvention = cconv
                      ReturnType = ret
                      Parameters = parameters }
                    |> Ok
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err
    | Error err -> Error err

//let methodRefSig chunk =
    //    failwith "TODO: Can reuse methodDefSig function, just check for var args"

let propertySig (chunk: inref<ChunkedMemory>) =
    if not chunk.IsEmpty then
        let magic = chunk.[0u]
        match magic with
        | 0x8uy // PROPERTY
        | 0x28uy -> // PROPERTY ||| HASTHIS
            let mutable chunk = chunk.Slice 1u
            match customModList &chunk with
            | Ok modifiers ->
                match compressedUnsigned &chunk with
                | Ok(_, pcount) ->
                    match etype &chunk with
                    | Ok ptype ->
                        match paramList &chunk pcount with
                        | Ok parameters ->
                            { HasThis = magic &&& 0x20uy <> 0uy
                              CustomModifiers = modifiers
                              PropertyType = ptype
                              Parameters = parameters }
                            |> Ok
                        | Error err -> Error err
                    | Error err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        | _ -> Error(InvalidPropertyMagic(ValueSome magic))
    else Error(InvalidPropertyMagic ValueNone)

// TODO: Many additional typespecs are allowed by ECMA-335 augmentations (https://github.com/dotnet/runtime/blob/main/docs/design/specs/Ecma-335-Augments.md).
    // but prevent usage of CLASS or VALUETYPE?
let typeSpec (chunk: inref<ChunkedMemory>) =
    let mutable chunk = chunk
    etype &chunk

let elemThingReplaceWithChunkMethod (length: uint32) (chunk: byref<ChunkedMemory>) =
    match chunk.TrySlice(0u, length) with
    | true, elem ->
        chunk <- chunk.Slice length
        Ok elem
    | false, _ -> Error(failwith "not enough room for elem in blob")

let rec fixedArg
    (fixedArgs: ImmutableArray<ElemType>)
    (arguments: ImmutableArray<FixedArg>.Builder)
    (chunk: byref<ChunkedMemory>)
    =
    if arguments.Count < fixedArgs.Length then
        let elem =
            match fixedArgs.[arguments.Count] with
            | ElemType.Type PrimitiveElemType.Bool ->
                match elemThingReplaceWithChunkMethod 1u &chunk with
                | Ok elem ->
                    ValBool(Convert.ToBoolean elem.[0u])
                    |> FixedArg.Elem
                    |> Ok
                | Error err -> Error err
            | _ ->
                failwith "TODO: Parse elem after determining type of argument"
        match elem with
        | Ok elem' ->
            arguments.Add elem'
            fixedArg fixedArgs arguments &chunk
        | Error err -> Error err
    else Ok(arguments.ToImmutable())

let rec namedArg
    (arguments: ImmutableArray<NamedArg>.Builder)
    (chunk: byref<ChunkedMemory>)
    =
    if arguments.Capacity <> arguments.Count then
        failwith "a"
    else Ok(arguments.ToImmutable())

let namedArgList fixedArgs (chunk: byref<ChunkedMemory>) =
    match chunk.TrySlice(0u, 2u) with
    | true, numNamedArgs ->
        chunk <- chunk.Slice 2u
        let namedArgs' =
            match ChunkedMemory.readU2 0u &numNamedArgs with
            | 0us -> Ok ImmutableArray.Empty
            | Convert.I4 numNamedArgs' -> namedArg (ImmutableArray.CreateBuilder numNamedArgs') &chunk
        match namedArgs' with
        | Ok namedArgs'' -> Ok { FixedArgs = fixedArgs; NamedArgs = namedArgs'' }
        | Error err -> Error err
    | false, _ -> Error MissingNamedArgumentCount

let fixedArgList (fixedArgs: ImmutableArray<_>) (chunk: byref<ChunkedMemory>) =
    let fixedArgs' =
        if fixedArgs.IsEmpty
        then Ok ImmutableArray.Empty
        else fixedArg fixedArgs (ImmutableArray.CreateBuilder fixedArgs.Length) &chunk
    match fixedArgs' with
    | Ok fixedArgs'' -> namedArgList fixedArgs'' &chunk
    | Error err -> Error err

let customAttrib fixedArgTypes (chunk: inref<ChunkedMemory>) =
    match chunk.TrySlice 2u with
    | true, chunk' ->
        match ChunkedMemory.readU2 0u &chunk with
        | 1us ->
            let mutable chunk' = chunk'
            fixedArgList fixedArgTypes &chunk'
        | prolog -> Error(InvalidCustomAttributeProlog(ValueSome prolog))
    | false, _ -> Error(InvalidCustomAttributeProlog ValueNone)

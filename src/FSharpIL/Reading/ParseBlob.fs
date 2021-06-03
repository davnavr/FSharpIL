// TODO: Make main parsing functions publically available.
module FSharpIL.Reading.ParseBlob

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Utilities

open FSharpIL.Metadata

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

let rec param (chunk: byref<ChunkedMemory>) (parameters: ImmutableArray<ParsedParam>.Builder) =
        if parameters.Count < parameters.Capacity then
            match paramOrRetType &chunk with
            | modifiers, Ok ptype ->
                parameters.Add { CustomModifiers = modifiers; ParamType = ptype }
                param &chunk parameters
            | _, Error err -> Error err
        else Ok(parameters.ToImmutable())

let paramList (chunk: byref<ChunkedMemory>) (Convert.I4 pcount) =
    match pcount with
    | 0 -> Ok ImmutableArray.Empty
    | _ -> param &chunk (ImmutableArray.CreateBuilder pcount)

let private methodDefOrRefCallingConvention (chunk: byref<ChunkedMemory>) =
        match chunk.TrySlice(0u, 1u) with
        | true, cconv ->
            chunk <- chunk.Slice 1u
            let magic = cconv.[0u]
            let magic' = LanguagePrimitives.EnumOfValue magic
            let inline invalid() = Error(InvalidMethodSignatureCallingConvention(ValueSome magic))
            match LanguagePrimitives.EnumOfValue(magic &&& 0b11111uy) with
            | CallingConvention.Default
            | CallingConvention.VarArg
            | CallingConvention.Generic as cconv' when magic' <= CallingConvention.ExplicitThis ->
                let this =
                    if magic'.HasFlag CallingConvention.HasThis then
                        if magic'.HasFlag CallingConvention.ExplicitThis
                        then Ok ParsedMethodThis.ExplicitThis
                        else Ok ParsedMethodThis.HasThis
                    elif magic &&& 0b1110_0000uy = 0uy then Ok ParsedMethodThis.NoThis
                    else invalid()
                match this with
                | Ok this' ->
                    let gcount =
                        if cconv' = CallingConvention.Generic then
                            match compressedUnsigned &chunk with
                            | Ok(_, 0u) -> Error MissingGenericArguments
                            | Ok(_, count) -> Ok count
                            | Error err -> Error err
                        else Ok 0u
                    match gcount with
                    | Ok 0u ->
                        Ok (
                            struct (
                                this',
                                if cconv' = CallingConvention.VarArg then VarArg else Default
                            )
                        )
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

let private elemThingReplaceWithChunkMethod (length: uint32) (chunk: byref<ChunkedMemory>) =
    match chunk.TrySlice(0u, length) with
    | true, elem ->
        chunk <- chunk.Slice length
        Ok elem
    | false, _ -> Error(failwith "not enough room for elem in blob")

let rec private fixedArg
    (fixedArgs: ImmutableArray<ParsedElemType>)
    (arguments: ImmutableArray<ParsedFixedArg>.Builder)
    (chunk: byref<ChunkedMemory>)
    =
    if arguments.Count < fixedArgs.Length then
        let elem =
            match fixedArgs.[arguments.Count] with
            | ParsedElemType.ElemType(PrimitiveElemType.Bool) ->
                match elemThingReplaceWithChunkMethod 1u &chunk with
                | Ok elem ->
                    Ok(ParsedFixedArg.Bool elem.[0u])
                | Error err -> Error err
            | _ ->
                failwith "TODO: Parse elem after determining type of argument"
        match elem with
        | Ok elem' ->
            arguments.Add elem'
            fixedArg fixedArgs arguments &chunk
        | Error err -> Error err
    else Ok(arguments.ToImmutable())

let rec private namedArg
    (arguments: ImmutableArray<ParsedNamedArg>.Builder)
    (chunk: byref<ChunkedMemory>)
    =
    if arguments.Capacity <> arguments.Count then
        failwith "a"
    else Ok(arguments.ToImmutable())

let private namedArgList fixedArgs (chunk: byref<ChunkedMemory>) =
    match chunk.TrySlice(0u, 2u) with
    | true, numNamedArgs ->
        chunk <- chunk.Slice 2u
        let namedArgs' =
            match ChunkedMemory.readU2 0u &numNamedArgs with
            | 0us -> Ok ImmutableArray.Empty
            | Convert.I4 numNamedArgs' -> namedArg (ImmutableArray.CreateBuilder numNamedArgs') &chunk
        match namedArgs' with
        | Ok namedArgs'' -> Ok { FixedArguments = fixedArgs; NamedArguments = namedArgs'' }
        | Error err -> Error err
    | false, _ -> Error MissingNamedArgumentCount

let private fixedArgList (fixedArgs: ImmutableArray<_>) (chunk: byref<ChunkedMemory>) =
    let fixedArgs' =
        if fixedArgs.IsEmpty
        then Ok ImmutableArray.Empty
        else fixedArg fixedArgs (ImmutableArray.CreateBuilder fixedArgs.Length) &chunk
    match fixedArgs' with
    | Ok fixedArgs'' -> namedArgList fixedArgs'' &chunk
    | Error err -> Error err

let customAttrib fixedArgs (chunk: inref<ChunkedMemory>) =
    match chunk.TrySlice 2u with
    | true, chunk' ->
        match ChunkedMemory.readU2 0u &chunk with
        | 1us ->
            let mutable chunk' = chunk'
            fixedArgList fixedArgs &chunk'
        | prolog -> Error(InvalidCustomAttributeProlog(ValueSome prolog))
    | false, _ -> Error(InvalidCustomAttributeProlog ValueNone)

[<RequireQualifiedAccess>]
module FSharpIL.Writing.BlobWriter

open FSharpIL.Utilities

open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures

let [<Literal>] MaxCompressedUnsigned = 0x1FFF_FFFFu
let [<Literal>] MaxCompressedSigned = 268435455
let [<Literal>] MinCompressedSigned = -268435456

let checkCompressedUnsigned value =
    if value > MaxCompressedUnsigned then
        sprintf
            "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x."
            value
            MaxCompressedUnsigned
        |> argOutOfRange "value" value

let compressedUnsignedSize value =
    checkCompressedUnsigned value
    if value > 0x3FFFu then 4u
    elif value > 0x7Fu then 2u
    else 1u

let compressedUnsigned value (stream: byref<ChunkedMemoryBuilder>) =
    checkCompressedUnsigned value
    if value > 0x3FFFu then
        // Sets bit 31 and bit 30, bit 29 remains clear
        stream.WriteLE(value ||| 0xC000_0000u)
    elif value > 0x7Fu then
        // Sets bit 15, bit 14 remains clear
        stream.WriteLE(uint16(value ||| 0x8000u))
    else stream.Write(uint8 value) // Bit 7 remains clear

let compressedSigned value (stream: byref<ChunkedMemoryBuilder>) =
    if value >= -64 && value < 64 then
        // Bit 0 contains sign bit, bit 7 remains clear
        let mutable result = abs value |> uint8
        result <- (result &&& 0b0011_1111uy) <<< 1
        if value < 0 then result <- result ||| 1uy
        stream.Write result
    elif value >= -8192 && value < 8192 then // between -2^13 and 2^13 - 1 inclusive
        // Sets bit 15, bit 14 remains clear, and sign bit is stored in bit 0
        let mutable result = 32768us ||| (abs value |> uint16)
        result <- (result &&& 0b0001_1111_1111_1111us) <<< 1
        if value < 0 then result <- result ||| 1us
        noImpl "Write result Big endian"
    elif value >= MinCompressedSigned && value <= MaxCompressedSigned then // between -2^28 and 2^28 - 1 inclusive
        noImpl "TODO: Implement writing of big compressed integers"
    else argOutOfRange "value" value "Cannot compress signed integer"

let inline elem (elementType: ElementType) (stream: byref<ChunkedMemoryBuilder>) = stream.Write(uint8 elementType)

/// <summary>Writes a compressed index into the <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c> table (II.23.2.8)</summary>
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the index cannot fit into 3 bytes.
/// </exception>
let typeDefOrRefOrSpec (item: TypeDefOrRefOrSpecEncoded) (stream: byref<ChunkedMemoryBuilder>) =
    if item.Index > 0xFFFFFFu then argOutOfRange "item" item "The type index must be able to fit into 3 bytes"
    compressedUnsigned (uint32 item.Tag ||| (item.Index <<< 2)) &stream

let inline typeDefOrRef item (wr: byref<_>) = typeDefOrRefOrSpec (TypeDefOrRefEncoded.toCodedIndex item) &wr

let customMod (modifier: CustomMod) (stream: byref<ChunkedMemoryBuilder>) =
    elem (if modifier.Required then ElementType.CModReqd else ElementType.CModOpt) &stream
    typeDefOrRefOrSpec modifier.ModifierType &stream

let rec customModList modifiers (stream: byref<ChunkedMemoryBuilder>) =
    match modifiers with
    | [] -> ()
    | current :: remaining ->
        customMod current &stream
        customModList remaining &stream

let arrayShape (shape: inref<ArrayShape>) (stream: byref<ChunkedMemoryBuilder>) =
    compressedUnsigned shape.Rank &stream
    compressedUnsigned (uint32 shape.Sizes.Length) &stream // NumSizes
    for size in shape.Sizes do compressedUnsigned size &stream
    compressedUnsigned (uint32 shape.LowerBounds.Length) &stream // NumLoBounds
    for bound in shape.LowerBounds do compressedSigned bound &stream // LoBound

let rec etype (t: EncodedType) (stream: byref<ChunkedMemoryBuilder>) =
    match t with
    | EncodedType.Boolean -> elem ElementType.Boolean &stream
    | EncodedType.Char -> elem ElementType.Char &stream
    | EncodedType.I1 -> elem ElementType.I1 &stream
    | EncodedType.U1 -> elem ElementType.U1 &stream
    | EncodedType.I2 -> elem ElementType.I2 &stream
    | EncodedType.U2 -> elem ElementType.U2 &stream
    | EncodedType.I4 -> elem ElementType.I4 &stream
    | EncodedType.U4 -> elem ElementType.U4 &stream
    | EncodedType.I8 -> elem ElementType.I8 &stream
    | EncodedType.U8 -> elem ElementType.U8 &stream
    | EncodedType.R4 -> elem ElementType.R4 &stream
    | EncodedType.R8 -> elem ElementType.R8 &stream
    | EncodedType.I -> elem ElementType.I &stream
    | EncodedType.U -> elem ElementType.U &stream
    | EncodedType.Array(item, shape) ->
        elem ElementType.Array &stream
        etype item &stream
        arrayShape &shape &stream
    | EncodedType.Class tref
    | EncodedType.ValueType tref ->
        match t with
        | EncodedType.Class _ -> elem ElementType.Class &stream
        | EncodedType.ValueType _
        | _ -> elem ElementType.ValueType &stream
        typeDefOrRef tref &stream
    //| EncodedType.FnPtr ptr ->
    | EncodedType.GenericInst inst -> genericInst inst &stream
    | EncodedType.MVar num
    | EncodedType.Var num ->
        match t with
        | EncodedType.Var _ -> elem ElementType.Var &stream
        | EncodedType.MVar _
        | _ -> elem ElementType.MVar &stream
        compressedUnsigned num &stream // number
    | EncodedType.Object -> elem ElementType.Object &stream
    | EncodedType.Ptr ptr ->
        elem ElementType.Ptr &stream
        match ptr with
        | Pointer.Void modifiers ->
            customModList modifiers &stream
            elem ElementType.Void &stream
        | Pointer.Type item -> etype item &stream
    | EncodedType.String -> elem ElementType.String &stream
    | EncodedType.SZArray item ->
        elem ElementType.SZArray &stream
        etype item &stream
    | EncodedType.Modified(modifiers, t) ->
        customModList modifiers &stream
        etype t &stream

and genericInst (inst: GenericInst) (stream: byref<ChunkedMemoryBuilder>) =
    elem ElementType.GenericInst &stream
    elem (if inst.IsValueType then ElementType.ValueType else ElementType.Class) &stream
    typeDefOrRef inst.GenericType &stream
    compressedUnsigned inst.GenericArguments.Count &stream // GenArgCount
    for arg in inst.GenericArguments do etype arg &stream

and retType (item: inref<RetTypeItem>) (stream: byref<ChunkedMemoryBuilder>) =
    // Writes the custom modifiers before the VOID, BYREF, or TYPEDBYREF bytes.
    if item.Tag <> RetTypeTag.Type then customModList item.Modifiers &stream

    match item.Tag with
    | RetTypeTag.Void -> elem ElementType.Void &stream
    | RetTypeTag.ByRef -> elem ElementType.ByRef &stream
    | RetTypeTag.TypedByRef -> elem ElementType.TypedByRef &stream
    | RetTypeTag.Type
    | _ -> ()

    match item.ReturnType with
    | ValueSome rtype -> etype rtype &stream
    | ValueNone -> ()

and param (item: inref<ParamItem>) (stream: byref<ChunkedMemoryBuilder>) =
    // Writes the custom modifiers before the BYREF or TYPEDBYREF bytes.
    if item.Tag <> ParamItemTag.Param then customModList item.Modifiers &stream

    match item.Tag with
    | ParamItemTag.ByRef -> elem ElementType.ByRef &stream
    | ParamItemTag.TypedByRef -> elem ElementType.TypedByRef &stream
    | ParamItemTag.Param
    | _ -> ()

    match item.ParamType with
    | ValueSome ptype -> etype ptype &stream
    | ValueNone -> ()

and inline parameters (parameters: ImmutableArray<ParamItem>) (stream: byref<_>) =
    for i = 0 to parameters.Length - 1 do param (&parameters.ItemRef i) &stream

and methodSigCommon (signature: inref<MethodDefSig>) paramCountExtra (stream: byref<ChunkedMemoryBuilder>) =
    stream.Write(uint8(signature.HasThis.Tag ||| signature.CallingConvention.Tag))

    match signature.CallingConvention with
    | Default
    | VarArg -> ()
    | Generic genParamCount -> compressedUnsigned genParamCount &stream
    
    compressedUnsigned (uint32 signature.Parameters.Length + paramCountExtra) &stream // ParamCount
    retType &signature.ReturnType &stream
    parameters signature.Parameters &stream // Param

and methodDefSig (signature: inref<MethodDefSig>) (stream: byref<ChunkedMemoryBuilder>) = methodSigCommon &signature 0u &stream

and methodRefSig (signature: inref<MethodRefSig>) (stream: byref<ChunkedMemoryBuilder>) =
    methodSigCommon &signature.Signature (uint32 signature.VarArgParams.Length) &stream

    if not signature.VarArgParams.IsDefaultOrEmpty then
        elem ElementType.Sentinel &stream
        parameters signature.VarArgParams &stream

let fieldSig (signature: inref<FieldSig>) (stream: byref<ChunkedMemoryBuilder>) =
    stream.Write 0x6uy // FIELD
    etype signature.FieldType &stream

let propertySig (signature: inref<PropertySig>) (stream: byref<ChunkedMemoryBuilder>) =
    stream.Write(if signature.HasThis then 0x28uy else 0x8uy) // PROPERTY
    compressedUnsigned (uint32 signature.Parameters.Length) &stream // ParamCount
    retType &signature.PropertyType &stream
    parameters signature.Parameters &stream

let serString str (stream: byref<ChunkedMemoryBuilder>) =
    match str with
    | null -> stream.Write System.Byte.MaxValue
    | "" -> stream.Write 0uy
    | _ ->
        let mutable buffer = if str.Length > 256 then Span.heapalloc<byte> str.Length else Span.stackalloc<byte> str.Length
        let count = System.Text.Encoding.UTF8.GetBytes(System.String.op_Implicit str, buffer)
        compressedUnsigned (uint32 count) &stream // PackedLen
        stream.Write buffer

let customAttribElem e (stream: byref<ChunkedMemoryBuilder>) =
    match e with
    | ValBool false -> stream.Write 0uy
    | ValBool true -> stream.Write 1uy
    | ValI1 (Convert.U1 i)
    | ValU1 i -> stream.Write i
    | ValChar (Convert.U2 i)
    | ValI2 (Convert.U2 i)
    | ValU2 i -> stream.WriteLE i
    | ValI4 (Convert.U4 i)
    | ValU4 i -> stream.WriteLE i
    | ValI8 (Convert.U8 i)
    | ValU8 i -> stream.WriteLE i
    | ValR4 r -> stream.WriteLE(Convert.unsafeTo<_, uint32> r)
    | ValR8 r -> stream.WriteLE(Convert.unsafeTo<_, uint64> r)
    | SerString str -> serString str &stream

let fixedArg arg (stream: byref<ChunkedMemoryBuilder>) =
    match arg with
    | FixedArg.Elem e -> customAttribElem e &stream
    | FixedArg.SZArray ValueNone -> stream.WriteLE System.UInt32.MaxValue
    | FixedArg.SZArray(ValueSome arr) ->
        stream.WriteLE(uint32 arr.Length)
        for e in arr do customAttribElem e &stream

let primitiveElemType prim =
    match prim with
    | PrimitiveElemType.Bool -> ElementType.Boolean
    | PrimitiveElemType.Char -> ElementType.Char
    | PrimitiveElemType.R4 -> ElementType.R4
    | PrimitiveElemType.R8 -> ElementType.R8
    | PrimitiveElemType.I1 -> ElementType.I1
    | PrimitiveElemType.I2 -> ElementType.I2
    | PrimitiveElemType.I4 -> ElementType.I4
    | PrimitiveElemType.I8 -> ElementType.I8
    | PrimitiveElemType.U1 -> ElementType.U1
    | PrimitiveElemType.U2 -> ElementType.U2
    | PrimitiveElemType.U4 -> ElementType.U4
    | PrimitiveElemType.U8 -> ElementType.U8
    | PrimitiveElemType.String -> ElementType.String
    | PrimitiveElemType.Type -> ElementType.Type

let fieldOrPropType etype (stream: byref<ChunkedMemoryBuilder>) =
    match etype with
    | ElemType.Primitive prim -> elem (primitiveElemType prim) &stream
    | ElemType.SZArray itype ->
        elem ElementType.SZArray &stream
        elem (primitiveElemType itype) &stream

let namedArg (arg: inref<_>) (stream: byref<ChunkedMemoryBuilder>) =
    elem (if arg.IsProperty then ElementType.Property else ElementType.Field) &stream
    fieldOrPropType arg.Type &stream
    serString arg.Name &stream // FieldOrPropName
    fixedArg arg.Value &stream

let customAttrib (attrib: inref<_>) (stream: byref<ChunkedMemoryBuilder>) =
    stream.WriteLE 1us // Prolog
    for farg in attrib.FixedArgs do fixedArg farg &stream
    stream.WriteLE(Checked.uint16 attrib.NamedArgs.Length) // NumNamed
    for i = 0 to attrib.NamedArgs.Length - 1 do
        namedArg (&attrib.NamedArgs.ItemRef i) &stream

let rec localConstraintList (constraints: Constraint list) (stream: byref<ChunkedMemoryBuilder>) =
    match constraints with
    | [] -> ()
    | Constraint.Pinned :: constraints' ->
        elem ElementType.Pinned &stream
        localConstraintList constraints' &stream

let localVarSig (signature: LocalVarSig) (stream: byref<ChunkedMemoryBuilder>) =
    if signature.IsDefaultOrEmpty then
        invalidArg (nameof signature) "At least 1 local variable must be defined"
    if signature.Length > int32 LocalVariable.maxLocalCount then
        invalidArg (nameof signature) (sprintf "The number of local variables cannot exceed %i" LocalVariable.maxLocalCount)

    stream.Write 0x7uy // LOCAL_SIG
    compressedUnsigned (uint32 signature.Length) &stream // Count

    for i = 0 to signature.Length - 1 do
        let local = &signature.ItemRef i

        if local.Tag <> LocalVariableTag.TypedByRef then
            customModList local.CustomMod &stream
            localConstraintList local.Constraints &stream
            if local.Tag = LocalVariableTag.ByRef then elem ElementType.ByRef &stream
            etype local.Type.Value &stream
        else elem ElementType.TypedByRef &stream

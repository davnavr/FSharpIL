[<RequireQualifiedAccess>]
module FSharpIL.Writing.BlobWriter

open FSharpIL.Utilities

open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

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

let compressedUnsigned value (wr: byref<ChunkedMemoryBuilder>) =
    checkCompressedUnsigned value
    if value > 0x3FFFu then
        // Sets bit 31 and bit 30, bit 29 remains clear
        wr.WriteLE(value ||| 0xC000_0000u)
    elif value > 0x7Fu then
        // Sets bit 15, bit 14 remains clear
        wr.WriteLE(uint16(value ||| 0x8000u))
    else wr.Write(uint8 value) // Bit 7 remains clear

let compressedSigned value (wr: byref<ChunkedMemoryBuilder>) =
    if value >= -64 && value < 64 then
        // Bit 0 contains sign bit, bit 7 remains clear
        let mutable result = abs value |> uint8
        result <- (result &&& 0b0011_1111uy) <<< 1
        if value < 0 then result <- result ||| 1uy
        wr.Write result
    elif value >= -8192 && value < 8192 then // between -2^13 and 2^13 - 1 inclusive
        // Sets bit 15, bit 14 remains clear, and sign bit is stored in bit 0
        let mutable result = 32768us ||| (abs value |> uint16)
        result <- (result &&& 0b0001_1111_1111_1111us) <<< 1
        if value < 0 then result <- result ||| 1us
        noImpl "Write result Big endian"
    elif value >= MinCompressedSigned && value <= MaxCompressedSigned then // between -2^28 and 2^28 - 1 inclusive
        noImpl "TODO: Implement writing of big compressed integers"
    else argOutOfRange "value" value "Cannot compress signed integer"

let inline elem (elementType: ElementType) (wr: byref<ChunkedMemoryBuilder>) = wr.Write(uint8 elementType)

/// <summary>Writes a compressed index into the <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c> table (II.23.2.8)</summary>
/// <exception cref="T:System.ArgumentOutOfRangeException">
/// Thrown when the index cannot fit into 3 bytes.
/// </exception>
let typeDefOrRefOrSpec (item: TypeDefOrRefOrSpecEncoded) (wr: byref<ChunkedMemoryBuilder>) =
    if item.Index > 0xFFFFFFu then argOutOfRange "item" item "The type index must be able to fit into 3 bytes"
    compressedUnsigned (uint32 item.Tag ||| (item.Index <<< 2)) &wr

let customMod (modifier: CustomMod) (wr: byref<ChunkedMemoryBuilder>) =
    elem (if modifier.Required then ElementType.CModReqd else ElementType.CModOpt) &wr
    typeDefOrRefOrSpec modifier.ModifierType &wr

let customModifierList (modifiers: CustomModifiers) (wr: byref<ChunkedMemoryBuilder>) =
    for modifier in modifiers do customMod modifier &wr

let arrayShape (shape: inref<ArrayShape>) (wr: byref<ChunkedMemoryBuilder>) =
    compressedUnsigned shape.Rank &wr
    compressedUnsigned (uint32 shape.Sizes.Length) &wr // NumSizes
    for size in shape.Sizes do compressedUnsigned size &wr
    compressedUnsigned (uint32 shape.LowerBounds.Length) &wr // NumLoBounds
    for bound in shape.LowerBounds do compressedSigned bound &wr // LoBound

let rec etype (t: EncodedType) (wr: byref<ChunkedMemoryBuilder>) =
    match t with
    | EncodedType.Boolean -> elem ElementType.Boolean &wr
    | EncodedType.Char -> elem ElementType.Char &wr
    | EncodedType.I1 -> elem ElementType.I1 &wr
    | EncodedType.U1 -> elem ElementType.U1 &wr
    | EncodedType.I2 -> elem ElementType.I2 &wr
    | EncodedType.U2 -> elem ElementType.U2 &wr
    | EncodedType.I4 -> elem ElementType.I4 &wr
    | EncodedType.U4 -> elem ElementType.U4 &wr
    | EncodedType.I8 -> elem ElementType.I8 &wr
    | EncodedType.U8 -> elem ElementType.U8 &wr
    | EncodedType.R4 -> elem ElementType.R4 &wr
    | EncodedType.R8 -> elem ElementType.R8 &wr
    | EncodedType.I -> elem ElementType.I &wr
    | EncodedType.U -> elem ElementType.U &wr
    | EncodedType.Array(item, shape) ->
        elem ElementType.Array &wr
        etype item &wr
        arrayShape &shape &wr
    | EncodedType.Class tref
    | EncodedType.ValueType tref ->
        match t with
        | EncodedType.Class _ -> elem ElementType.Class &wr
        | EncodedType.ValueType _
        | _ -> elem ElementType.ValueType &wr
        typeDefOrRefOrSpec tref &wr
    //| EncodedType.FnPtr ptr ->
    | EncodedType.GenericInst inst -> genericInst inst &wr
    | EncodedType.MVar num
    | EncodedType.Var num ->
        match t with
        | EncodedType.Var _ -> elem ElementType.Var &wr
        | EncodedType.MVar _
        | _ -> elem ElementType.MVar &wr
        compressedUnsigned num &wr // number
    | EncodedType.Object -> elem ElementType.Object &wr
    | EncodedType.Ptr ptr ->
        elem ElementType.Ptr &wr
        customModifierList ptr.Modifiers &wr
        match ptr with
        | Pointer.Void _ -> elem ElementType.Void &wr
        | Pointer.Type(_, item) -> etype item &wr
    | EncodedType.String -> elem ElementType.String &wr
    | EncodedType.SZArray(modifiers, item) ->
        elem ElementType.SZArray &wr
        customModifierList modifiers &wr
        etype item &wr

and genericInst (inst: GenericInst) (wr: byref<ChunkedMemoryBuilder>) =
    elem ElementType.GenericInst &wr
    elem (if inst.IsValueType then ElementType.ValueType else ElementType.Class) &wr
    typeDefOrRefOrSpec inst.GenericType &wr
    compressedUnsigned inst.GenericArguments.Count &wr // GenArgCount
    for arg in inst.GenericArguments do etype arg &wr

and retType (item: inref<ReturnType>) (wr: byref<ChunkedMemoryBuilder>) =
    customModifierList item.CustomModifiers &wr
    if item.IsVoid then elem ElementType.Void &wr
    elif item.IsTypedByRef then elem ElementType.TypedByRef &wr
    else
        if item.IsByRef then elem ElementType.ByRef &wr
        etype item.ReturnType.Value &wr

and param (item: inref<ParamItem>) (wr: byref<ChunkedMemoryBuilder>) =
    customModifierList item.CustomModifiers &wr
    if item.IsTypedByRef
    then elem ElementType.TypedByRef &wr
    else
        if item.IsByRef then elem ElementType.ByRef &wr
        etype item.ParamType.Value &wr

and inline parameters (parameters: ImmutableArray<ParamItem>) (wr: byref<_>) =
    for i = 0 to parameters.Length do param (&parameters.ItemRef i) &wr

// and methodRefSig

let methodDefSig (signature: inref<MethodDefSig>) (wr: byref<ChunkedMemoryBuilder>) =
    wr.Write(uint8(signature.HasThis.Tag ||| signature.CallingConvention.Tag))

    match signature.CallingConvention with
    | Default
    | VarArg -> ()
    | Generic genParamCount -> compressedUnsigned genParamCount &wr

    compressedUnsigned (uint32 signature.Parameters.Length) &wr // ParamCount
    retType &signature.ReturnType &wr
    parameters signature.Parameters &wr // Param

let fieldSig (signature: inref<FieldSig>) (wr: byref<ChunkedMemoryBuilder>) =
    wr.Write 0x6uy // FIELD
    customModifierList signature.CustomModifiers &wr
    etype signature.FieldType &wr

let propertySig (signature: inref<PropertySig>) (wr: byref<ChunkedMemoryBuilder>) =
    wr.Write(if signature.HasThis then 0x28uy else 0x8uy) // PROPERTY
    compressedUnsigned (uint32 signature.Parameters.Length) &wr // ParamCount
    customModifierList signature.CustomModifiers &wr
    etype signature.PropertyType &wr
    parameters signature.Parameters &wr

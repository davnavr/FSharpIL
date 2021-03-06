/// <summary>
/// Contains functions for calculating the sizes of various "blobs" and signatures (II.23.2) in the <c>#Blob</c> heap (II.24.2.4).
/// </summary>
[<RequireQualifiedAccess>]
module internal rec FSharpIL.Metadata.Heaps.BlobSize

open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata

[<Literal>]
let MaxCompressedUnsigned = 0x1FFF_FFFFu

/// <summary>Calculates how many bytes are needed to store the specified value in an unsigned compressed integer (II.23.2). </summary>
let ofUnsigned (value: uint32) =
    if value > MaxCompressedUnsigned then
        sprintf
            "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x."
            value
            MaxCompressedUnsigned
        |> invalidArg (nameof value)
    elif value > 0x3FFFu then 4u
    elif value > 0x7Fu then 2u
    else 1u

let ofSigned (value) = invalidOp "TODO: Calculate size of signed compressed integers."

let (|B4|B2|B1|) (value: uint32) =
    match ofUnsigned value with
    | 4u -> B4
    | 2u -> B2
    | 1u -> B1
    | _ -> invalidArg (nameof value) "Invalid compressed size."

let ofCustomMod (customMod: CustomModifier) =
    failwith "TODO: Calculate size of custom modifiers"

// TODO: Consider using a for loop to avoid object allocations since the ImmutableArray.Enumerator is a struct.
let private customModifiers modifiers = Seq.sumBy ofCustomMod modifiers

let ofGenericInst =
    function
    | GenericInst.TypeRef(_, _, head, tail) ->
        2u
        + (failwith "TODO: How to calculate size of TypeDefOrRefOrSpecEncoded?")
        + ofType head
        + Seq.sumBy ofType tail

let rec ofType =
    function
    | EncodedType.Boolean
    | EncodedType.Char
    | EncodedType.I1
    | EncodedType.U1
    | EncodedType.I2
    | EncodedType.U2
    | EncodedType.I4
    | EncodedType.U4
    | EncodedType.I8
    | EncodedType.U8
    | EncodedType.R4
    | EncodedType.R8
    | EncodedType.I
    | EncodedType.U
    | EncodedType.Object
    | EncodedType.String -> 1u
    | EncodedType.Array(item, shape) ->
        1u
        + ofType item
        + ofUnsigned shape.Rank
        + ofUnsigned (uint32 shape.Sizes.Length)
        + Seq.sumBy ofUnsigned shape.Sizes
        + ofUnsigned (uint32 shape.LowerBounds.Length)
        + Seq.sumBy ofSigned shape.LowerBounds
    | EncodedType.GenericInst inst -> ofGenericInst inst
    | EncodedType.SZArray(modifiers, item) ->
        if not modifiers.IsEmpty then
            failwith "Cannot calculate size of blob, custom modifiers for SZArray not yet supported"
        1u + ofType item
    | t -> failwithf "Cannot calculate size for unsupported type %A" t

let ofRetType (retType: ReturnTypeItem) =
    let size =
        match retType.ReturnType with
        | ReturnType.Void -> 1u
        | ReturnType.Type item -> ofType item
        | bad -> failwithf "Unable to calculate size for unsupported return type %A" bad
    size + customModifiers retType.CustomMod

let ofFieldSignature (signature: FieldSignature) =
    1u
    + customModifiers signature.CustomMod
    + ofType signature.FieldType

let ofParam (param: ParamItem) =
    ofType param.ParamType + customModifiers param.CustomMod

let private parameters paramList = Seq.sumBy ofParam paramList

let ofMethodDefSignature (signature: MethodDefSignature) =
    let cconv =
        match signature.CallingConventions with
        | Default
        | VarArg -> 0u
        // | Generic -> failwithf "TODO: Calculate size needed to store generic for method def"
    1u
    + cconv
    + ofUnsigned (uint32 signature.Parameters.Length)
    + ofRetType signature.ReturnType
    + parameters signature.Parameters

let ofMethodRefSignature (signature: MethodRefSignature) =
    let varargs =
        if signature.VarArgParameters.IsEmpty
        then 0u
        else invalidOp "TODO: Calculate size of varargs"
    1u
    + ofUnsigned signature.ParamCount
    + ofRetType signature.ReturnType
    + parameters signature.Parameters
    + varargs

let ofElem =
    function
    | Elem.ValBool _
    | Elem.ValI1 _
    | Elem.ValU1 _ -> 1u
    | Elem.ValChar _
    | Elem.ValI2 _
    | Elem.ValU2 _ -> 2u
    | Elem.ValI4 _
    | Elem.ValU4 _
    | Elem.ValR4 _ -> 4u
    | Elem.ValI8 _
    | Elem.ValU8 _
    | Elem.ValR8 _ -> 8u
    | Elem.SerString str ->
        let size = Encoding.UTF8.GetByteCount str |> uint32
        (ofUnsigned size) + size


let ofCustomAttribute (attribute: CustomAttributeSignature) =
    2u
    + Seq.sumBy
        (function
        | FixedArg.Elem elem -> ofElem elem
        | FixedArg.SZArray _ -> invalidOp "TODO: Add support for SZArray in custom attribute values")
        attribute.FixedArg
    + 2u
    + Seq.sumBy
        (fun arg ->
            invalidOp "TODO: Implement writing of custom attribute named arguments")
        attribute.NamedArg

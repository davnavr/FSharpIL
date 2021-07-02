[<RequireQualifiedAccess>]
module internal rec FSharpIL.Metadata.Signatures.Blob

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Collections

let mapCustomMod (mapping: 'T -> 'U) (modifiers: ImmutableArray<_>) =
    let mutable modifiers' = Array.zeroCreate modifiers.Length
    for i = 0 to modifiers'.Length - 1 do
        let modf = &modifiers.ItemRef i
        modifiers'.[i] <- { Required = modf.Required; ModifierType = mapping modf.ModifierType }
    Unsafe.As<_, ImmutableArray<CustomMod<'U>>> &modifiers'

let rec mapType namedTypeMapping modifierTypeMapping etype =
    let inline inner elem = mapType namedTypeMapping modifierTypeMapping elem
    match etype with
    | EncodedType.Boolean -> EncodedType.Boolean
    | EncodedType.Char -> EncodedType.Char
    | EncodedType.I1 -> EncodedType.I1
    | EncodedType.U1 -> EncodedType.U1
    | EncodedType.I2 -> EncodedType.I2
    | EncodedType.U2 -> EncodedType.U2
    | EncodedType.I4 -> EncodedType.I4
    | EncodedType.U4 -> EncodedType.U4
    | EncodedType.I8 -> EncodedType.I8
    | EncodedType.U8 -> EncodedType.U8
    | EncodedType.R4 -> EncodedType.R4
    | EncodedType.R8 -> EncodedType.R8
    | EncodedType.I -> EncodedType.I
    | EncodedType.U -> EncodedType.U
    | EncodedType.Array(elem, shape) -> EncodedType.Array(inner elem, shape)
    | EncodedType.Class t -> EncodedType.Class(namedTypeMapping t)
    //| Encodedtype.FnPtr
    | EncodedType.GenericInst inst ->
        let mutable gargs = Array.zeroCreate inst.GenericArguments.GenArgs.Length
        for i = 0 to gargs.Length - 1 do gargs.[i] <- inner inst.GenericArguments.[i]
        EncodedType.GenericInst
            { IsValueType = inst.IsValueType
              GenericType = namedTypeMapping inst.GenericType
              GenericArguments = Unsafe.As &gargs }
    | EncodedType.MVar index -> EncodedType.MVar index
    | EncodedType.Object -> EncodedType.Object
    | EncodedType.Ptr ptr ->
        EncodedType.Ptr
            { Modifiers = mapCustomMod modifierTypeMapping ptr.Modifiers
              PointerType = ValueOption.map inner ptr.PointerType }
    | EncodedType.String -> EncodedType.String
    | EncodedType.SZArray(modifiers, elem) -> EncodedType.SZArray(mapCustomMod modifierTypeMapping modifiers, inner elem)
    | EncodedType.ValueType t -> EncodedType.ValueType(namedTypeMapping t)
    | EncodedType.Var index -> EncodedType.Var index

let mapOptionalType namedTypeMapping modifierTypeMapping value =
    match value with
    | ValueSome etype -> ValueSome(mapType namedTypeMapping modifierTypeMapping etype)
    | ValueNone -> ValueNone

let mapFieldSig namedTypeMapping modifierTypeMapping (signature: inref<FieldSig<_, _>>) =
    { CustomModifiers = mapCustomMod modifierTypeMapping signature.CustomModifiers
      FieldType = mapType namedTypeMapping modifierTypeMapping signature.FieldType }

let mapReturnType namedTypeMapping modifierTypeMapping (rtype: inref<ReturnType<_, _>>) =
    ReturnType<_, _> (
        rtype.Tag,
        mapCustomMod modifierTypeMapping rtype.CustomModifiers,
        mapOptionalType namedTypeMapping modifierTypeMapping rtype.ReturnType
    )

let mapParameters namedTypeMapping modifierTypeMapping (parameters: ImmutableArray<ParamItem<'Type1, 'MType1>>) =
    let mutable parameters' = Array.zeroCreate<ParamItem<'Type2, 'MType2>> parameters.Length
    for i = 0 to parameters'.Length - 1 do
        let param = &parameters.ItemRef i
        parameters'.[i] <-
            ParamItem<_, _> (
                param.Tag,
                mapCustomMod modifierTypeMapping param.CustomModifiers,
                mapOptionalType namedTypeMapping modifierTypeMapping param.ParamType
            )
    Unsafe.As<_, ImmutableArray<ParamItem<'Type2, 'MType2>>> &parameters'

let mapMethodDefSig namedTypeMapping modifierTypeMapping (signature: inref<MethodDefSig<_, _>>) =
    { HasThis = signature.HasThis
      CallingConvention = signature.CallingConvention
      ReturnType = mapReturnType namedTypeMapping modifierTypeMapping &signature.ReturnType
      Parameters = mapParameters namedTypeMapping modifierTypeMapping signature.Parameters }

let mapMethodRefSig namedTypeMapping modifierTypeMapping (signature: inref<MethodRefSig<_, _>>) =
    MethodRefSig (
        mapMethodDefSig namedTypeMapping modifierTypeMapping &signature.Signature,
        mapParameters namedTypeMapping modifierTypeMapping signature.VarArgParams
    )

let mapLocalVarSig namedTypeMapping modifierTypeMapping (signature: LocalVarSig<'Type1, 'MType1>) =
    if signature.IsDefaultOrEmpty
    then ImmutableArray.Empty
    else
        let mutable signature' = Array.zeroCreate<LocalVariable<'Type2, 'MType2>> signature.Length
        for i = 0 to signature'.Length - 1 do
            let local = &signature.ItemRef i
            signature'.[i] <-
                LocalVariable (
                    mapCustomMod modifierTypeMapping local.CustomMod,
                    local.Constraints,
                    local.Tag,
                    mapOptionalType namedTypeMapping modifierTypeMapping local.Type
                )
        Unsafe.As<_, ImmutableArray<ParamItem<'Type2, 'MType2>>> &signature'

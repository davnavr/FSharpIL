namespace rec FSharpIL.Metadata.Signatures

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities.Compare

[<IsReadOnly; Struct>]
type TypeDefOrRefEncoded =
    val IsDefinition: bool
    val Index: uint32

    new (isTypeDef, index) = { IsDefinition = isTypeDef; Index = index }

    member this.IsReference = not this.IsDefinition

[<RequireQualifiedAccess>]
module TypeDefOrRefEncoded =
    let toCodedIndex (index: TypeDefOrRefEncoded) =
        if index.IsDefinition
        then TypeDefOrRef.Def { TableIndex = index.Index }
        else TypeDefOrRef.Ref { TableIndex = index.Index }

    let Def (index: TableIndex<TypeDefRow>) = TypeDefOrRefEncoded(true, index.TableIndex)
    let Ref (index: TableIndex<TypeRefRow>) = TypeDefOrRefEncoded(false, index.TableIndex)

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type EncodedType =
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
    | Array of EncodedType * ArrayShape
    | Class of TypeDefOrRefEncoded
    | GenericInst of GenericInst
    | MVar of index: uint32
    | Object
    | Ptr of Pointer
    | String
    | SZArray of EncodedType
    | ValueType of TypeDefOrRefEncoded
    | Var of index: uint32
    | Modified of modifiers: CustomMod list * EncodedType

[<IsReadOnly; Struct>]
type FieldSig = { FieldType: EncodedType }

type ParamItemTag =
    | Param = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy

[<IsReadOnly>]
type ParamItem = struct
    val Tag: ParamItemTag
    val Modifiers: CustomMod list
    val ParamType: EncodedType voption

    new (tag, modifiers, paramType) = { Tag = tag; Modifiers = modifiers; ParamType = paramType }

    member inline this.IsTypedByRef = this.Tag = ParamItemTag.TypedByRef
    member inline this.IsByRef = this.Tag = ParamItemTag.ByRef

    member this.CustomModifiers =
        match this.Modifiers, this.ParamType with
        | [], ValueSome(EncodedType.Modified(modifiers', _)) -> modifiers'
        | _ -> this.Modifiers
end

[<RequireQualifiedAccess>]
module ParamItem =
    let inline (|Type|ByRef|TypedByRef|) (param: ParamItem) =
        match param.Tag with
        | ParamItemTag.ByRef -> ByRef(struct(param.CustomModifiers, param.ParamType.Value))
        | ParamItemTag.TypedByRef -> TypedByRef param.CustomModifiers
        | ParamItemTag.Param
        | _ -> Type param.ParamType.Value

    let Type paramType = ParamItem(ParamItemTag.Param, List.empty, ValueSome paramType)
    let ByRef(modifiers, toType) = ParamItem(ParamItemTag.ByRef, modifiers, ValueSome toType)
    let ByRef' toType = ByRef(List.empty, toType)
    let TypedByRef modifiers = ParamItem(ParamItemTag.TypedByRef, modifiers, ValueNone)
    let TypedByRef' = TypedByRef List.empty

type RetTypeTag =
    | Type = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy
    | Void = 3uy

[<IsReadOnly; Struct>]
type RetTypeItem =
    val Tag: RetTypeTag
    val Modifiers: CustomMod list
    val ReturnType: EncodedType voption

    new (tag, modifiers, returnType) = { Tag = tag; Modifiers = modifiers; ReturnType = returnType }

    member inline this.IsTypedByRef = this.Tag = RetTypeTag.TypedByRef
    member inline this.IsVoid = this.Tag = RetTypeTag.Void
    member inline this.IsByRef = this.Tag = RetTypeTag.ByRef

    member this.CustomModifiers =
        match this.Modifiers, this.ReturnType with
        | [], ValueSome(EncodedType.Modified(modifiers', _)) -> modifiers'
        | _ -> this.Modifiers

[<RequireQualifiedAccess>]
module RetTypeItem =
    let inline (|Type|ByRef|TypedByRef|Void|) (returnType: RetTypeItem) =
        match returnType.Tag with
        | RetTypeTag.ByRef -> ByRef(struct(returnType.CustomModifiers, returnType.ReturnType.Value))
        | RetTypeTag.TypedByRef -> TypedByRef returnType.CustomModifiers
        | RetTypeTag.Void -> Void returnType.CustomModifiers
        | RetTypeTag.Type
        | _ -> Type returnType.ReturnType.Value

    let Type returnType = RetTypeItem(RetTypeTag.Type, List.empty, ValueSome returnType)
    let ByRef(modifiers, toType) = RetTypeItem(RetTypeTag.ByRef, modifiers, ValueSome toType)
    let ByRef' toType = ByRef(List.empty, toType)
    let TypedByRef modifiers = RetTypeItem(RetTypeTag.TypedByRef, modifiers, ValueNone)
    let Void modifiers = RetTypeItem(RetTypeTag.Void, modifiers, ValueNone)
    let TypedByRef' = TypedByRef List.empty
    let Void' = Void List.empty

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodThis (tag: CallConvFlags) =
    member _.Tag = tag

[<AutoOpen>]
module MethodThis =
    let inline (|NoThis|HasThis|ExplicitThis|) (this: MethodThis) =
        if this.Tag.HasFlag CallConvFlags.ExplicitThis then ExplicitThis
        elif this.Tag.HasFlag CallConvFlags.HasThis then HasThis
        else NoThis

    let NoThis = MethodThis CallConvFlags.Default
    let HasThis = MethodThis CallConvFlags.HasThis
    let ExplicitThis = MethodThis CallConvFlags.ExplicitThis

[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type CallingConventions = struct
    val Tag: CallConvFlags
    val Count: uint32

    new (tag, count) = { Tag = tag; Count = count }
end

(*
[<IsReadOnly; Struct>]
type CallingConventions =
    | Default
    | VarArg
    | Generic of Count: uint32
*)

[<AutoOpen>]
module CallingConventions =
    let inline (|Default|VarArg|Generic|) (cconv: CallingConventions) =
        match cconv.Tag with
        | CallConvFlags.VarArg -> VarArg
        | CallConvFlags.Generic -> Generic(cconv.Count)
        | CallConvFlags.Default
        | _ -> Default

    let Default = CallingConventions(CallConvFlags.Default, 0u)
    let VarArg = CallingConventions(CallConvFlags.VarArg, 0u)
    let Generic count = CallingConventions(CallConvFlags.Generic, count)

[<IsReadOnly; Struct>]
type MethodDefSig =
    { HasThis: MethodThis
      CallingConvention: CallingConventions
      ReturnType: RetTypeItem
      Parameters: ImmutableArray<ParamItem> }

[<IsReadOnly>]
type MethodRefSig = struct
    val Signature: MethodDefSig
    val VarArgParams: ImmutableArray<ParamItem>

    new (signature, varArgParams) = { Signature = signature; VarArgParams = varArgParams }

    member this.HasThis = this.Signature.HasThis
    member this.CallingConvention = this.Signature.CallingConvention
    member this.ReturnType = this.Signature.ReturnType
    member this.Parameters = this.Signature.Parameters
end

[<RequireQualifiedAccess>]
module MethodRefSig =
    let ofMethodDefSig signature = MethodRefSig(signature, ImmutableArray.Empty)

    let inline Default(hasThis, returnType, parameters) =
        ofMethodDefSig
            { HasThis = hasThis
              CallingConvention = CallingConventions.Default
              ReturnType = returnType
              Parameters = parameters }

    let inline Generic(hasThis, returnType, genParamCount, parameters) =
        ofMethodDefSig
            { HasThis = hasThis
              CallingConvention = CallingConventions.Generic genParamCount
              ReturnType = returnType
              Parameters = parameters }

    let VarArg(hasThis, returnType, parameters, varArgParams) =
        MethodRefSig (
            { HasThis = hasThis
              CallingConvention = CallingConventions.VarArg
              ReturnType = returnType
              Parameters = parameters },
            varArgParams
        )

[<IsReadOnly; Struct>]
type PropertySig =
    { HasThis: bool
      PropertyType: EncodedType
      Parameters: ImmutableArray<ParamItem> }

[<IsReadOnly; Struct>]
type GenericArgList =
    internal { GenArgs: ImmutableArray<EncodedType> }
    member this.Count = uint32 this.GenArgs.Length
    member this.Item with get index = this.GenArgs.[index]
    member this.GetEnumerator() = this.GenArgs.GetEnumerator()

[<IsReadOnly; Struct>]
type GenericInst =
    { IsValueType: bool
      GenericType: TypeDefOrRefEncoded
      GenericArguments: GenericArgList }

[<RequireQualifiedAccess>]
module GenericInst =
    let inline (|Class|ValueType|) inst =
        let inline gtype() = struct(inst.GenericType, inst.GenericArguments)
        if inst.IsValueType then ValueType(gtype()) else Class(gtype())

    let inline Class(instantiated, genericArgs) =
        { IsValueType = false; GenericType = instantiated; GenericArguments = genericArgs }

    let inline ValueType(instantiated, genericArgs) =
        { IsValueType = true; GenericType = instantiated; GenericArguments = genericArgs }

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type ArrayShape =
    { Rank: uint32 // TODO: How to prevent a value of zero?
      Sizes: ImmutableArray<uint32> // NOTE: The two arrays containing information for each dimension can contain less items than Rank.
      LowerBounds: ImmutableArray<int32> }

    override this.ToString() =
        let rank = Checked.int32 this.Rank
        let str = System.Text.StringBuilder(2 + rank).Append('[')

        for i = 0 to rank - 1 do
            let lower =
                if not this.LowerBounds.IsDefaultOrEmpty && i < this.LowerBounds.Length
                then ValueSome this.LowerBounds.[i]
                else ValueNone

            let upper =
                if not this.Sizes.IsDefaultOrEmpty && i < this.Sizes.Length then
                    let upper' = int64 this.Sizes.[i]
                    match lower with
                    | ValueSome lower' -> ValueSome(upper' + int64 lower')
                    | ValueNone -> ValueSome upper'
                else ValueNone

            if i > 0 then str.Append ", " |> ignore

            match lower, upper with
            | ValueNone, ValueNone -> ()
            | ValueSome lower', ValueNone -> Printf.bprintf str "%i..." lower'
            | ValueNone, ValueSome upper' -> str.Append upper' |> ignore
            | ValueSome lower', ValueSome upper' -> Printf.bprintf str "%i...%i" lower' upper'

        str.Append(']').ToString()

    interface System.IEquatable<ArrayShape> with
        member this.Equals other =
            this.Rank = other.Rank &&
            Equatable.blocks this.Sizes other.Sizes &&
            Equatable.blocks this.LowerBounds other.LowerBounds

[<RequireQualifiedAccess>]
module ArrayShape =
    let internal ofVector =
        { Rank = 1u
          Sizes = ImmutableArray.Empty
          LowerBounds = ImmutableArray.Empty }

[<RequireQualifiedAccess>]
[<IsReadOnly; Struct>]
type Pointer =
    | Type of toType: EncodedType
    | Void of modifiers: CustomMod list

[<RequireQualifiedAccess>]
module Pointer =
    let Void' = Pointer.Void List.empty

[<RequireQualifiedAccess>]
module EncodedType =
    let rec isMethodVar (etype: EncodedType) =
        match etype with
        | EncodedType.MVar _ -> true
        | EncodedType.SZArray t
        | EncodedType.Array(t, _)
        | EncodedType.Ptr(Pointer.Type t) -> isMethodVar t
        | _ -> false

    let private toPrimitiveType (etype: EncodedType) =
        match etype with
        | EncodedType.Boolean -> ValueSome PrimitiveElemType.Bool
        | EncodedType.Char -> ValueSome PrimitiveElemType.Char
        | EncodedType.R4 -> ValueSome PrimitiveElemType.R4
        | EncodedType.R8 -> ValueSome PrimitiveElemType.R8
        | EncodedType.I1 -> ValueSome PrimitiveElemType.I1
        | EncodedType.I2 -> ValueSome PrimitiveElemType.I2
        | EncodedType.I4 -> ValueSome PrimitiveElemType.I4
        | EncodedType.I8 -> ValueSome PrimitiveElemType.I8
        | EncodedType.U1 -> ValueSome PrimitiveElemType.U1
        | EncodedType.U2 -> ValueSome PrimitiveElemType.U2
        | EncodedType.U4 -> ValueSome PrimitiveElemType.U4
        | EncodedType.U8 -> ValueSome PrimitiveElemType.U8
        | EncodedType.String -> ValueSome PrimitiveElemType.String
        //| _ -> ValueSome(ElemType.Primitive(PrimitiveElemType.Type))
        | _ -> ValueNone

    let toElemType (etype: EncodedType) =
        match toPrimitiveType etype with
        | ValueSome prim -> ValueSome(ElemType.Primitive prim)
        | ValueNone ->
            match etype with
            | EncodedType.SZArray item -> ValueOption.map ElemType.Primitive (toPrimitiveType item)
            | _ -> ValueNone

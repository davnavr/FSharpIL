﻿namespace rec FSharpIL.Metadata.Signatures

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Tables

/// <summary>Represents a <c>Type</c> (II.23.2.12).</summary>
[<RequireQualifiedAccess>]
type EncodedType =
    /// <summary>The <see cref="T:System.Boolean"/> type.</summary>
    | Boolean
    /// <summary>The <see cref="T:System.Char"/> type.</summary>
    | Char
    /// <summary>The <see cref="T:System.SByte"/> type.</summary>
    | I1
    /// <summary>The <see cref="T:System.Byte"/> type.</summary>
    | U1
    /// <summary>The <see cref="T:System.Int16"/> type.</summary>
    | I2
    /// <summary>The <see cref="T:System.UInt16"/> type.</summary>
    | U2
    /// <summary>The <see cref="T:System.Int32"/> type.</summary>
    | I4
    /// <summary>The <see cref="T:System.UInt32"/> type.</summary>
    | U4
    /// <summary>The <see cref="T:System.Int64"/> type.</summary>
    | I8
    /// <summary>The <see cref="T:System.UInt64"/> type.</summary>
    | U8
    /// <summary>The <see cref="T:System.Single"/> type.</summary>
    | R4
    /// <summary>The <see cref="T:System.Double"/> type.</summary>
    | R8
    /// <summary>The <see cref="T:System.IntPtr"/> type.</summary>
    | I
    /// <summary>The <see cref="T:System.UIntPtr"/> type.</summary>
    | U
    | Array of EncodedType * ArrayShape
    | Class of TypeDefOrRef
    /// A method pointer (II.14.5).
    //| FnPtr of FunctionPointer
    | GenericInst of GenericInst
    /// A generic parameter in a generic method.
    | MVar of index: uint32
    /// <summary>The <see cref="T:System.Object"/> type.</summary>
    | Object
    /// A native pointer (II.14.4.1).
    | Ptr of Pointer
    /// <summary>The <see cref="T:System.String"/> type.</summary>
    | String
    /// A single-dimensional array with a lower bound of zero, also known as a vector (I.8.9.1).
    | SZArray of CustomModifiers * EncodedType
    | ValueType of TypeDefOrRef
    /// A generic parameter in a generic type definition.
    | Var of index: uint32

/// <summary>
/// Represents a <c>FieldSig</c> item, which captures the definition of a field or global variable (II.23.2.4).
/// </summary>
[<IsReadOnly; Struct>]
type FieldSig = { CustomModifiers: CustomModifiers; FieldType: EncodedType }

type ParamItemTag =
    | Param = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy

/// <summary>Represents a <c>Param</c> item used in method signatures (II.23.2.10).</summary>
[<IsReadOnly>]
type ParamItem = struct
    val Tag: ParamItemTag
    val CustomModifiers: CustomModifiers
    val internal ParamType: EncodedType
    internal new (tag, modifiers, paramType) = { Tag = tag; CustomModifiers = modifiers; ParamType = paramType }
    member this.IsTypedByRef = this.Tag = ParamItemTag.TypedByRef
end

(*
[<RequireQualifiedAccess>]
type ParamItem =
    | Param of CustomModifiers: CustomModifiers * ParamType: EncodedType
    | ByRef of CustomModifiers: CustomModifiers * ParamType: EncodedType
    | TypedByRef of CustomModifiers: CustomModifiers
*)

[<RequireQualifiedAccess>]
module ParamItem =
    let (|Param|ByRef|TypedByRef|) (param: ParamItem) =
        let ptype = struct(param.CustomModifiers, param.ParamType)
        match param.Tag with
        | ParamItemTag.ByRef -> ByRef ptype
        | ParamItemTag.TypedByRef -> TypedByRef param.CustomModifiers
        | ParamItemTag.Param
        | _ -> Param ptype

    let Param (modifiers, paramType) = ParamItem(ParamItemTag.Param, modifiers, paramType)
    let ByRef (modifiers, paramType) = ParamItem(ParamItemTag.ByRef, modifiers, paramType)
    let TypedByRef modifiers = ParamItem(ParamItemTag.TypedByRef, modifiers, Unchecked.defaultof<EncodedType>)

type ReturnTypeTag =
    | Type = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy
    | Void = 3uy

/// <summary>Represents a <c>RetType</c> item used in method signatures (II.23.2.11).</summary>
[<IsReadOnly>]
type ReturnType = struct
    val Tag: ReturnTypeTag
    val CustomModifiers: CustomModifiers
    val internal ReturnType: EncodedType
    internal new (tag, modifiers, paramType) = { Tag = tag; CustomModifiers = modifiers; ReturnType = paramType }
    member this.IsTypedByRef = this.Tag = ReturnTypeTag.TypedByRef
    member this.IsVoid = this.Tag = ReturnTypeTag.Void
end

(*
[<RequireQualifiedAccess>]
type ReturnType =
    | Type of CustomModifiers: CustomModifiers * ParamType: EncodedType
    | ByRef of CustomModifiers: CustomModifiers * ParamType: EncodedType
    | TypedByRef of CustomModifiers: CustomModifiers
    | Void of CustomModifiers: CustomModifiers
*)

[<RequireQualifiedAccess>]
module ReturnType =
    let (|Type|ByRef|TypedByRef|Void|) (retType: ReturnType) =
        let rtype = struct(retType.CustomModifiers, retType.ReturnType)
        match retType.Tag with
        | ReturnTypeTag.ByRef -> ByRef rtype
        | ReturnTypeTag.TypedByRef -> TypedByRef retType.CustomModifiers
        | ReturnTypeTag.Void -> Void retType.CustomModifiers
        | ReturnTypeTag.Type
        | _ -> Type rtype

    let Type (modifiers, paramType) = ReturnType(ReturnTypeTag.Type, modifiers, paramType)
    let ByRef (modifiers, paramType) = ReturnType(ReturnTypeTag.ByRef, modifiers, paramType)
    let TypedByRef modifiers = ReturnType(ReturnTypeTag.TypedByRef, modifiers, Unchecked.defaultof<EncodedType>)
    let Void modifiers = ReturnType(ReturnTypeTag.TypedByRef, modifiers, Unchecked.defaultof<EncodedType>)

type [<IsReadOnly; Struct>] MethodThis internal (tag: CallConvFlags) = member _.Tag = tag

(*
type MethodThis =
    | NoThis
    | HasThis
    | ExplicitThis
*)

[<AutoOpen>]
module MethodThisPatterns =
    let inline (|NoThis|HasThis|ExplicitThis|) (this: MethodThis) =
        if this.Tag.HasFlag CallConvFlags.ExplicitThis then ExplicitThis
        elif this.Tag.HasFlag CallConvFlags.HasThis then HasThis
        else NoThis
    let NoThis = MethodThis CallConvFlags.Default
    let HasThis = MethodThis CallConvFlags.HasThis
    let ExplicitThis = MethodThis CallConvFlags.ExplicitThis

[<IsReadOnly>]
type CallingConventions = struct
    val value: uint32
    internal new (value) = { value = value }
end

(*
[<IsReadOnly; Struct>]
type CallingConventions =
    | Default
    | VarArg
    | Generic of count: uint32
*)

[<AutoOpen>]
module CallingConventionsPatterns =
    let [<Literal>] private MaxValue = 0xFFFFFFu
    let inline (|Default|VarArg|Generic|) (cconv: CallingConventions) =
        match LanguagePrimitives.EnumOfValue(uint8(cconv.value >>> 28)) with
        | CallConvFlags.VarArg -> VarArg
        | CallConvFlags.Generic -> Generic(cconv.value &&& MaxValue)
        | CallConvFlags.Default
        | _ -> Default
    let inline private create tag = uint32 tag <<< 28
    let Default = CallingConventions(create CallConvFlags.Default)
    let VarArg = CallingConventions(create CallConvFlags.VarArg)
    let Generic count =
        if count > MaxValue then failwith "TODO: Check that generic argument count does not overlap with tag bits"
        CallingConventions(create CallConvFlags.Generic ||| count)

/// <summary>Represents a <c>MethodDefSig</c>, which captures the signature of a method or global function (II.23.2.1).</summary>
[<IsReadOnly; Struct>]
type MethodDefSig =
    { HasThis: MethodThis
      CallingConvention: CallingConventions
      ReturnType: ReturnType
      Parameters: ImmutableArray<ParamItem> }

[<IsReadOnly; Struct>]
type PropertySig =
    { HasThis: bool
      CustomModifiers: CustomModifiers
      PropertyType: EncodedType
      Parameters: ImmutableArray<ParamItem> }

[<IsReadOnly; Struct>]
type GenericArgList =
    internal { GenArgs: ImmutableArray<EncodedType> }
    member this.Count = this.GenArgs.Length
    member this.Item with get i = this.GenArgs.[i]
    member this.GetEnumerator() = this.GenArgs.GetEnumerator()

/// Represents a generic instantiation (II.23.2.12 and II.23.2.14).
[<IsReadOnly; Struct>]
type GenericInst =
    { IsValueType: bool
      GenericType: TypeDefOrRef
      GenericArguments: GenericArgList }

(*
[<RequireQualifiedAccess>]
type GenericInst =
    | Class of GenericType: TypeDefOrRef * GenericArguments: GenericArgList
    | ValueType of GenericType: TypeDefOrRef * GenericArguments: GenericArgList
*)
[<RequireQualifiedAccess>]
module GenericInst =
    let inline (|Class|ValueType|) inst =
        let inline gtype() = struct(inst.GenericType, inst.GenericArguments)
        if inst.IsValueType then ValueType(gtype()) else Class(gtype())
    let Class(gtype, gargs) = { IsValueType = false; GenericType = gtype; GenericArguments = gargs }
    let ValueType(gtype, gargs) = { IsValueType = true; GenericType = gtype; GenericArguments = gargs }

/// Describes the lower bounds, lengths, and number of dimensions of an array (II.23.2.13).
[<IsReadOnly; Struct>]
type ArrayShape =
    { /// Specifies the number of dimensions in the array.
      Rank: uint32 // TODO: How to prevent a value of zero?
      /// <summary>Specifies the sizes of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumSizes</c> item and <c>Size</c> items in the signature.</remarks>
      Sizes: ImmutableArray<uint32> // NOTE: The two arrays containing information for each dimension can contain less items than Rank.
      /// <summary>Specifies the lower bounds of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumLoBounds</c> item and <c>LoBound</c> items in the signature.</remarks>
      LowerBounds: ImmutableArray<int32> }

[<IsReadOnly; Struct>]
type Pointer =
    { Modifiers: CustomModifiers; PointerType: EncodedType voption }
    member this.IsVoid = this.PointerType.IsNone

(*
[<RequireQualifiedAccess>]
type Pointer =
    | Type of Modifiers: CustomModifiers * PointerType: EncodedType
    | Void of Modifiers: CustomModifiers
*)

[<RequireQualifiedAccess>]
module Pointer =
    let inline (|Type|Void|) { Modifiers = modifiers; PointerType = ptype } =
        match ptype with
        | ValueSome ptype' -> Type(modifiers, ptype')
        | ValueNone -> Void modifiers

    let inline Type(modifiers, ptype) = { Modifiers = modifiers; PointerType = ValueSome ptype }
    let inline Void modifiers = { Modifiers = modifiers; PointerType = ValueNone }

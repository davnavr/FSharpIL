﻿namespace rec FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module TypeDefOrRefOrSpecEncoded =
    let (|TypeDef|TypeRef|TypeSpec|) (encoded: TypeDefOrRefOrSpecEncoded) =
        match encoded.Tag with
        | TypeDefOrRefOrSpecTag.Def -> TypeDef(encoded.ToRawIndex<TypeDefRow>())
        | TypeDefOrRefOrSpecTag.Ref -> TypeRef(encoded.ToRawIndex<TypeRef>())
        | TypeDefOrRefOrSpecTag.Spec -> TypeSpec(encoded.ToRawIndex<TypeSpecRow>())
        | _ -> invalidArg "encoded" "Invalid encoded type"

    let TypeDef (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex TypeDefOrRefOrSpecTag.Def
    let InterfaceDef (index: RawIndex<InterfaceDef>) = index.ToTaggedIndex TypeDefOrRefOrSpecTag.Def
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex TypeDefOrRefOrSpecTag.Ref
    let TypeSpec (index: RawIndex<TypeSpecRow>) = index.ToTaggedIndex TypeDefOrRefOrSpecTag.Spec

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module CustomModifier =
    let optional (modifierType: TypeDefOrRefOrSpecEncoded) = CustomModifier(false, modifierType)
    let required (modifierType: TypeDefOrRefOrSpecEncoded) = CustomModifier(true, modifierType)

/// II.23.2.13
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

[<IsReadOnly>]
type GenericInst = struct
    val IsValueType: bool
    val Type: TypeDefOrRefOrSpecEncoded
    val GenericArguments: ImmutableArray<EncodedType>

    /// <exception cref="T:System.ArgumentException">The generic argument list is empty.</exception>
    new (t, genericArguments: ImmutableArray<_>, [<Optional; DefaultParameterValue(false)>] isValueType) =
        if genericArguments.IsEmpty then
            invalidArg "genericArguments" "More than one generic arguments must be specified"
        { IsValueType = isValueType
          Type = t
          GenericArguments = genericArguments }

    new (t, isValueType, head, [<ParamArray>] tail: EncodedType[]) =
        let gargs = ImmutableArray.CreateBuilder<EncodedType>(1 + tail.Length)
        gargs.Add head
        gargs.AddRange tail
        GenericInst(t, gargs.ToImmutable(), isValueType)
end

type internal FunctionPointerTag =
    | RefDefault = 1uy
    | RefGeneric = 2uy
    | RefVarArg = 3uy
    | Def = 4uy

/// <summary>Represents a pointer to a function <c>FNPTR</c> (II.23.2.12).</summary>
[<IsReadOnly; Struct>]
type FunctionPointer internal (tag: FunctionPointerTag, index: int32) =
    member internal _.Tag = tag
    member internal _.Index = index

/// Represents a native pointer (II.14.4.1).
[<IsReadOnly; Struct>]
type Pointer (modifiers: ImmutableArray<CustomModifier>, ptype: EncodedType voption) =
    member _.Modifiers = modifiers
    member _.Type = ptype

[<RequireQualifiedAccess>]
module Pointer =
    let inline (|Encoded|Void|) (pointer: Pointer) =
        match pointer.Type with
        | ValueSome ptype -> Encoded(pointer.Modifiers, ptype)
        | ValueNone -> Void pointer.Modifiers
    let inline Encoded modifiers (ptype: EncodedType) = Pointer(modifiers, ValueSome ptype)
    let inline Void modifiers = Pointer(modifiers, ValueNone)
    let inline toType ptype = Encoded ImmutableArray.Empty ptype

[<RequireQualifiedAccess>]
module FunctionPointer =
    let (|Def|Ref|) (signature: FunctionPointer) =
        match signature.Tag with
        | FunctionPointerTag.Def -> Def(Blob<MethodDefSignature> signature.Index)
        | _ ->
            let mutable tag = signature.Tag
            Ref(MethodRefSignature(Unsafe.As &tag, signature.Index))
    let Def (signature: Blob<MethodDefSignature>) = FunctionPointer(FunctionPointerTag.Def, signature.Index)
    let Ref (signature: MethodRefSignature) =
        let mutable tag = signature.Tag
        FunctionPointer(Unsafe.As &tag, signature.Index)

/// <summary>Represents a <c>Type</c> (II.23.2.12).</summary>
[<RequireQualifiedAccess>]
type EncodedType =
    /// <summary>Represents the <see cref="T:System.Boolean"/> type.</summary>
    | Boolean
    /// <summary>Represents the <see cref="T:System.Char"/> type.</summary>
    | Char
    /// <summary>Represents the <see cref="T:System.SByte"/> type.</summary>
    | I1
    /// <summary>Represents the <see cref="T:System.Byte"/> type.</summary>
    | U1
    /// <summary>Represents the <see cref="T:System.Int16"/> type.</summary>
    | I2
    /// <summary>Represents the <see cref="T:System.UInt16"/> type.</summary>
    | U2
    /// <summary>Represents the <see cref="T:System.Int32"/> type.</summary>
    | I4
    /// <summary>Represents the <see cref="T:System.UInt32"/> type.</summary>
    | U4
    /// <summary>Represents the <see cref="T:System.Int64"/> type.</summary>
    | I8
    /// <summary>Represents the <see cref="T:System.UInt64"/> type.</summary>
    | U8
    /// <summary>Represents the <see cref="T:System.Single"/> type.</summary>
    | R4
    /// <summary>Represents the <see cref="T:System.Double"/> type.</summary>
    | R8
    /// <summary>Represents the <see cref="T:System.IntPtr"/> type.</summary>
    | I
    /// <summary>Represents the <see cref="T:System.UIntPtr"/> type.</summary>
    | U
    | Array of EncodedType * ArrayShape
    | Class of TypeDefOrRefOrSpecEncoded
    /// Represents a method pointer (II.14.5).
    | FnPtr of FunctionPointer
    | GenericInst of GenericInst
    /// Represents a generic parameter in a generic method.
    | MVar of number: uint32
    /// <summary>Represents the <see cref="T:System.Object"/> type.</summary>
    | Object
    /// Represents a native pointer (II.14.4.1).
    | Ptr of Pointer
    /// <summary>Represents the <see cref="T:System.String"/> type.</summary>
    | String
    | SZArray of ImmutableArray<CustomModifier> * EncodedType
    | ValueType of TypeDefOrRefOrSpecEncoded
    /// Represents a generic parameter in a generic type definition.
    | Var of number: uint32

    interface IEncodedType

/// <summary>Represents all different possible return types encoded in a <c>RetType</c> (II.23.2.11).</summary>
/// <seealso cref="T:FSharpIL.Metadata.ReturnTypeItem"/>
[<RequireQualifiedAccess>]
type ReturnType =
    | Type of EncodedType
    | ByRefType of EncodedType
    | TypedByRef
    | Void

    interface IReturnType

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module EncodedType =
    let enumDef (enumDef: RawIndex<EnumDef>) = enumDef.ChangeTag() |> TypeDefOrRefOrSpecEncoded.TypeDef |> EncodedType.ValueType

    let typeDefStruct typeDef = TypeDefOrRefOrSpecEncoded.TypeDef typeDef |> EncodedType.ValueType
    let typeRefClass typeRef = TypeDefOrRefOrSpecEncoded.TypeRef typeRef |> EncodedType.Class
    let typeRefStruct typeRef = TypeDefOrRefOrSpecEncoded.TypeRef typeRef |> EncodedType.ValueType

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module ReturnType =
    let item (returnType: ReturnType) = ReturnTypeItem returnType
    let encoded encodedType = ReturnTypeItem(ReturnType.Type encodedType)
    let modified modifiers (returnType: ReturnType) = ReturnTypeItem(modifiers, returnType)
    let itemVoid = ReturnTypeItem ReturnType.Void
    let itemBool = encoded EncodedType.Boolean
    let itemI4 = encoded EncodedType.I4
    let itemU4 = encoded EncodedType.U4
    let itemVar number = EncodedType.Var number |> encoded

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module GenericInst =
    let private inst1 valueType t (gargument: EncodedType) =
        GenericInst(t, valueType, gargument)

    let typeRef1 valueType typeRef gargument =
        inst1 valueType (TypeDefOrRefOrSpecEncoded.TypeRef typeRef) gargument

    let typeDef1 valueType typeDef gargument =
        inst1 valueType (TypeDefOrRefOrSpecEncoded.TypeDef typeDef) gargument

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module ParamItem =
    let getType (param: inref<ParamItem>) =
        ValueOption.map Unsafe.As<EncodedType> param.ParamType

    let inline (|Param|ByRef|TypedByRef|) (param: ParamItem) =
        match getType &param with
        | ValueNone -> TypedByRef
        | ValueSome ptype ->
            match param.Tag with
            | ParamItemTag.ByRef -> ByRef(param.CustomMod, ptype)
            | ParamItemTag.Param
            | _ -> Param(param.CustomMod, ptype)

    let Param modifiers (paramType: EncodedType) = ParamItem(ParamItemTag.Param, modifiers, ValueSome(paramType :> IEncodedType))
    let ByRef modifiers (paramType: EncodedType) = ParamItem(ParamItemTag.ByRef, modifiers, ValueSome(paramType :> IEncodedType))
    let TypedByRef modifiers = ParamItem(ParamItemTag.TypedByRef, modifiers, ValueNone)

    let create paramType = Param ImmutableArray.Empty paramType
    let byRef paramType = ByRef ImmutableArray.Empty paramType
    let typedByRef = TypedByRef ImmutableArray.Empty
    let mvar num = EncodedType.MVar num |> create
    let var num = EncodedType.Var num |> create

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FieldSignature =
    let modified modifiers (fieldType: EncodedType) = FieldSignature(modifiers, fieldType)
    let create fieldType = modified ImmutableArray.Empty fieldType

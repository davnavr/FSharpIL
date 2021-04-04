namespace rec FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type internal TypeDefOrRefOrSpecTag =
    | Def = 0uy
    | Ref = 1uy
    | Spec = 2uy

/// II.23.2.8
[<IsReadOnly; Struct>]
type TypeDefOrRefOrSpecEncoded internal (tag: TypeDefOrRefOrSpecTag, index: int32) =
    member internal _.Tag = tag
    member _.Value = index
    interface ITypeDefOrRefOrSpec

[<RequireQualifiedAccess>]
module TypeDefOrRefOrSpecEncoded =
    let (|TypeDef|TypeRef|TypeSpec|) (encoded: TypeDefOrRefOrSpecEncoded) =
        match encoded.Tag with
        | TypeDefOrRefOrSpecTag.Ref -> TypeRef(RawIndex<TypeRef> encoded.Value)
        | TypeDefOrRefOrSpecTag.Spec -> TypeSpec(RawIndex<TypeSpecRow> encoded.Value)
        | TypeDefOrRefOrSpecTag.Def
        | _ -> TypeDef(RawIndex<TypeDefRow> encoded.Value)

    let TypeDef (index: RawIndex<TypeDefRow>) = TypeDefOrRefOrSpecEncoded(TypeDefOrRefOrSpecTag.Def, index.Value)
    let InterfaceDef (index: RawIndex<InterfaceDef>) = index.ChangeTag() |> TypeDef
    let TypeRef (index: RawIndex<TypeRef>) = TypeDefOrRefOrSpecEncoded(TypeDefOrRefOrSpecTag.Ref, index.Value)
    let TypeSpec (index: RawIndex<TypeSpecRow>) = TypeDefOrRefOrSpecEncoded(TypeDefOrRefOrSpecTag.Spec, index.Value)

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
    | FunctionPointer // of MethodDefSig
    //| FunctionPointer // of MethodRefSig
    | GenericInst of GenericInst
    /// Represents a generic parameter in a generic method.
    | MVar of number: uint32
    /// <summary>Represents the <see cref="T:System.Object"/> type.</summary>
    | Object
    //| PTR // of ?
    //| PTR // of ?
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
    let modified modifiers (paramType: EncodedType) = ParamItem(modifiers, paramType)
    let create paramType = modified ImmutableArray.Empty paramType
    let mvar num = EncodedType.MVar num |> create
    let var num = EncodedType.Var num |> create

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FieldSignature =
    let modified modifiers (fieldType: EncodedType) = FieldSignature(modifiers, fieldType)
    let create fieldType = modified ImmutableArray.Empty fieldType

namespace FSharpIL.Metadata.Signatures

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Tables

/// <summary>Represents an index into the <c>TypeDef</c> or <c>TypeRef</c> table (II.23.2.8).</summary>
[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type TypeDefOrRefEncoded =
    val IsDefinition: bool
    val internal Index: uint32

    internal new: isTypeDef: bool * index: uint32 -> TypeDefOrRefEncoded

    member IsReference: bool

    interface IEquatable<TypeDefOrRefEncoded>

[<RequireQualifiedAccess>]
module TypeDefOrRefEncoded =
    val Def : index: TableIndex<TypeDefRow> -> TypeDefOrRefEncoded
    val Ref : index: TableIndex<TypeRefRow> -> TypeDefOrRefEncoded

    val toCodedIndex : index: TypeDefOrRefEncoded -> TypeDefOrRef

/// Describes the lower bounds, lengths, and number of dimensions of an array (II.23.2.13).
[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type ArrayShape =
    { /// Specifies the number of dimensions in the array.
      Rank: uint32
      /// <summary>Specifies the sizes of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumSizes</c> item and <c>Size</c> items in the signature.</remarks>
      Sizes: ImmutableArray<uint32>
      /// <summary>Specifies the lower bounds of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumLoBounds</c> item and <c>LoBound</c> items in the signature.</remarks>
      LowerBounds: ImmutableArray<int32> }

    interface IEquatable<ArrayShape>

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type MethodThis =
    member Tag: CallConvFlags

    interface IEquatable<MethodThis>

(*
[<IsReadOnly; Struct>]
type MethodThis =
    | NoThis
    | HasThis
    | ExplicitThis
*)

[<AutoOpen>]
module MethodThis =
    val inline (|NoThis|HasThis|ExplicitThis|) : this: MethodThis -> Choice<unit, unit, unit>

    val NoThis : MethodThis
    val HasThis : MethodThis
    val ExplicitThis : MethodThis

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type CallingConventions =
    val Tag: CallConvFlags
    /// <summary>The number of generic parameters, stored in <c>GenParamCount</c> (II.23.2.1).</summary>
    val Count: uint32

    interface IEquatable<CallingConventions>

(*
[<IsReadOnly; Struct>]
type CallingConventions =
    | Default
    | VarArg
    | Generic of Count: uint32
*)

[<AutoOpen>]
module CallingConventions =
    val inline (|Default|VarArg|Generic|) : CallingConventions -> Choice<unit, unit, uint32>

    val Default : CallingConventions
    val VarArg : CallingConventions
    val Generic : count: uint32 -> CallingConventions

/// <summary>Represents a <c>Type</c> (II.23.2.12).</summary>
[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
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
    /// A user-defined reference type.
    | Class of TypeDefOrRefEncoded
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
    /// A single-dimensional array with a lower bound of zero, also known as a vector (I.8.9.1 and II.14.1).
    | SZArray of EncodedType
    /// A user-defined value type.
    | ValueType of TypeDefOrRefEncoded
    /// A generic parameter in a generic type definition.
    | Var of index: uint32
    | Modified of modifiers: CustomMod list * EncodedType

    interface IEquatable<EncodedType>

/// Represents an unmanaged pointer (II.14.4).
and [<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; StructuralEquality>] Pointer =
    | Type of toType: EncodedType
    /// Represents an unmanaged pointer to a value of unknown type.
    | Void of modifiers: CustomMod list

    interface IEquatable<Pointer>

and [<IsReadOnly; Struct>] GenericArgList = // TODO: GenericArgList should use CustomEquality and override GetHashCode
    internal { GenArgs: ImmutableArray<EncodedType> }

    member Count: uint32
    member Item: index: int32 -> EncodedType with get
    member GetEnumerator: unit -> ImmutableArray<EncodedType>.Enumerator

(*
[<RequireQualifiedAccess>]
type GenericInst =
    | Class of GenericType: TypeDefOrRefOrSpecEncoded * GenericArguments: GenericArgList
    | ValueType of GenericType: TypeDefOrRefOrSpecEncoded * GenericArguments: GenericArgList
*)

/// Represents a generic instantiation (II.23.2.12 and II.23.2.14).
and [<IsReadOnly; Struct>] GenericInst =
    { IsValueType: bool
      GenericType: TypeDefOrRefEncoded
      GenericArguments: GenericArgList }

and ParamItemTag =
    | Param = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy

(*
[<RequireQualifiedAccess>]
type ParamItem =
    | Param of ParamType: EncodedType
    | ByRef of CustomModifiers: CustomMod list * ParamType: EncodedType
    | TypedByRef of CustomModifiers: CustomMod list
*)

/// <summary>Represents a <c>Param</c> item used in method signatures (II.23.2.10).</summary>
and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] ParamItem =
    val Tag: ParamItemTag
    val internal Modifiers: CustomMod list
    val ParamType: EncodedType voption // TODO: Consider using Unchecked.defaultof<EncodedType> instead of ValueNone to save 4 bytes.

    internal new: tag: ParamItemTag * modifiers: CustomMod list * paramType: EncodedType voption -> ParamItem

    member inline IsByRef: bool
    member inline IsTypedByRef: bool

    /// <summary>
    /// Gets the custom modifiers defined before the <c>BYREF</c> or <c>TYPEDBYREF</c> byte, or any modifiers defined before the
    /// type if the parameter type is a normal type.
    /// </summary>
    member CustomModifiers: CustomMod list

    interface IEquatable<ParamItem>

and RetTypeTag =
    | Type = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy
    | Void = 3uy

(*
[<RequireQualifiedAccess>]
type RetType =
    | Type of ParamType: EncodedType
    | ByRef of CustomModifiers: CustomModifiers * ParamType: EncodedType
    | TypedByRef of CustomModifiers: CustomModifiers
    | Void of CustomModifiers: CustomModifiers
*)

/// <summary>Represents a <c>RetType</c> item used in method signatures (II.23.2.11).</summary>
and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] RetTypeItem =
    val Tag: RetTypeTag
    val internal Modifiers: CustomMod list
    val ReturnType: EncodedType voption // TODO: Consider using Unchecked.defaultof<EncodedType> instead of ValueNone to save 4 bytes.

    internal new: tag: RetTypeTag * modifiers: CustomMod list * returnType: EncodedType voption -> RetTypeItem

    member inline IsVoid: bool
    member inline IsByRef: bool
    member inline IsTypedByRef: bool

    /// <summary>
    /// Gets the custom modifiers defined before the <c>BYREF</c>, <c>TYPEDBYREF</c>, or <c>VOID</c> byte, or any modifiers
    /// defined before the type if the return type is a normal type.
    /// </summary>
    member CustomModifiers: CustomMod list

    interface IEquatable<RetTypeItem>

/// <summary>Represents a <c>MethodDefSig</c>, which captures the signature of a method or global function (II.23.2.1).</summary>
and [<IsReadOnly; Struct>] MethodDefSig =
    { HasThis: MethodThis
      CallingConvention: CallingConventions
      ReturnType: RetTypeItem
      Parameters: ImmutableArray<ParamItem> }

/// <summary>Represents a <c>MethodRefSig</c>, which captures "the call site signature for a method" (II.23.2.2).</summary>
and [<IsReadOnly; Struct>] MethodRefSig =
    val Signature: MethodDefSig
    val VarArgParams: ImmutableArray<ParamItem>

    member HasThis: MethodThis
    member CallingConvention: CallingConventions
    member ReturnType: RetTypeItem
    member Parameters: ImmutableArray<ParamItem>

/// <summary>
/// Represents a <c>FieldSig</c> item, which captures the definition of a field or global variable (II.23.2.4).
/// </summary>
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type FieldSig =
    { FieldType: EncodedType }

    interface IEquatable<FieldSig>

/// <summary>
/// Represents a <c>PropertySig</c> item, which describes the return type and parameter types of a <c>get_</c> method.
/// </summary>
[<IsReadOnly; Struct>]
type PropertySig =
    { HasThis: bool
      PropertyType: EncodedType
      Parameters: ImmutableArray<ParamItem> }

[<RequireQualifiedAccess>]
module ParamItem =
    val inline (|Type|ByRef|TypedByRef|) :
        param: ParamItem -> Choice<EncodedType, struct(CustomMod list * EncodedType), CustomMod list>

    val Type : paramType: EncodedType -> ParamItem

    val ByRef : modifiers: CustomMod list * toType: EncodedType -> ParamItem

    val ByRef' : toType: EncodedType -> ParamItem

    /// <summary>The <see cref="T:System.TypedReference"/> type, as a parameter type.</summary>
    val TypedByRef : modifiers: CustomMod list -> ParamItem

    /// <summary>The <see cref="T:System.TypedReference"/> type, as a parameter type with no custom modifiers.</summary>
    val TypedByRef' : ParamItem

[<RequireQualifiedAccess>]
module RetTypeItem =
    val inline (|Type|ByRef|TypedByRef|Void|) :
        returnType: RetTypeItem -> Choice<EncodedType, struct(CustomMod list * EncodedType), CustomMod list, CustomMod list>

    val Type : returnType: EncodedType -> RetTypeItem

    val ByRef : modifiers: CustomMod list * toType: EncodedType -> RetTypeItem

    val ByRef' : toType: EncodedType -> RetTypeItem

    /// <summary>The <see cref="T:System.TypedReference"/> type, as a return type.</summary>
    val TypedByRef : modifiers: CustomMod list -> RetTypeItem

    /// <summary>The <see cref="T:System.TypedReference"/> type, as a return type with no custom modifiers.</summary>
    val TypedByRef' : RetTypeItem

    val Void : modifiers: CustomMod list -> RetTypeItem

    val Void' : RetTypeItem

[<RequireQualifiedAccess>]
module MethodRefSig =
    val ofMethodDefSig : signature: MethodDefSig -> MethodRefSig

    val inline Default : hasThis: MethodThis * returnType: RetTypeItem * parameters: ImmutableArray<ParamItem> -> MethodRefSig

    val inline Generic :
        hasThis: MethodThis *
        returnType: RetTypeItem *
        genParamCount: uint32 *
        parameters: ImmutableArray<ParamItem> -> MethodRefSig

    val VarArg :
        hasThis: MethodThis *
        returnType: RetTypeItem *
        parameters: ImmutableArray<ParamItem> *
        varArgParams: ImmutableArray<ParamItem> -> MethodRefSig

[<RequireQualifiedAccess>]
module GenericInst =
    val inline (|Class|ValueType|) :
        inst: GenericInst -> Choice<struct(TypeDefOrRefEncoded * GenericArgList), struct(TypeDefOrRefEncoded * GenericArgList)>

    val inline Class : instantiated: TypeDefOrRefEncoded * genericArgs: GenericArgList -> GenericInst
    val inline ValueType : instantiated: TypeDefOrRefEncoded * genericArgs: GenericArgList -> GenericInst

[<RequireQualifiedAccess>]
module ArrayShape =
    val internal ofVector : ArrayShape

[<RequireQualifiedAccess>]
module Pointer =
    /// An unmanaged pointer type to a value of unknown type, with no custom modifiers.
    val Void' : Pointer

[<RequireQualifiedAccess>]
module EncodedType =
    /// Gets a value indicating whether the specified type is or references a generic parameter in a method.
    val isMethodVar : EncodedType -> bool

    val toElemType : EncodedType -> ElemType voption

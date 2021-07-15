namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<AbstractClass>]
type NamedType =
    member TypeName: Identifier
    member TypeNamespace: Identifier voption
    member EnclosingType: NamedType voption
    member IsNested: bool

    member Equals: other: NamedType -> bool
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<NamedType>

[<AutoOpen>]
module NamedTypePatterns =
    val inline internal (|NamedType|) : #NamedType -> NamedType
    val inline (|IsSystemType|) : expected: string -> #NamedType -> bool
    // TODO: Add cases for unmanaged pointer types.
    //val inline (|DefinedType|ReferencedType|InstantiatedType|ArrayType|PrimitiveType|) : NamedType -> Choice<DefinedType, ReferencedType, InstantiatedType, ArrayType, PrimitiveType>

[<RequireQualifiedAccess>]
module NamedType =
    val toElemType: NamedType -> FSharpIL.Metadata.Blobs.ElemType voption

type GenericParamKind =
    | Invariant
    | Covariant
    | Contravariant

[<IsReadOnly; Struct>]
type GenericSpecialConstraint =
    | NoSpecialConstriant
    | ReferenceTypeConstraint
    | NonNullableValueTypeConstraint

[<NoComparison; CustomEquality>]
type GenericParam = // TODO: Make this inherit NamedType
    { Name: Identifier
      SpecialConstraint: GenericSpecialConstraint
      RequiresDefaultConstructor: bool
      Constraints: ImmutableArray<NamedType> }

    member Equals: other: GenericParam -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericParam>

[<RequireQualifiedAccess>]
module GenericParam =
    val named : Identifier -> GenericParam

[<IsReadOnly>]
[<NoComparison; NoEquality>]
type GenericParamList = struct // TODO: Make this type lazy.
    member Parameters: ImmutableArray<GenericParam>
end

[<RequireQualifiedAccess>]
module GenericParamList =
    val empty : GenericParamList
    val tryOfSeq : seq<GenericParam> -> Result<GenericParamList, GenericParam>
    val tryOfArray : GenericParam[] -> Result<GenericParamList, GenericParam>
    val tryOfBlock : ImmutableArray<GenericParam> -> Result<GenericParamList, GenericParam>
    /// <summary>Creates a generic parameter list from the sequence of generic parameters.</summary>
    /// <exception cref="ArgumentException">The sequence of generic <paramref name="parameters"/> contains a duplicate.</exception>
    val ofSeq : parameters: seq<GenericParam> -> GenericParamList

[<AutoOpen>]
module GenericParamListPatterns =
    val inline (|GenericParamList|): GenericParamList -> ImmutableArray<GenericParam>

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type ModifierType =
    member Required: bool
    member Modifier: NamedType

    new: required: bool * modifier: NamedType -> ModifierType

[<RequireQualifiedAccess>]
module ModifierType =
    val inline Req : modifier: NamedType -> ModifierType
    val inline Opt : modifier: NamedType -> ModifierType

/// Represents a type that is modified by custom modifiers (II.7.1.1).
[<Sealed>]
type ModifiedType =
    inherit NamedType

    val Modifiers: ImmutableArray<ModifierType>
    /// The type that is modified by the custom modifiers.
    val Modified: NamedType

    new: modifiers: ImmutableArray<ModifierType> * modified: NamedType -> ModifiedType

[<Sealed>]
type PrimitiveType = class
    inherit NamedType

    member Encoded: FSharpIL.Metadata.Signatures.EncodedType
end

[<RequireQualifiedAccess>]
module PrimitiveType =
    val Boolean : PrimitiveType
    val Char : PrimitiveType
    val I1 : PrimitiveType
    val U1 : PrimitiveType
    val I2 : PrimitiveType
    val U2 : PrimitiveType
    val I4 : PrimitiveType
    val U4 : PrimitiveType
    val I8 : PrimitiveType
    val U8 : PrimitiveType
    val R4 : PrimitiveType
    val R8 : PrimitiveType
    val I : PrimitiveType
    val U : PrimitiveType
    val Object : PrimitiveType
    val String : PrimitiveType

type ArrayType =
    inherit NamedType

    val ElementType: NamedType
    val Shape: FSharpIL.Metadata.Signatures.ArrayShape

    new: element: NamedType * shape: FSharpIL.Metadata.Signatures.ArrayShape -> ArrayType

/// <summary>
/// Represents a new one-dimensional array type with a zero lower bound, called a vector or <c>SZArray</c> (II.14.1).
/// </summary>
[<Sealed>]
type SZArrayType =
    inherit ArrayType
    new: element: NamedType -> SZArrayType

[<AbstractClass>]
type GenericType =
    inherit NamedType

    /// Gets the generic parameters of this type.
    val GenericParameters: GenericParamList

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type TypeReferenceParent =
    | Null
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

and [<Sealed>] ReferencedType =
    inherit GenericType

    //val Flags: TypeDefFlags voption

    member ResolutionScope: TypeReferenceParent

    internal new: TypeReferenceParent * Identifier voption * Identifier * GenericParamList -> ReferencedType

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    member Extends: NamedType voption
    member IsNull: bool

[<IsReadOnly; Struct>]
type TypeVisibility =
    val Flag: TypeDefFlags
    val internal Parent: DefinedType

    internal new: TypeDefFlags * DefinedType voption -> TypeVisibility

    member IsNested: bool
    member EnclosingClass: DefinedType voption

and DefinedType =
    inherit GenericType

    val Flags: TypeDefFlags
    val Extends: ClassExtends

    internal new:
        flags: TypeDefFlags *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosing: DefinedType voption *
        typeName: Identifier *
        genericParameters: GenericParamList -> DefinedType

    member Visibility: TypeVisibility
    /// Gets the type that contains this nested type (II.22.32).
    member EnclosingClass: DefinedType voption

[<RequireQualifiedAccess>]
module DefinedType =
    val inline (|IsInterface|NotInterface|) : tdef: DefinedType -> Choice<unit, unit>
    //val inline (|IsValueType|NotValueType|) : tdef: DefinedType -> Choice<unit, unit>

[<AutoOpen>]
module internal ModuleType =
    /// <summary>Represents the special <c>&lt;Module&gt;</c> class, which contains global fields and methods (II.10.8).</summary>
    val ModuleType : DefinedType

[<RequireQualifiedAccess>]
module TypeKinds =
    type ConcreteClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type AbstractClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type SealedClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type StaticClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type Delegate = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType
    end

    type Enum = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType
    end

    type Interface = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
    end

    type ValueType = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

[<Sealed>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    inherit DefinedType

    internal new:
        visibility: TypeVisibility *
        flags: TypeAttributes<'Kind> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        genericParameters: GenericParamList -> TypeDefinition<'Kind>

[<RequireQualifiedAccess>]
module TypeVisibility =
    val NotPublic : TypeVisibility
    val Public : TypeVisibility
    val NestedPublic : parent: DefinedType -> TypeVisibility
    val NestedPrivate : parent: DefinedType -> TypeVisibility
    val NestedFamily : parent: DefinedType -> TypeVisibility
    val NestedAssembly : parent: DefinedType -> TypeVisibility
    val NestedFamilyAndAssembly : parent: DefinedType -> TypeVisibility
    val NestedFamilyOrAssembly : parent: DefinedType -> TypeVisibility

[<NoComparison; NoEquality>]
type InstantiatedTypeArgumentsEnumerator = struct
    val mutable internal Index: int32
    val internal Parameters: ImmutableArray<GenericParam>
    val internal Instantiator: int32 -> GenericParam -> NamedType

    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown when the enumerator was not started or when the end of the parameter list has been reached.
    /// </exception>
    member Current: NamedType
    member MoveNext: unit -> bool

    interface System.Collections.Generic.IEnumerator<NamedType>
end

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type InstantiatedTypeArguments = struct
    member Count: int32
    member Item: index: int32 -> NamedType with get

    member Equals: InstantiatedTypeArguments -> bool
    member GetEnumerator: unit -> InstantiatedTypeArgumentsEnumerator

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<InstantiatedTypeArguments>
    interface System.Collections.Generic.IReadOnlyList<NamedType>
end

[<Sealed>]
type InstantiatedType<'Inst when 'Inst :> GenericType> = class
    inherit NamedType

    member Arguments: InstantiatedTypeArguments
    member Instantiated: 'Inst
end

[<RequireQualifiedAccess>]
module GenericType =
    val instantiate : gtype: 'Inst -> instantiator: (int32 -> GenericParam -> NamedType) -> InstantiatedType<'Inst>
    val inline (|Instantiation|) : gtype: InstantiatedType<'Inst> -> struct('Inst * InstantiatedTypeArguments)

[<RequireQualifiedAccess>]
module ClassExtends =
    val inline (|Null|Defined|Referenced|DefinedGeneric|ReferencedGeneric|):
        ClassExtends ->
        Choice<unit, DefinedType, ReferencedType, InstantiatedType<DefinedType>, InstantiatedType<ReferencedType>>

    val Null: ClassExtends
    val Defined: extends: DefinedType -> ClassExtends
    val DefinedGeneric: extends: InstantiatedType<DefinedType> -> ClassExtends
    val Referenced: extends: ReferencedType -> ClassExtends
    val ReferencedGeneric: extends: InstantiatedType<ReferencedType> -> ClassExtends

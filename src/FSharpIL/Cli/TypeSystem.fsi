/// Contains types modeling the Common Type System (I.8 and II.7.1)
[<AutoOpen>]
module FSharpIL.Cli.TypeSystem

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type PrimitiveType =
    member Encoded: FSharpIL.Metadata.Signatures.EncodedType

    interface IEquatable<PrimitiveType>

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

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type GenericParamKind =
    | Invariant
    | Covariant
    | Contravariant

    interface IEquatable<GenericParamKind>

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type GenericSpecialConstraint =
    | NoSpecialConstriant
    | ReferenceTypeConstraint
    | NonNullableValueTypeConstraint

    interface IEquatable<GenericSpecialConstraint>

[<Sealed>]
type GenericParamType =
    member Sequence: uint16
    member Flags: GenericParamFlags
    member Name: Identifier

    member Kind: GenericParamKind
    member SpecialConstraint: GenericSpecialConstraint
    member RequiresDefaultConstructor: bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericParamType>

[<IsReadOnly; NoComparison; NoEquality>]
type private GenericParamListIndexer = struct end

[<Struct; NoComparison; NoEquality>]
type GenericParamListEnumerator =
    val mutable private i: int32
    val private indexer: GenericParamListIndexer

    member Current: GenericParamType
    member MoveNext: unit -> bool

    interface IEnumerator<GenericParamType>

[<IsReadOnly; Struct; NoComparison; CustomEquality>]
type GenericParamList =
    member Count : int32
    /// <summary>Gets the generic parameter at the specified index.</summary>
    /// <param name="index">The index of the generic parameter to retrieve.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> is negative or exceeds the index of the last generic parameter in the list.
    /// </exception>
    member Item: index: int32 -> GenericParamType with get
    member GetEnumerator: unit -> GenericParamListEnumerator

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEnumerable<GenericParamType>
    interface IReadOnlyCollection<GenericParamType>
    interface IReadOnlyList<GenericParamType>
    interface IEquatable<GenericParamList>

[<Sealed>]
type GenericType<'Type when 'Type : not struct> =
    member Type: 'Type
    member Parameters: GenericParamList

val inline (|GenericType|) : GenericType<'Type> -> struct('Type * GenericParamList)

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type TypeReferenceParent =
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

    interface IEquatable<TypeReferenceParent>

/// Represents a type defined outside of the current module.
and [<NoComparison; CustomEquality>] TypeReference =
    { Flags: TypeDefFlags voption
      ResolutionScope: TypeReferenceParent
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    interface IEquatable<TypeReference>

and [<RequireQualifiedAccess; NoComparison; CustomEquality>] ReferencedType =
    | Reference of TypeReference
    | Generic of GenericType<TypeReference>

    interface IEquatable<ReferencedType>

/// Represents a type in the type system of the Common Language Infrastructure (II.7.1).
[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type CliType =
    | Primitive of PrimitiveType
    /// A single-dimensional array with a lower bound of zero, also known as a vector (I.8.9.1 and II.14.1).
    | SZArray of CliType
    | Array of shape: FSharpIL.Metadata.Signatures.ArrayShape * etype: CliType
    /// Represents a type that is modified by custom modifiers (II.7.1.1).
    | Modified of modified: ImmutableArray<ModifierType> * CliType
    /// A user-defined reference type.
    | Class of NamedType
    | ValueType of NamedType
    //| Pointer of UnmanagedPointerType
    | Var of GenericParamType

    interface IEquatable<CliType>

and [<RequireQualifiedAccess; NoComparison; CustomEquality>] DefinedType =
    | Definition of TypeDefinition
    | Generic of GenericType<TypeDefinition>

    interface IEquatable<DefinedType>

and [<NoComparison; CustomEquality>] NamedType =
    /// Represents a type defined in the current module.
    | DefinedType of DefinedType
    /// Represents a type defined outside of the current module.
    | ReferencedType of ReferencedType

    interface IEquatable<NamedType>

and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] ClassExtends = interface IEquatable<ClassExtends>

and [<NoComparison; CustomEquality>] TypeDefinition =
    { Flags: TypeDefFlags
      Extends: ClassExtends
      /// The type that contains this nested type (II.22.32).
      EnclosingClass: DefinedType voption
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<TypeDefinition>

and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] ModifierType = interface IEquatable<ModifierType>

[<IsReadOnly; Struct; NoComparison; CustomEqualityAttribute>]
type GenericParamOwner =
    interface IEquatable<GenericParamOwner>

type GenericParamType with
    /// TODO: Don't forget to skip duplicate while writing generic constraints. (Maybe just make an error when the CliModuleBuilder first gets a hold of a duplicate)
    member Constraints: ImmutableArray<CliType> // TODO: Instead, make custom colelction type w/ a ctor function that explicitly says it will ignore attributes.
    member Owner: GenericParamOwner

    internal new:
        owner: GenericParamOwner *
        i: uint16 *
        flags: GenericParamFlags *
        name: Identifier *
        constraints: ImmutableArray<CliType> -> GenericParamType

type ModifierType with
    member Required: bool
    member Modifier: CliType

    new: required: bool * modifier: CliType -> ModifierType

type NamedType with
    member TypeName: Identifier
    member TypeNamespace: Identifier voption
    member EnclosingType: NamedType voption

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type TypeVisibility =
    member Flag: TypeDefFlags
    member EnclosingClass: DefinedType voption

    internal new: flags: TypeDefFlags * parent: DefinedType voption -> TypeVisibility

    interface IEquatable<TypeVisibility>

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

type TypeDefinition with
    member Visibility: TypeVisibility
    member IsNested: bool

type ClassExtends with
    member Extends: CliType voption
    member IsNull: bool

[<RequireQualifiedAccess>]
module ClassExtends =
    val Null: ClassExtends
    val Class: extends: NamedType -> ClassExtends

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: ImmutableArray<CliType>

    internal new: name: Identifier * flags: GenericParamFlags * constraints: ImmutableArray<CliType> -> GenericParam

    new:
        name: Identifier *
        kind: GenericParamKind *
        special: GenericSpecialConstraint *
        requiresDefaultConstructor: bool *
        constraints: ImmutableArray<CliType> -> GenericParam

type GenericType<'Type when 'Type : not struct> with
    new: 'Type * parameters: ImmutableArray<GenericParam> -> GenericType<'Type>

[<RequireQualifiedAccess>]
module TypeKinds =
    type IHasConstructor = interface end

    type ConcreteClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type AbstractClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type SealedClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type StaticClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type Delegate = struct
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
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
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

[<IsReadOnly; Struct>]
[<NoComparison; NoEquality>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    member Definition: TypeDefinition

val inline (|TypeDefinition|): definition: TypeDefinition<'Kind> -> TypeDefinition

type TypeDefinition with
    static member ConcreteClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ConcreteClass> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.ConcreteClass>

    static member AbstractClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.AbstractClass> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.AbstractClass>

    static member SealedClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.SealedClass> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.SealedClass>

    static member StaticClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.StaticClass> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.StaticClass>

    static member Interface:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.Interface> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.Interface>

    static member ValueType:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ValueType> *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier -> TypeDefinition<TypeKinds.ValueType>

    //static member Delegate
    //static member Enum

[<IsReadOnly; Struct>]
[<NoComparison; NoEquality>]
type TypeReference<'Kind when 'Kind :> IAttributeTag<TypeDefFlags>> =
    member Reference: TypeReference

val inline (|TypeReference|): reference: TypeReference<'Kind> -> TypeReference

type TypeReference with
    static member ConcreteClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.ConcreteClass>




    static member SealedClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.SealedClass>

    static member StaticClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.StaticClass>

[<AutoOpen>]
module internal ModuleType =
    /// <summary>Represents the special <c>&lt;Module&gt;</c> class, which contains global fields and methods (II.10.8).</summary>
    val ModuleType : TypeDefinition

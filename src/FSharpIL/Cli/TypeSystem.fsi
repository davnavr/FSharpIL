﻿/// Contains types modeling the Common Type System (I.8 and II.7.1)
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

/// Marker interface used to indicate that type references and type definitions can own generic parameters.
type [<Interface>] IGenericType<'This> = inherit IEquatable<'This>

[<Sealed>]
type GenericParamType<'Owner when 'Owner :> IGenericType<'Owner>> =
    member Number: uint16
    member Flags: GenericParamFlags
    member Name: Identifier

    member Kind: GenericParamKind
    member SpecialConstraint: GenericSpecialConstraint
    member RequiresDefaultConstructor: bool

    override ToString: unit -> string
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericParamType<'Owner>>

and [<IsReadOnly; NoComparison; NoEquality>] private GenericParamListIndexer<'Owner when 'Owner :> IGenericType<'Owner>> =
    struct end

// TODO: Replace enumerator for generic parameters which just using a for loop.
and [<Struct; NoComparison; NoEquality>] GenericParamListEnumerator<'Owner when 'Owner :> IGenericType<'Owner>> =
    val mutable private i: int32
    val private indexer: GenericParamListIndexer<'Owner>

    member Current: GenericParamType<'Owner>
    member MoveNext: unit -> bool

    interface IEnumerator<GenericParamType<'Owner>>

and [<IsReadOnly; Struct; NoComparison; CustomEquality>] GenericParamList<'Owner when 'Owner :> IGenericType<'Owner>> =
    member Count : int32
    /// <summary>Gets the generic parameter at the specified index.</summary>
    /// <param name="index">The index of the generic parameter to retrieve.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> is negative or exceeds the index of the last generic parameter in the list.
    /// </exception>
    member Item: index: int32 -> GenericParamType<'Owner> with get
    member GetEnumerator: unit -> GenericParamListEnumerator<'Owner>

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEnumerable<GenericParamType<'Owner>>
    interface IReadOnlyCollection<GenericParamType<'Owner>>
    interface IReadOnlyList<GenericParamType<'Owner>>
    interface IEquatable<GenericParamList<'Owner>>

and [<Sealed>] GenericType<'Type when 'Type : not struct and 'Type :> IGenericType<'Type>> =
    member Type: 'Type
    member ParameterTypes: GenericParamList<'Type>

    override ToString: unit -> string

val inline (|GenericType|) : GenericType<'Type> -> struct('Type * GenericParamList<'Type>)

[<RequireQualifiedAccess; NoComparison; CustomEquality>]
type TypeReferenceParent =
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

    override Equals: obj -> bool
    override GetHashCode: unit -> int32
    override ToString: unit -> string

    interface IEquatable<TypeReferenceParent>

/// Describes the location of a type defined outside of the current module (II.7.3).
and [<NoComparison; CustomEquality>] TypeReference =
    { Flags: TypeDefFlags voption
      ResolutionScope: TypeReferenceParent
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    override ToString: unit -> string

    interface IEquatable<TypeReference>
    interface IGenericType<TypeReference>

/// Represents a type defined outside of the current module (II.7.3).
and [<RequireQualifiedAccess; NoComparison; CustomEquality>] ReferencedType =
    | Reference of TypeReference
    | Generic of GenericType<TypeReference>

    override ToString: unit -> string
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<ReferencedType>

/// Represents a type in the type system of the Common Language Infrastructure (II.7.1).
[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type CliType =
    | Primitive of PrimitiveType
    /// A single-dimensional array with a lower bound of zero, also known as a vector (I.8.9.1 and II.14.1).
    | SZArray of CliType
    | Array of CliType * shape: FSharpIL.Metadata.Signatures.ArrayShape
    /// Represents a type that is modified by custom modifiers (II.7.1.1).
    | Modified of modifiers: ImmutableArray<ModifierType> * modified: CliType
    /// A user-defined reference type.
    | Class of NamedType
    /// A user-defined value type.
    | ValueType of NamedType
    /// A generic instantiation of a user-defined reference type.
    | GenericClass of GenericTypeInstantiation
    /// A generic instantiation of a user-defined value type.
    | GenericValueType of GenericTypeInstantiation
    //| Pointer of UnmanagedPointerType
    | TypeDefVar of GenericParamType<TypeDefinition>
    | TypeRefVar of GenericParamType<TypeReference>
    // TODO: Allow using of GenericParamType<SomeMethod>
    //| MethodTypeVar

    override ToString: unit -> string

    interface IEquatable<CliType>

/// Represents the generic arguments of a generic type instantiation.
and [<IsReadOnly; Struct; NoComparison; CustomEquality>] GenericArgumentList =
    member Count: int32
    /// <summary>Gets the generic argument at the specified index.</summary>
    /// <param name="index">The index of the generic argument to retrieve.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> is negative or exceeds the index of the last generic argument in the list.
    /// </exception>
    member Item: index: int32 -> CliType with get

    member ToImmutableArray: unit -> ImmutableArray<CliType>

    override ToString: unit -> string
    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface IEquatable<GenericArgumentList>
    //interface IReadOnlyList<CliType>

and [<Sealed>] GenericTypeInstantiation =
    val Instantiated: GenericType
    val Arguments: GenericArgumentList

    override ToString: unit -> string
    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface IEquatable<GenericTypeInstantiation>

and [<IsReadOnly; Struct; NoComparison; NoEquality>] GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: ImmutableArray<TypeTok>

/// Represents a type defined in a module or assembly (II.7.3).
and [<RequireQualifiedAccess; NoComparison; CustomEquality>] DefinedType =
    | Definition of TypeDefinition
    | Generic of GenericType<TypeDefinition>

    override ToString: unit -> string
    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface IEquatable<DefinedType>

/// Represents a type defined in a module or assembly (II.7.3).
and [<NoComparison; CustomEquality>] NamedType =
    /// Represents a type defined in the current module.
    | DefinedType of DefinedType
    /// Represents a type defined outside of the current module.
    | ReferencedType of ReferencedType

    override ToString: unit -> string

    interface IEquatable<NamedType>

and [<IsReadOnly; Struct; NoComparison; CustomEquality; RequireQualifiedAccess>] GenericType =
    | Defined of definition: GenericType<TypeDefinition>
    | Referenced of reference: GenericType<TypeReference>

    override ToString: unit -> string
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericType>

and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] ClassExtends = interface IEquatable<ClassExtends>

/// Describes the characteristics of a type defined in the current module (II.10).
and [<NoComparison; CustomEquality>] TypeDefinition =
    { Flags: TypeDefFlags
      Extends: ClassExtends
      /// The type that contains this nested type (II.22.32).
      EnclosingClass: DefinedType voption
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    override ToString: unit -> string
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<TypeDefinition>
    interface IGenericType<TypeDefinition>

and [<IsReadOnly; Struct; NoComparison; StructuralEquality>] ModifierType = interface IEquatable<ModifierType>

and [<IsReadOnly; Struct; NoComparison; StructuralEquality; RequireQualifiedAccess>] TypeTok =
    | Named of NamedType
    | Specified of spec: CliType

    override ToString: unit -> string

    interface IEquatable<TypeTok>

[<Sealed>]
type GenericParamType<'Owner when 'Owner :> IGenericType<'Owner>> with
    /// TODO: Don't forget to skip duplicate while writing generic constraints. (Maybe just make an error when the CliModuleBuilder first gets a hold of a duplicate)
    member Constraints: ImmutableArray<TypeTok> // TODO: Instead, make custom collection type w/ a ctor function that explicitly says it will ignore attributes.
    member Owner: 'Owner

    internal new:
        owner: 'Owner *
        i: uint16 *
        flags: GenericParamFlags *
        name: Identifier *
        constraints: ImmutableArray<TypeTok> -> GenericParamType<'Owner>

    override ToString: unit -> string

val inline (|GenericParamIndex|) : parameter: GenericParamType<'Owner> -> uint16

[<RequireQualifiedAccess>]
module PrimitiveType =
    val Boolean : CliType
    val Char : CliType
    val I1 : CliType
    val U1 : CliType
    val I2 : CliType
    val U2 : CliType
    val I4 : CliType
    val U4 : CliType
    val I8 : CliType
    val U8 : CliType
    val R4 : CliType
    val R8 : CliType
    val I : CliType
    val U : CliType
    val Object : CliType
    val String : CliType

type ModifierType with
    member Required: bool
    member Modifier: TypeTok

    new: required: bool * modifier: TypeTok -> ModifierType

    override ToString: unit -> string

type NamedType with
    member inline TypeName: Identifier
    member inline TypeNamespace: Identifier voption
    member inline EnclosingType: NamedType voption

type DefinedType with
    member inline EnclosingClass: DefinedType voption

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type TypeVisibility =
    member Flag: TypeDefFlags
    member EnclosingClass: DefinedType voption

    internal new: flag: TypeDefFlags * parent: DefinedType voption -> TypeVisibility

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
    member Extends: TypeTok voption
    member IsNull: bool

[<RequireQualifiedAccess>]
module ClassExtends =
    val Null: ClassExtends
    val Named: extends: NamedType -> ClassExtends

type GenericParam with
    new:
        name: Identifier *
        kind: GenericParamKind *
        special: GenericSpecialConstraint *
        requiresDefaultConstructor: bool *
        constraints: ImmutableArray<TypeTok> -> GenericParam

    new: name: Identifier -> GenericParam

    internal new: name: Identifier * flags: GenericParamFlags * constraints: ImmutableArray<TypeTok> -> GenericParam

type GenericType<'Type when 'Type : not struct and 'Type :> IGenericType<'Type>> with
    member internal Parameters: ImmutableArray<GenericParam>

[<RequireQualifiedAccess>]
module TypeKinds =
    /// Marker interface used to indicate that certain type definitions are allowed to have object constructors.
    type IHasConstructors = interface end
    /// Marker interface used to indicate that certain type definitions are allowed to have instance methods.
    type IHasInstanceMethods = interface end

    type [<Struct>] ConcreteClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface IHasInstanceMethods
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] AbstractClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface IHasInstanceMethods
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] SealedClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface IHasInstanceMethods
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] StaticClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] Delegate =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface TypeAttributes.ISerializableType

    type [<Struct>] Enum =
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType

    type [<Struct>] Interface =
        interface IAttributeTag<TypeDefFlags>
        interface IHasInstanceMethods // Supports default interface methods.
        interface TypeAttributes.IHasStaticMethods

    type [<Struct>] ValueType =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructors
        interface IHasInstanceMethods
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    member Definition: TypeDefinition

    override ToString: unit -> string

    interface IEquatable<TypeDefinition<'Kind>>

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
[<NoComparison; CustomEquality>]
type TypeReference<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    member Reference: TypeReference

    override ToString: unit -> string

    interface IEquatable<TypeReference<'Kind>>

val inline (|TypeReference|): reference: TypeReference<'Kind> -> TypeReference

type TypeReference with
    static member ConcreteClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.ConcreteClass>

    static member AbstractClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.AbstractClass>

    static member SealedClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.SealedClass>

    static member StaticClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.StaticClass>

/// Contains functions for defining and referencing generic types.
[<RequireQualifiedAccess>]
module GenericType =
    [<IsReadOnly; Struct; NoComparison; StructuralEquality>]
    type Definition<'Kind when 'Kind : struct and 'Kind :> TypeAttributes.Tag> =
        member Definition: GenericType<TypeDefinition>
        member Parameters: GenericParamList<TypeDefinition>

        interface IEquatable<Definition<'Kind>>

    type ArgumentInitializer = ImmutableArray<GenericParam> -> int32 -> CliType

    val defined: parameters: ImmutableArray<GenericParam> -> definition: TypeDefinition -> GenericType<TypeDefinition>
    // TODO: For certain kinds of generic types, don't allow certain variance kinds.
    val definedKind: parameters: ImmutableArray<GenericParam> -> definition: TypeDefinition<'Kind> -> Definition<'Kind>
    val referenced: parameters: ImmutableArray<GenericParam> -> reference: TypeReference -> GenericType<TypeReference>

    val instantiate: instantiated: GenericType -> arguments: ArgumentInitializer -> GenericTypeInstantiation

/// Describes the type of a local variable (II.23.2.6).
[<NoComparison; StructuralEquality; RequireQualifiedAccess>]
type LocalType =
    | T of modifiers: ImmutableArray<ModifierType> * pinned: bool * CliType
    | ByRef of modifiers: ImmutableArray<ModifierType> * pinned: bool * CliType
    | TypedByRef of modifiers: ImmutableArray<ModifierType>

    member CustomModifiers: ImmutableArray<ModifierType>

    /// <summary>
    /// Gets a value indicating whether the value pointed to by this local variable can move during garbage collection (II.23.2.9).
    /// </summary>
    /// <returns>
    /// <see langword="true"/> if the value pointed to by this local variable cannot be moved during garbage collection;
    /// otherwise <see langword="false"/>.
    /// </returns>
    member IsPinned: bool

    interface IEquatable<LocalType>

[<RequireQualifiedAccess>]
module LocalType =
    val TypedByRef' : LocalType

[<RequireQualifiedAccess>]
module CliType =
    /// Attempts to convert the specified type to a type that can be used in the constructor of a custom attribute.
    val toElemType: CliType -> FSharpIL.Metadata.Blobs.ElemType voption

    val inline toLocalType: CliType -> LocalType

[<NoComparison; NoEquality>]
type internal NamedTypeCache

[<RequireQualifiedAccess>]
module internal NamedTypeCache =
    val empty: definedTypeCapacity: int32 -> referencedTypeCapacity: int32 -> NamedTypeCache
    val addDefined: defined: DefinedType -> cache: NamedTypeCache -> NamedType
    val addReferenced: referenced: ReferencedType -> cache: NamedTypeCache -> NamedType

[<RequireQualifiedAccess>]
module internal ModuleType =
    /// <summary>Represents the special <c>&lt;Module&gt;</c> class, which contains global fields and methods (II.10.8).</summary>
    val Definition : TypeDefinition
    val Definition' : DefinedType
    val Named : NamedType

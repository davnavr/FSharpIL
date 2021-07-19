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
type GenericParamType<'Owner when 'Owner :> IEquatable<'Owner>> =
    member Sequence: uint16
    member Flags: GenericParamFlags
    member Name: Identifier

    member Kind: GenericParamKind
    member SpecialConstraint: GenericSpecialConstraint
    member RequiresDefaultConstructor: bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericParamType<'Owner>>

and [<IsReadOnly; NoComparison; NoEquality>] private GenericParamListIndexer<'Owner when 'Owner :> IEquatable<'Owner>> =
    struct end

and [<Struct; NoComparison; NoEquality>] GenericParamListEnumerator<'Owner when 'Owner :> IEquatable<'Owner>> =
    val mutable private i: int32
    val private indexer: GenericParamListIndexer<'Owner>

    member Current: GenericParamType<'Owner>
    member MoveNext: unit -> bool

    interface IEnumerator<GenericParamType<'Owner>>

and [<IsReadOnly; Struct; NoComparison; CustomEquality>] GenericParamList<'Owner when 'Owner :> IEquatable<'Owner>> =
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

and [<Sealed>] GenericType<'Type when 'Type : not struct and 'Type :> IEquatable<'Type>> =
    member Type: 'Type
    member Parameters: GenericParamList<'Type>

val inline (|GenericType|) : GenericType<'Type> -> struct('Type * GenericParamList<'Type>)

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

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

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
    | DefinedTypeVar of GenericParamType<DefinedType>
    | ReferencedTypeVar of GenericParamType<ReferencedType>
    // TODO: Allow using of GenericParamType<SomeMethod>
    //| DefinedMethodVar

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

type GenericParamType<'Owner when 'Owner :> IEquatable<'Owner>> with
    /// TODO: Don't forget to skip duplicate while writing generic constraints. (Maybe just make an error when the CliModuleBuilder first gets a hold of a duplicate)
    member Constraints: ImmutableArray<CliType> // TODO: Instead, make custom colelction type w/ a ctor function that explicitly says it will ignore attributes.
    member Owner: 'Owner

    internal new:
        owner: 'Owner *
        i: uint16 *
        flags: GenericParamFlags *
        name: Identifier *
        constraints: ImmutableArray<CliType> -> GenericParamType<'Owner>

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

type GenericType<'Type when 'Type : not struct and 'Type :> IEquatable<'Type>> with
    new: 'Type * parameters: ImmutableArray<GenericParam> -> GenericType<'Type>

[<RequireQualifiedAccess>]
module TypeKinds =
    type IHasConstructor = interface end

    type [<Struct>] ConcreteClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] AbstractClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] SealedClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] StaticClass =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

    type [<Struct>] Delegate =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.ISerializableType

    type [<Struct>] Enum =
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType

    type [<Struct>] Interface =
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods

    type [<Struct>] ValueType =
        interface IAttributeTag<TypeDefFlags>
        interface IHasConstructor
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType

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

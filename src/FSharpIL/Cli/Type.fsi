namespace rec FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type GenericParamKind =
    | Invariant
    | Covariant
    | Contravariant

[<IsReadOnly; Struct>]
type GenericSpecialConstraint =
    | NoSpecialConstriant
    | ReferenceTypeConstraint
    | NonNullableValueTypeConstraint

[<CustomComparison; CustomEquality>]
type GenericParam =
    { Name: Identifier
      SpecialConstraint: GenericSpecialConstraint
      RequiresDefaultConstructor: bool
      Constraints: ImmutableArray<TypeDefOrRefOrSpec> }

    member Equals: other: GenericParam -> bool
    member CompareTo: other: GenericParam -> int32

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IComparable
    interface IComparable<GenericParam>
    interface IEquatable<GenericParam>

[<RequireQualifiedAccess>]
module GenericParam =
    val named: Identifier -> GenericParam

[<IsReadOnly>]
[<NoComparison; NoEquality>]
type GenericParamList = struct
    member Parameters: ImmutableArray<GenericParam>
end

[<RequireQualifiedAccess>]
module GenericParamList =
    val empty: GenericParamList
    val ofSet: Set<GenericParam> -> GenericParamList
    val tryOfSeq: seq<GenericParam> -> Result<GenericParamList, GenericParam>
    val tryOfArray: GenericParam[] -> Result<GenericParamList, GenericParam>
    val tryOfBlock: ImmutableArray<GenericParam> -> Result<GenericParamList, GenericParam>
    /// <summary>Creates a generic parameter list from the sequence of generic parameters.</summary>
    /// <exception cref="ArgumentException">The sequence of generic <paramref name="parameters"/> contains a duplicate.</exception>
    val ofSeq: parameters: seq<GenericParam> -> GenericParamList

[<AutoOpen>]
module GenericParamListPatterns =
    val inline (|GenericParamList|): GenericParamList -> ImmutableArray<GenericParam>

[<AbstractClass>]
type Type = // TODO: Rename to NamedType.
    member TypeName: Identifier
    member TypeNamespace: Identifier voption
    member EnclosingType: Type voption
    member IsNested: bool

    member Equals: Type -> bool
    member CompareTo: Type -> int32
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<Type>
    interface IComparable<Type>
    interface IComparable

type TypeSpec = FSharpIL.Metadata.Signatures.EncodedType<Type, TypeDefOrRefOrSpec>

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type TypeSpecification = { Spec: TypeSpec }

// TODO: Come up with a better name, maybe Type or MetadataType?
[<IsReadOnly>]
[<CustomComparison; CustomEquality>]
type TypeDefOrRefOrSpec = struct
    internal new: IComparable -> TypeDefOrRefOrSpec

    member Type: IComparable
    member IsRef: bool
    member IsDef: bool
    member IsSpec: bool

    member Equals: TypeDefOrRefOrSpec -> bool
    member CompareTo: TypeDefOrRefOrSpec -> int32
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<TypeDefOrRefOrSpec>
    interface IComparable<TypeDefOrRefOrSpec>
    interface IComparable
end

[<RequireQualifiedAccess>]
module TypeDefOrRefOrSpec =
    val Def: DefinedType -> TypeDefOrRefOrSpec
    val Ref: ReferencedType -> TypeDefOrRefOrSpec
    val Spec: TypeSpecification -> TypeDefOrRefOrSpec
    val inline (|Def|Ref|Spec|): TypeDefOrRefOrSpec -> Choice<DefinedType, ReferencedType, TypeSpec>

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

[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type TypeReferenceParent =
    | Null
    | Type of ReferencedType
    | Assembly of AssemblyReference
    //| Module of ModuleReference

[<AbstractClass>]
type ReferencedType =
    inherit Type
    val ResolutionScope: TypeReferenceParent

[<Sealed>]
type TypeReference<'Kind> = class
    inherit ReferencedType
end

type ReferencedType with
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

[<IsReadOnly>]
type TypeVisibility = struct
    val Flag: TypeDefFlags
    val internal Parent: DefinedType

    internal new: TypeDefFlags * DefinedType voption -> TypeVisibility

    member IsNested: bool
    member EnclosingClass: DefinedType voption
end

[<RequireQualifiedAccess>]
module TypeVisibility =
    val NotPublic: TypeVisibility
    val Public: TypeVisibility
    //val NestedPublic

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type ClassExtends = struct
    internal new: TypeDefOrRefOrSpec -> ClassExtends

    member Extends: TypeDefOrRefOrSpec voption
    member IsNull: bool
end

[<RequireQualifiedAccess>]
module ClassExtends =
    /// <summary>
    /// The class does not extend another type, used for <see cref="T:System.Object"/> and the special class
    /// <c>&lt;Module&gt;</c>.
    /// </summary>
    val Null: ClassExtends
    val ConcreteDef: TypeDefinition<TypeKinds.ConcreteClass> -> ClassExtends
    val AbstractDef: TypeDefinition<TypeKinds.AbstractClass> -> ClassExtends
    val ConcreteRef: TypeReference<TypeKinds.ConcreteClass> -> ClassExtends
    val AbstractRef: TypeReference<TypeKinds.AbstractClass> -> ClassExtends
    val Spec: TypeSpecification -> ClassExtends
    val inline (|Null|ConcreteDef|AbstractDef|ConcreteRef|AbstractRef|Spec|):
        ClassExtends ->
            Choice<unit,
                   TypeDefinition<TypeKinds.ConcreteClass>,
                   TypeDefinition<TypeKinds.AbstractClass>,
                   TypeReference<TypeKinds.ConcreteClass>,
                   TypeReference<TypeKinds.AbstractClass>,
                   TypeSpec>

[<AbstractClass>]
type DefinedType =
    inherit Type
    val Flags: TypeDefFlags
    val Extends: ClassExtends
    /// Gets the generic parameters of this type.
    val GenericParameters: GenericParamList // TODO: Have list of generic parameters be in Type base class.

    member Visibility: TypeVisibility
    /// Gets the type that contains this nested type (II.22.32).
    member EnclosingClass: DefinedType voption

/// <summary>Represents the special <c>&lt;Module&gt;</c> class, which contains global fields and methods (II.10.8).</summary>
[<Sealed>]
type ModuleType = class
    inherit DefinedType
end

[<Sealed>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> = class
    inherit DefinedType
end

type DefinedType with
    static member ConcreteClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ConcreteClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.ConcreteClass>

    static member AbstractClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.AbstractClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.AbstractClass>

    static member SealedClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.SealedClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.SealedClass>

    static member StaticClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.StaticClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.StaticClass>

    static member Interface:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.Interface> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.Interface>

    static member ValueType:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ValueType> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends *
        genericParameters: GenericParamList -> TypeDefinition<TypeKinds.ValueType>

    //static member Delegate
    //static member Enum

[<AutoOpen>]
module TypePatterns =
    val internal ModuleType: ModuleType
    val inline (|AsDefinedType|): #DefinedType -> DefinedType
    val inline (|AsReferencedType|): #ReferencedType -> ReferencedType
    val inline (|DefinedType|ReferencedType|): Type -> Choice<DefinedType, ReferencedType>

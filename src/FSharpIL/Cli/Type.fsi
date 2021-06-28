namespace rec FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<AbstractClass>]
type Type = // TODO: Rename to NamedType.
    member TypeName: Identifier
    member TypeNamespace: Identifier voption
    member EnclosingType: Type voption

    internal new: Identifier voption * Type voption * Identifier -> Type

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

    internal new: TypeReferenceParent * Identifier voption * Identifier -> ReferencedType

[<Sealed>]
type TypeReference<'Kind> =
    inherit ReferencedType
    internal new: TypeReferenceParent * Identifier voption * Identifier -> TypeReference<'Kind>

type ReferencedType with
    static member ConcreteClass:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> TypeReference<TypeKinds.ConcreteClass>

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

    internal new:
        TypeVisibility *
        TypeDefFlags *
        typeNamespace: Identifier voption *
        parent: DefinedType voption *
        Identifier *
        ClassExtends -> DefinedType

    member Visibility: TypeVisibility
    /// Gets the type that contains this nested type (II.22.32).
    member EnclosingClass: DefinedType voption

[<Sealed>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    inherit DefinedType
    internal new:
        TypeVisibility *
        TypeAttributes<'Kind> *
        Identifier voption *
        enclosingClass: DefinedType voption *
        Identifier *
        ClassExtends -> TypeDefinition<'Kind>

type DefinedType with
    static member ConcreteClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ConcreteClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.ConcreteClass>

    static member AbstractClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.AbstractClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.AbstractClass>

    static member SealedClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.SealedClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.SealedClass>

    static member StaticClass:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.StaticClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.StaticClass>

    static member Interface:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.Interface> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.Interface>

    static member ValueType:
        visibility: TypeVisibility *
        flags: TypeAttributes<TypeKinds.ValueType> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        extends: ClassExtends -> TypeDefinition<TypeKinds.ValueType>

    //static member Delegate
    //static member Enum

[<AutoOpen>]
module TypePatterns =
    val inline (|AsDefinedType|): #DefinedType -> DefinedType
    val inline (|AsReferencedType|): #ReferencedType -> ReferencedType
    val inline (|DefinedType|ReferencedType|): Type -> Choice<DefinedType, ReferencedType>

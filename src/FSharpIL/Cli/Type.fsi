namespace rec FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<AbstractClass>]
type Type =
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

//type TypeSpec

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
    | TypeRef of ReferencedType
    | Assembly of AssemblyReference

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

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    | Null

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
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.ConcreteClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.ConcreteClass>

    static member AbstractClass:
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.AbstractClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.AbstractClass>

    static member SealedClass:
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.SealedClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.SealedClass>

    static member StaticClass:
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.StaticClass> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.StaticClass>

    static member Interface:
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.Interface> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.Interface>

    static member ValueType:
        TypeVisibility *
        flags: TypeAttributes<TypeKinds.ValueType> *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        ClassExtends -> TypeDefinition<TypeKinds.ValueType>

    //static member Delegate
    //static member Enum

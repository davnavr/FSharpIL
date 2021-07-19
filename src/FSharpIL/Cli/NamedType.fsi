namespace FSharpIL.Cli

open System
open System.Collections.Generic
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
    val (|IsSystemType|) : expected: string -> #NamedType -> bool
    // TODO: Add cases for unmanaged pointer types.
    //val inline (|DefinedType|ReferencedType|InstantiatedType|ArrayType|PrimitiveType|) : NamedType -> Choice<DefinedType, ReferencedType, InstantiatedType, ArrayType, PrimitiveType>

[<RequireQualifiedAccess>]
module NamedType =
    val toElemType: NamedType -> FSharpIL.Metadata.Blobs.ElemType voption

[<Struct>]
[<NoComparison; NoEquality>]
type GenericConstraintSetEnumerator =
    val mutable private inner: ImmutableHashSet<NamedType>.Enumerator

    member Current: NamedType
    member MoveNext: unit -> bool

    interface IEnumerator<NamedType>

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericConstraintSet =
    member Add: constr: NamedType -> GenericConstraintSet
    member Count: int32
    member Contains: constr: NamedType -> bool
    member GetEnumerator: unit -> GenericConstraintSetEnumerator
    member Equals: other: GenericConstraintSet -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericConstraintSet>
    interface IEnumerable<NamedType>
    interface IReadOnlyCollection<NamedType>

[<RequireQualifiedAccess>]
module GenericConstraintSet =
    val empty : GenericConstraintSet
    val inline add : constr: NamedType -> constraints: GenericConstraintSet -> GenericConstraintSet
    val inline contains : constr: NamedType -> constraints: GenericConstraintSet -> bool

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: GenericConstraintSet

    new: name: Identifier * flags: GenericParamFlags * constraints: GenericConstraintSet -> GenericParam

    member Equals: other: GenericParam -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<GenericParam>

[<IsReadOnly; Struct>]
type GenericParamKind =
    | Invariant
    | Covariant
    | Contravariant

[<IsReadOnly; Struct>]
type GenericSpecialConstraint =
    | NoSpecialConstriant
    | ReferenceTypeConstraint
    | NonNullableValueTypeConstraint

[<Sealed>]
type GenericParamType = class
    inherit NamedType

    member Sequence: uint16
    member Flags: GenericParamFlags
    member Constraints: GenericConstraintSet
    member Kind: GenericParamKind
    member SpecialConstraint: GenericSpecialConstraint
    member RequiresDefaultConstructor: bool

    override GetHashCode: unit -> int32

    interface IComparable<GenericParamType>
    interface IEquatable<GenericParamType>
end

[<RequireQualifiedAccess>]
module GenericParam =
    val named : Identifier -> GenericParam

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type GenericParamList =
    member Parameters: ImmutableArray<GenericParam>
    interface IEquatable<GenericParamList>

[<RequireQualifiedAccess>]
module GenericParamList =
    val empty : GenericParamList
    val singleton : parameter: GenericParam -> GenericParamList
    /// <summary>Creates a generic parameter list from the sequence of generic parameters.</summary>
    /// <exception cref="ArgumentException">The sequence of generic <paramref name="parameters"/> contains a duplicate.</exception>
    val ofSeq : parameters: seq<GenericParam> -> GenericParamList

[<AutoOpen>]
module GenericParamListPatterns =
    val inline (|GenericParamList|): GenericParamList -> ImmutableArray<GenericParam>

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ModifierType =
    member Required: bool
    member Modifier: NamedType

    new: required: bool * modifier: NamedType -> ModifierType

    interface IEquatable<ModifierType>

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

//type NativePointerType

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
    member GenericParameters: ImmutableArray<GenericParamType>

    member Definition: NamedType

[<Sealed>]
type GenericType<'Definition when 'Definition :> NamedType> = class
    inherit GenericType

    member Definition: 'Definition
end

type GenericParamType with
    /// Gets the type that owns this generic parameter.
    member Owner: GenericType

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type TypeReferenceParent =
    | Null
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

and ReferencedType =
    inherit NamedType

    //val Flags: TypeDefFlags voption

    member ResolutionScope: TypeReferenceParent

    new:
        resolutionScope: TypeReferenceParent *
        typeNamespace: Identifier voption *
        typeName: Identifier -> ReferencedType

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
    inherit NamedType

    val Flags: TypeDefFlags
    val Extends: ClassExtends

    new:
        flags: TypeDefFlags *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosing: DefinedType voption *
        typeName: Identifier -> DefinedType

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

[<Sealed>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> = class
    inherit DefinedType
end

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

type DefinedType with
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

[<Sealed>]
type TypeReference<'Kind when 'Kind :> IAttributeTag<TypeDefFlags>> = class
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

[<Struct>]
[<NoComparison; NoEquality>]
type InstantiatedTypeArgumentsEnumerator =
    val mutable private index: int32
    val private parameters: ImmutableArray<GenericParamType>
    val private instantiator: GenericParamType -> NamedType

    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown when the enumerator was not started or when the end of the parameter list has been reached.
    /// </exception>
    member Current: NamedType
    member MoveNext: unit -> bool

    interface System.Collections.Generic.IEnumerator<NamedType>

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type InstantiatedTypeArguments =
    member Count: int32
    member Item: index: int32 -> NamedType with get

    member Equals: InstantiatedTypeArguments -> bool
    member GetEnumerator: unit -> InstantiatedTypeArgumentsEnumerator

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IEquatable<InstantiatedTypeArguments>
    interface IEnumerable<NamedType>
    interface IReadOnlyCollection<NamedType>
    interface IReadOnlyList<NamedType>

[<AbstractClass>]
type GenericTypeInstantiation = class
    inherit NamedType

    member Instantiated: GenericType
    member Arguments: InstantiatedTypeArguments
end

[<Sealed>]
type InstantiatedType<'Inst when 'Inst :> GenericType and 'Inst : not struct> = class
    inherit GenericTypeInstantiation

    member Instantiated: 'Inst
end

type GenericType with
    static member Defined<'Definition
        when 'Definition :> DefinedType
        and 'Definition : not struct> :
        definition: 'Definition *
        genericParamList: ImmutableArray<GenericParamType> -> GenericType<'Definition>

[<RequireQualifiedAccess>]
module GenericType =
    val instantiate : gtype: 'Inst -> instantiator: (GenericParamType -> NamedType) -> InstantiatedType<'Inst>

    val inline (|Instantiation|) : gtype: InstantiatedType<'Inst> -> struct('Inst * InstantiatedTypeArguments)

[<RequireQualifiedAccess>]
module ClassExtends =
    val inline (|Null|Defined|Referenced|Generic|):
        ClassExtends ->
        Choice<unit, DefinedType, ReferencedType, GenericTypeInstantiation>

    val Null: ClassExtends
    val Defined: extends: DefinedType -> ClassExtends
    val Referenced: extends: ReferencedType -> ClassExtends
    val Generic: extends: InstantiatedType<#GenericType> -> ClassExtends

/// Describes the type of a local variable (II.23.2.6).
[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type LocalVariableType =
    val CustomModifiers: ImmutableArray<ModifierType>
    /// <summary>
    /// Gets a value indicating whether the value pointed to by this local variable can move during garbage collection (II.23.2.9).
    /// </summary>
    /// <returns>
    /// <see langword="true"/> if the value pointed to by this local variable cannot be moved during garbage collection;
    /// otherwise <see langword="false"/>.
    /// </returns>
    val IsPinned: bool
    val Tag: FSharpIL.Metadata.Signatures.LocalVariableTag
    val Type: NamedType voption

    internal new:
        modifiers: ImmutableArray<ModifierType> *
        pinned: bool *
        tag: FSharpIL.Metadata.Signatures.LocalVariableTag *
        NamedType voption -> LocalVariableType

    interface IEquatable<LocalVariableType>

[<RequireQualifiedAccess>]
module LocalVariableType =
    val Type : pinned: bool * localVarType: NamedType -> LocalVariableType
    val ByRef : modifiers: ImmutableArray<ModifierType> * pinned: bool * localVarType: NamedType -> LocalVariableType
    val TypedByRef : modifiers: ImmutableArray<ModifierType> -> LocalVariableType
    val TypedByRef' : LocalVariableType

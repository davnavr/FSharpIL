﻿module FSharpIL.Cli.TypeSystem

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities
open FSharpIL.Utilities.Compare
open FSharpIL.Utilities.Collections.CollectionFail

#nowarn "0060" // Override implementations in augmentations are now deprecated.

[<IsReadOnly; Struct>]
type PrimitiveType (encoded: EncodedType) =
    member _.Encoded = encoded

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

type [<Interface>] IGenericType<'This> = inherit IEquatable<'This>

type CliType =
    | Primitive of PrimitiveType
    | SZArray of CliType
    | Array of CliType * shape: FSharpIL.Metadata.Signatures.ArrayShape
    | Modified of modified: ImmutableArray<ModifierType> * CliType
    | Class of NamedType
    | ValueType of NamedType
    | DefinedTypeVar of GenericParamType<TypeDefinition>
    | ReferencedTypeVar of GenericParamType<TypeReference>

and [<Struct>] ClassExtends (extends: TypeTok voption) =
    member _.Extends = extends
    member _.IsNull = extends.IsNone

and TypeReferenceParent =
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

and [<NoComparison; CustomEquality>] TypeReference =
    { Flags: TypeDefFlags voption
      ResolutionScope: TypeReferenceParent
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    override this.GetHashCode() = HashCode.Combine(this.ResolutionScope, this.TypeNamespace, this.TypeName)

    interface IGenericType<TypeReference> with
        member this.Equals other =
            this.TypeName === other.TypeName &&
            Equatable.voption this.TypeNamespace other.TypeNamespace &&
            this.ResolutionScope = other.ResolutionScope

    override this.Equals obj =
        match obj with
        | :? TypeReference as other -> this === other
        | _ -> false

and [<AbstractClass; Sealed>] NamedTypeComparers private () =
    static member val CompareDefined = Unchecked.defaultof<DefinedType -> DefinedType -> bool> with get, set
    static member val CompareReferenced = Unchecked.defaultof<ReferencedType -> ReferencedType -> bool> with get, set

and [<NoComparison; CustomEquality>] ReferencedType =
    | Reference of TypeReference
    | Generic of GenericType<TypeReference>

    interface IEquatable<ReferencedType> with member this.Equals other = NamedTypeComparers.CompareReferenced this other

    override this.Equals obj =
        match obj with
        | :? ReferencedType as other -> this === other
        | _ -> false

and [<NoComparison; CustomEquality>] DefinedType =
    | Definition of TypeDefinition
    | Generic of GenericType<TypeDefinition>

    interface IEquatable<DefinedType> with member this.Equals other = NamedTypeComparers.CompareDefined this other

and NamedType =
    | DefinedType of DefinedType
    | ReferencedType of ReferencedType

and [<NoComparison; CustomEquality>] TypeDefinition =
    { Flags: TypeDefFlags
      Extends: ClassExtends
      EnclosingClass: DefinedType voption
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    member this.IsNested = this.EnclosingClass.IsSome

    override this.GetHashCode() = HashCode.Combine(this.EnclosingClass, this.TypeNamespace, this.TypeName)

    interface IGenericType<TypeDefinition> with
        member this.Equals other =
            this.TypeName === other.TypeName &&
            Equatable.voption this.TypeNamespace other.TypeNamespace &&
            Equatable.voption this.EnclosingClass other.EnclosingClass

    override this.Equals obj =
        match obj with
        | :? TypeDefinition as other -> this === other
        | _ -> false

and [<Struct>] TypeTok =
    | Named of NamedType
    | Specified of spec: CliType

and [<Struct>] ModifierType (required: bool, modifier: TypeTok) =
    member _.Required = required
    member _.Modifier = modifier

and [<Struct>] GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: ImmutableArray<CliType>

and [<Sealed>] GenericParamType<'Owner when 'Owner :> IGenericType<'Owner>>
    (
        owner: 'Owner,
        i: uint16,
        flags: GenericParamFlags,
        name: Identifier,
        constraints: ImmutableArray<CliType>
    )
    =
    member _.Sequence = i
    member _.Flags = flags
    member _.Name = name
    member _.Constraints = constraints
    member _.Owner = owner

    member _.Kind =
        match flags &&& GenericParamFlags.VarianceMask with
        | GenericParamFlags.Covariant -> GenericParamKind.Covariant
        | GenericParamFlags.Contravariant -> GenericParamKind.Contravariant
        | GenericParamFlags.None
        | _ -> GenericParamKind.Invariant

    member _.RequiresDefaultConstructor = Flags.set GenericParamFlags.DefaultConstructorConstraint flags

    member _.SpecialConstraint =
        if Flags.set GenericParamFlags.ReferenceTypeConstraint flags
        then ReferenceTypeConstraint
        elif Flags.set GenericParamFlags.NotNullableValueTypeConstraint flags
        then NonNullableValueTypeConstraint
        else NoSpecialConstriant

    interface IEquatable<GenericParamType<'Owner>> with member _.Equals other = owner === other.Owner && i = other.Sequence

    override this.Equals obj =
        match obj with
        | :? GenericParamType<'Owner> as other -> this === other
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(owner, i)

and [<Struct>] GenericParamListIndexer<'Owner when 'Owner :> IGenericType<'Owner>>
    (
        initializer: int32 -> GenericParamType<'Owner>,
        parameters: GenericParamType<'Owner>[]
    )
    =
    member _.Max = parameters.Length - 1

    member this.Item with get index =
        if index < 0 then
            argOutOfRange (nameof index) index "The index into the generic parameter list cannot be negative"
        if index > this.Max then
            argOutOfRange (nameof index) index "The index exceeded the index of the last generic parameter in the list"

        let existing = parameters.[index]
        if Object.ReferenceEquals(existing, null) then
            let initialized = initializer index
            parameters.[index] <- initialized
            initialized
        else existing

and [<Struct>] GenericParamListEnumerator<'Owner when 'Owner :> IGenericType<'Owner>> =
    val mutable private i: int32
    val private indexer: GenericParamListIndexer<'Owner>

    new (indexer) = { i = -1; indexer = indexer }

    member this.Current =
        if this.i < 0 then enumeratorNotStarted()
        if this.i > this.indexer.Max then enumeratorReachedEnd()
        this.indexer.[this.i]

    member this.MoveNext() =
        if this.i <= this.indexer.Max then this.i <- this.i + 1
        this.i <= this.indexer.Max

    interface IEnumerator with
        member this.Current = this.Current :> obj
        member this.MoveNext() = this.MoveNext()
        member this.Reset() = this.i <- -1

    interface IDisposable with member _.Dispose() = ()

    interface IEnumerator<GenericParamType<'Owner>> with member this.Current = this.Current

and [<Struct; NoComparison; CustomEquality>] GenericParamList<'Owner when 'Owner :> IGenericType<'Owner>> private
    (
        initializer: int32 -> GenericParamType<'Owner>,
        parameters: GenericParamType<'Owner>[]
    )
    =
    member _.Indexer = GenericParamListIndexer(initializer, parameters)
    member _.Count = parameters.Length
    member this.Item with get index = this.Indexer.[index]
    member this.GetEnumerator() = new GenericParamListEnumerator<'Owner>(this.Indexer)

    new (initializer, count) = GenericParamList(initializer, Array.zeroCreate count)

    override this.GetHashCode() =
        let mutable hcode = HashCode()
        for parameter in this do hcode.Add parameter
        hcode.ToHashCode()

    interface IReadOnlyList<GenericParamType<'Owner>> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.Count = this.Count
        member this.Item with get index = this.[index]

    interface IEquatable<GenericParamList<'Owner>> with
        member this.Equals other = this.Count = other.Count && Equatable.sequences this other

    override this.Equals obj =
        match obj with
        | :? GenericParamList<'Owner> as other -> this === other
        | _ -> false

and [<Sealed>] GenericType<'Type when 'Type : not struct and 'Type :> IGenericType<'Type>>
    (
        tdef: 'Type,
        parameters: ImmutableArray<GenericParam>
    )
    =
    let initializer i =
        let parameter = parameters.[i]
        GenericParamType<'Type>(tdef, uint16 i, parameter.Flags, parameter.Name, parameter.Constraints)
    member _.Type = tdef
    member val Parameters = GenericParamList<'Type>(initializer, parameters.Length)

module PrimitiveType =
    let Boolean = CliType.Primitive(PrimitiveType EncodedType.Boolean)
    let Char = CliType.Primitive(PrimitiveType EncodedType.Char)
    let I1 = CliType.Primitive(PrimitiveType EncodedType.I1)
    let U1 = CliType.Primitive(PrimitiveType EncodedType.U1)
    let I2 = CliType.Primitive(PrimitiveType EncodedType.I2)
    let U2 = CliType.Primitive(PrimitiveType EncodedType.U2)
    let I4 = CliType.Primitive(PrimitiveType EncodedType.I4)
    let U4 = CliType.Primitive(PrimitiveType EncodedType.U4)
    let I8 = CliType.Primitive(PrimitiveType EncodedType.I8)
    let U8 = CliType.Primitive(PrimitiveType EncodedType.U8)
    let R4 = CliType.Primitive(PrimitiveType EncodedType.R4)
    let R8 = CliType.Primitive(PrimitiveType EncodedType.R8)
    let I = CliType.Primitive(PrimitiveType EncodedType.I)
    let U = CliType.Primitive(PrimitiveType EncodedType.U)
    let Object = CliType.Primitive(PrimitiveType EncodedType.Object)
    let String = CliType.Primitive(PrimitiveType EncodedType.String)

type GenericParam with
    new (name, flags, constraints) = { Name = name; Flags = flags; Constraints = constraints }

    new (name, kind, special, requiresDefaultConstructor, constraints) =
        let mutable flags =
            match kind with
            | GenericParamKind.Invariant -> GenericParamFlags.None
            | GenericParamKind.Covariant -> GenericParamFlags.Covariant
            | GenericParamKind.Contravariant -> GenericParamFlags.Contravariant

        match special with
        | GenericSpecialConstraint.NoSpecialConstriant -> ()
        | GenericSpecialConstraint.ReferenceTypeConstraint ->
            flags <- flags ||| GenericParamFlags.ReferenceTypeConstraint
        | GenericSpecialConstraint.NonNullableValueTypeConstraint ->
            flags <- flags ||| GenericParamFlags.NotNullableValueTypeConstraint

        if requiresDefaultConstructor then flags <- flags ||| GenericParamFlags.DefaultConstructorConstraint

        { Name = name; Flags = flags; Constraints = constraints }

let inline (|GenericType|) (gtype: GenericType<'Type>) = struct(gtype.Type, gtype.Parameters)

type ReferencedType with
    override this.GetHashCode() =
        match this with
        | ReferencedType.Reference tref
        | ReferencedType.Generic(GenericType(tref, _)) -> tref.GetHashCode()

do NamedTypeComparers.CompareDefined <- fun x y ->
    match x, y with
    | DefinedType.Definition x', DefinedType.Definition y'
    | DefinedType.Generic(GenericType(x', _)), DefinedType.Definition y'
    | DefinedType.Definition x', DefinedType.Generic(GenericType(y', _))
    | DefinedType.Generic(GenericType(x', _)), DefinedType.Generic(GenericType(y', _))-> x' === y'

do NamedTypeComparers.CompareReferenced <- fun x y ->
    match x, y with
    | ReferencedType.Reference x', ReferencedType.Reference y'
    | ReferencedType.Generic(GenericType(x', _)), ReferencedType.Reference y'
    | ReferencedType.Reference x', ReferencedType.Generic(GenericType(y', _))
    | ReferencedType.Generic(GenericType(x', _)), ReferencedType.Generic(GenericType(y', _))-> x' === y'

type NamedType with
    member this.TypeNamespace =
        match this with
        | DefinedType(DefinedType.Definition { TypeNamespace = ns })
        | DefinedType(DefinedType.Generic(GenericType({ TypeNamespace = ns }, _)))
        | ReferencedType(ReferencedType.Reference { TypeNamespace = ns })
        | ReferencedType(ReferencedType.Generic (GenericType({ TypeNamespace = ns }, _))) -> ns

    member this.TypeName =
        match this with
        | DefinedType(DefinedType.Definition { TypeName = name })
        | DefinedType(DefinedType.Generic(GenericType({ TypeName = name }, _)))
        | ReferencedType(ReferencedType.Reference { TypeName = name })
        | ReferencedType(ReferencedType.Generic(GenericType({ TypeName = name }, _))) -> name

    member this.EnclosingType =
        match this with
        | DefinedType(DefinedType.Definition { EnclosingClass = ValueSome parent })
        | DefinedType(DefinedType.Generic(GenericType({ EnclosingClass = ValueSome parent }, _))) ->
            ValueSome(DefinedType parent)
        | ReferencedType(ReferencedType.Reference { ResolutionScope = TypeReferenceParent.Type parent })
        | ReferencedType(ReferencedType.Generic(GenericType({ ResolutionScope = TypeReferenceParent.Type parent }, _))) ->
            ValueSome(ReferencedType parent)
        | _ -> ValueNone

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends ValueNone
    let Named (extends: NamedType) = ClassExtends(ValueSome(TypeTok.Named extends))

[<RequireQualifiedAccess>]
module TypeKinds =
    type IHasConstructor = interface end

    type ConcreteClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>
    end

    type AbstractClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract
    end

    type SealedClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

    type StaticClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract ||| TypeDefFlags.Sealed
    end

    type Delegate = struct
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

    type Enum = struct
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract
    end

    type Interface = struct
        interface TypeAttributes.IHasStaticMethods
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = TypeDefFlags.Abstract ||| TypeDefFlags.Interface
    end

    type ValueType = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IHasConstructor
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type TypeVisibility private (flag: TypeDefFlags, parent: DefinedType) =
    member _.Flag = flag
    member _.EnclosingClass = if Object.ReferenceEquals(parent, null) then ValueNone else ValueSome parent

    new (flag, parent) =
        TypeVisibility(flag, match parent with | ValueSome parent' -> parent' | ValueNone -> Unchecked.defaultof<_>)

[<RequireQualifiedAccess>]
module TypeVisibility =
    let NotPublic = TypeVisibility(TypeDefFlags.NotPublic, ValueNone)
    let Public = TypeVisibility(TypeDefFlags.Public, ValueNone)
    let NestedPublic parent = TypeVisibility(TypeDefFlags.NestedPublic, ValueSome parent)
    let NestedPrivate parent = TypeVisibility(TypeDefFlags.NestedPrivate, ValueSome parent)
    let NestedFamily parent = TypeVisibility(TypeDefFlags.NestedFamily, ValueSome parent)
    let NestedAssembly parent = TypeVisibility(TypeDefFlags.NestedAssembly, ValueSome parent)
    let NestedFamilyAndAssembly parent = TypeVisibility(TypeDefFlags.NestedFamAndAssem, ValueSome parent)
    let NestedFamilyOrAssembly parent = TypeVisibility(TypeDefFlags.NestedFamOrAssem, ValueSome parent)

type TypeDefinition with
    member this.Visibility = TypeVisibility(this.Flags &&& TypeDefFlags.VisibilityMask, this.EnclosingClass)

[<Struct>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct>
    (
        visibility: TypeVisibility,
        flags: TypeAttributes<'Kind>,
        extends: ClassExtends,
        typeNamespace: Identifier voption,
        enclosingClass: DefinedType voption,
        typeName: Identifier
    )
    =
    member _.Definition =
        { TypeDefinition.TypeName = typeName
          TypeNamespace = typeNamespace
          Flags = visibility.Flag ||| flags.Flags ||| Unchecked.defaultof<'Kind>.RequiredFlags
          Extends = extends
          EnclosingClass = enclosingClass }

let inline (|TypeDefinition|) (definition: TypeDefinition<'Kind>) = definition.Definition

type TypeDefinition with
    static member ConcreteClass(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.ConcreteClass>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

    static member AbstractClass(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.AbstractClass>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

    static member SealedClass(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.SealedClass>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

    static member StaticClass(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.StaticClass>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

    static member Interface(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.Interface>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

    static member ValueType(visibility, flags, extends, typeNamespace, enclosingClass, typeName) =
        TypeDefinition<TypeKinds.ValueType>(visibility, flags, extends, typeNamespace, enclosingClass, typeName)

[<Struct>]
type TypeReference<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct>
    (
        parent: TypeReferenceParent,
        typeNamespace: Identifier voption,
        typeName: Identifier
    )
    =
    member _.Reference =
        { TypeReference.TypeName = typeName
          TypeNamespace = typeNamespace
          Flags = ValueSome Unchecked.defaultof<'Kind>.RequiredFlags
          ResolutionScope = parent }

let inline (|TypeReference|) (reference: TypeReference<'Kind>) = reference.Reference

type TypeReference with
    static member ConcreteClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.ConcreteClass>(resolutionScope, typeNamespace, typeName)




    static member SealedClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.SealedClass>(resolutionScope, typeNamespace, typeName)

    static member StaticClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.StaticClass>(resolutionScope, typeNamespace, typeName)

module CliType =
    let rec toElemType =
        function
        | CliType.Primitive prim -> EncodedType.toElemType prim.Encoded
        | CliType.SZArray e -> toElemType e
        | _ -> ValueNone

[<AbstractClass; Sealed>]
type GenericType =
    static member Defined (definition: TypeDefinition) parameters = GenericType(definition, parameters)
    static member Referenced (reference: TypeReference) parameters = GenericType(reference, parameters)

[<RequireQualifiedAccess>]
type internal NamedTypeCache =
    private
        { defined: Dictionary<DefinedType, NamedType>
          referenced: Dictionary<ReferencedType, NamedType> }

[<RequireQualifiedAccess>]
type LocalType =
    | T of modifiers: ImmutableArray<ModifierType> * pinned: bool * CliType
    | ByRef of modifiers: ImmutableArray<ModifierType> * pinned: bool * CliType
    | TypedByRef of modifiers: ImmutableArray<ModifierType>

    member this.CustomModifiers =
        match this with
        | T(modifiers, _, _)
        | ByRef(modifiers, _, _)
        | TypedByRef modifiers -> modifiers

    member this.IsPinned =
        match this with
        | T(_, pinned, _)
        | ByRef(_, pinned, _) -> pinned
        | TypedByRef _ -> false

module LocalType =
    let TypedByRef' = LocalType.TypedByRef ImmutableArray.Empty

module ModuleType =
    let Definition =
        { TypeDefinition.Flags = Unchecked.defaultof<TypeDefFlags>
          Extends = ClassExtends.Null // According to ECMA-335, the module class "does not have a base type" (II.10.8).
          EnclosingClass = ValueNone
          TypeNamespace = ValueNone
          TypeName = Identifier.ofStr "<Module>" }

    let Definition' = DefinedType.Definition Definition

    let Named = NamedType.DefinedType Definition'

module NamedTypeCache =
    let empty definedTypeCapacity referencedTypeCapacity =
        { NamedTypeCache.defined = Dictionary(capacity = definedTypeCapacity)
          NamedTypeCache.referenced = Dictionary(capacity = referencedTypeCapacity) }

    let addDefined defined { NamedTypeCache.defined = cache } =
        if Object.ReferenceEquals(defined, ModuleType.Definition')
        then ModuleType.Named
        else
            match cache.TryGetValue defined with
            | true, existing -> existing
            | false, _ ->
                let named = NamedType.DefinedType defined
                cache.[defined] <- named
                named

    let addReferenced referenced { NamedTypeCache.referenced = cache } =
        match cache.TryGetValue referenced with
        | true, existing -> existing
        | false, _ ->
            let named = NamedType.ReferencedType referenced
            cache.[referenced] <- named
            named

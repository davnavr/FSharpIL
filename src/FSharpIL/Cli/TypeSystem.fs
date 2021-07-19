module FSharpIL.Cli.TypeSystem

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

[<IsReadOnly; Struct>]
type PrimitiveType (encoded: EncodedType) =
    member _.Encoded = encoded

module PrimitiveType =
    let Boolean = PrimitiveType EncodedType.Boolean
    let Char = PrimitiveType EncodedType.Char
    let I1 = PrimitiveType EncodedType.I1
    let U1 = PrimitiveType EncodedType.U1
    let I2 = PrimitiveType EncodedType.I2
    let U2 = PrimitiveType EncodedType.U2
    let I4 = PrimitiveType EncodedType.I4
    let U4 = PrimitiveType EncodedType.U4
    let I8 = PrimitiveType EncodedType.I8
    let U8 = PrimitiveType EncodedType.U8
    let R4 = PrimitiveType EncodedType.R4
    let R8 = PrimitiveType EncodedType.R8
    let I = PrimitiveType EncodedType.I
    let U = PrimitiveType EncodedType.U
    let Object = PrimitiveType EncodedType.Object
    let String = PrimitiveType EncodedType.String

type TypeReferenceParent =
    | Null
    | Type of ReferencedType
    | Assembly of ReferencedAssembly
    //| Module of ModuleReference

and ReferencedType =
    { Flags: TypeDefFlags voption
      ResolutionScope: TypeReferenceParent
      TypeNamespace: Identifier voption
      TypeName: Identifier }

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

type CliType =
    | Primitive of PrimitiveType
    | SZArray of CliType
    | Array of shape: ArrayShape * etype: CliType
    | Modified of modified: ImmutableArray<ModifierType> * CliType
    | Class of NamedType
    | ValueType of NamedType
    | Var of GenericParamType

and [<Struct>] ClassExtends (extends: CliType voption) =
    member _.Extends = extends
    member _.IsNull = extends.IsNone

and NamedType =
    | DefinedType of DefinedType
    | ReferencedType of ReferencedType

    member this.TypeNamespace =
        match this with
        | DefinedType { TypeNamespace = ns }
        | ReferencedType { TypeNamespace = ns } -> ns

    member this.TypeName =
        match this with
        | DefinedType { TypeName = name }
        | ReferencedType { TypeName = name } -> name

    member this.EnclosingType =
        match this with
        | DefinedType { EnclosingClass = ValueSome parent } -> ValueSome(DefinedType parent)
        | ReferencedType { ResolutionScope = TypeReferenceParent.Type parent } -> ValueSome(ReferencedType parent)
        | ReferencedType { ResolutionScope = TypeReferenceParent.Null | TypeReferenceParent.Assembly _ }
        | DefinedType { EnclosingClass = ValueNone } -> ValueNone

and DefinedType =
    { Flags: TypeDefFlags
      Extends: ClassExtends
      EnclosingClass: DefinedType voption
      TypeNamespace: Identifier voption
      TypeName: Identifier }

    member this.IsNested = this.EnclosingClass.IsSome

and [<Struct>] ModifierType (required: bool, modifier: CliType) =
    member _.Required = required
    member _.Modifier = modifier

and [<Struct; NoComparison; CustomEquality>] GenericParamOwner (a: obj) = // TODO: Move this to a different file.
    interface IEquatable<GenericParamOwner> with
        member _.Equals other = failwith "bad"

and [<Sealed>] GenericParamType (owner: GenericParamOwner, i, flags, name, constraints) =
    member _.Sequence: uint16 = i
    member _.Flags: GenericParamFlags = flags
    member _.Name: Identifier = name
    member _.Constraints: ImmutableArray<CliType> = constraints
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

    interface IEquatable<GenericParamType> with member _.Equals other = owner === other.Owner && i = other.Sequence

    override this.Equals obj =
        match obj with
        | :? GenericParamType as other -> this === other
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(owner, i)

and [<Struct>] GenericParamListIndexer (initializer: int32 -> GenericParamType, parameters: GenericParamType[]) =
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

and [<Struct>] GenericParamListEnumerator =
    val mutable private i: int32
    val private indexer: GenericParamListIndexer

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

    interface IEnumerator<GenericParamType> with member this.Current = this.Current

and [<Struct; NoComparison; CustomEquality>] GenericParamList private
    (
        initializer: int32 -> GenericParamType,
        parameters: GenericParamType[]
    )
    =
    member _.Indexer = GenericParamListIndexer(initializer, parameters)
    member _.Count = parameters.Length
    member this.Item with get index = this.Indexer.[index]
    member this.GetEnumerator() = new GenericParamListEnumerator(this.Indexer)

    new (initializer, count) = GenericParamList(initializer, Array.zeroCreate count)

    interface IReadOnlyList<GenericParamType> with
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.Count = this.Count
        member this.Item with get index = this.[index]

    interface IEquatable<GenericParamList> with
        member this.Equals other = this.Count = other.Count && Equatable.sequences this other

[<Struct>]
type GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: ImmutableArray<CliType>

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

[<RequireQualifiedAccess>]
module ClassExtends =
    let Null = ClassExtends ValueNone
    let Class (extends: NamedType) = ClassExtends(ValueSome(CliType.Class extends))

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

type DefinedType with
    member this.Visibility = TypeVisibility(this.Flags &&& TypeDefFlags.VisibilityMask, this.EnclosingClass)

module GenericParamList =
    let createGenericParam owner (parameter: GenericParam) i =
        GenericParamType(owner, uint16 i, parameter.Flags, parameter.Name, parameter.Constraints)

    let singleton owner parameter = GenericParamList(createGenericParam owner parameter, 1)

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
        { DefinedType.TypeName = typeName
          TypeNamespace = typeNamespace
          Flags = visibility.Flag ||| flags.Flags ||| Unchecked.defaultof<'Kind>.RequiredFlags
          Extends = extends
          EnclosingClass = enclosingClass }

let inline (|TypeDefinition|) (definition: TypeDefinition<'Kind>) = definition.Definition

[<AbstractClass; Sealed>]
type TypeDefinition =
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
        { ReferencedType.TypeName = typeName
          TypeNamespace = typeNamespace
          Flags = ValueSome Unchecked.defaultof<'Kind>.RequiredFlags
          ResolutionScope = parent }

let inline (|TypeReference|) (reference: TypeReference<'Kind>) = reference.Reference

[<AbstractClass; Sealed>]
type TypeReference =
    static member ConcreteClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.ConcreteClass>(resolutionScope, typeNamespace, typeName)




    static member SealedClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.SealedClass>(resolutionScope, typeNamespace, typeName)

    static member StaticClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.StaticClass>(resolutionScope, typeNamespace, typeName)

[<AutoOpen>]
module ModuleType =
    let ModuleType =
        { DefinedType.Flags = Unchecked.defaultof<TypeDefFlags>
          Extends = ClassExtends.Null // According to ECMA-335, the module class "does not have a base type" (II.10.8).
          EnclosingClass = ValueNone
          TypeNamespace = ValueNone
          TypeName = Identifier.ofStr "<Module>" }

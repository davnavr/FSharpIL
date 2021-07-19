namespace rec FSharpIL.Cli

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections
open FSharpIL.Utilities.Compare

[<AbstractClass>]
type GenericTypeInstantiation (instantiator: GenericParamType -> NamedType, inst: GenericType) = class
    inherit NamedType(inst.TypeNamespace, inst.EnclosingType, inst.TypeName)

    member _.Instantiated = inst
    member val Arguments = InstantiatedTypeArguments(inst.GenericParameters, instantiator)
end

[<Sealed>]
type InstantiatedType<'Inst when 'Inst :> GenericType and 'Inst : not struct>
    (
        instantiator: GenericParamType -> NamedType,
        inst: 'Inst
    )
    =
    inherit GenericTypeInstantiation(instantiator, inst)

    member _.Instantiated = Unsafe.As<'Inst> base.Instantiated

[<AbstractClass>]
type NamedType (ns: Identifier voption, parent: NamedType voption, tname: Identifier) =
    member _.TypeName = tname
    member _.TypeNamespace = ns
    member _.EnclosingType = parent
    member _.IsNested = parent.IsSome

    member private this.CaseEquals(other: NamedType) =
        match this, other with
        | (:? ModifiedType as x), (:? ModifiedType as y) -> x.Modified === y.Modified && Equatable.blocks x.Modifiers y.Modifiers
        | (:? PrimitiveType as x), (:? PrimitiveType as y) -> x.Encoded === y.Encoded
        | (:? ArrayType), (:? SZArrayType)
        | (:? SZArrayType), (:? ArrayType) -> false
        | (:? ArrayType as x), (:? ArrayType as y) -> x.ElementType === y.ElementType
        | (:? PrimitiveType), _
        | _, (:? PrimitiveType)
        | (:? DefinedType), (:? DefinedType)
        | (:? ReferencedType), (:? ReferencedType) -> true
        | (:? GenericType as x), (:? GenericType as y) -> x.Definition === y.Definition
        | (:? GenericTypeInstantiation as x), (:? GenericTypeInstantiation as y) ->
            x.Instantiated.Equals(other = y.Instantiated) && x.Arguments === y.Arguments
        | (:? GenericParamType as x), (:? GenericParamType as y) -> x.Owner = y.Owner && x.Sequence = y.Sequence
        | _ -> false

    member this.Equals(other: NamedType) =
        Equatable.voption this.TypeNamespace other.TypeNamespace &&
        this.TypeName === other.TypeName &&
        Equatable.voption this.EnclosingType other.EnclosingType &&
        this.CaseEquals other

    override this.Equals obj =
        match obj with
        | :? NamedType as other -> this.Equals(other = other)
        | _ -> false

    override this.GetHashCode() =
        let c = HashCode.Combine(ns, parent, tname)

        match this with
        | :? ReferencedType as tref -> HashCode.Combine(c, tref.ResolutionScope)
        | :? GenericType as tdef -> HashCode.Combine(c, tdef.GenericParameters)
        | _ -> c

    interface IEquatable<NamedType> with member this.Equals other = this.Equals(other = other)

[<AutoOpen>]
module NamedTypePatterns =
    let inline (|NamedType|) (t: #NamedType) = t :> NamedType

    let (|IsSystemType|) expected (t: #NamedType) =
        t.TypeNamespace = PrimitiveType.ns && t.EnclosingType.IsNone && t.TypeName = Identifier expected

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericParam =
    val Name: Identifier
    val Flags: GenericParamFlags
    val Constraints: GenericConstraintSet

    new (name, flags, constraints) = { Name = name; Flags = flags; Constraints = constraints }

    member this.Equals(other: GenericParam) = this.Name === other.Name

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(obj: obj) =
        match obj with
        | :? GenericParam as other -> this.Equals(other = other)
        | _ -> false

    interface IEquatable<GenericParam> with member this.Equals other = this.Equals(other = other)

[<Struct>]
type GenericConstraintSetEnumerator =
    val mutable inner: ImmutableHashSet<NamedType>.Enumerator

    new (inner) = { inner = inner }

    member this.Current = this.inner.Current
    member this.MoveNext() = this.inner.MoveNext()

    interface IEnumerator<NamedType> with
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.MoveNext() = this.MoveNext()
        member this.Reset() = this.inner.Reset()
        member _.Dispose() = ()

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericConstraintSet (inner: ImmutableHashSet<NamedType>) =
    member _.Count = inner.Count
    member _.GetEnumerator() = new GenericConstraintSetEnumerator(inner.GetEnumerator())
    member _.Add constr = GenericConstraintSet(inner.Add constr)
    member _.Contains constr = inner.Contains constr
    member this.Equals(other: GenericConstraintSet) =
        if this.Count = other.Count
        then Equatable.sequences this other
        else false

    override this.GetHashCode() =
        let mutable hcode = HashCode()
        for constr in this do hcode.Add constr
        hcode.ToHashCode()

    override this.Equals obj =
        match obj with
        | :? GenericConstraintSet as other -> this.Equals(other = other)
        | _ -> false

    interface IReadOnlyCollection<NamedType> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

    interface IEquatable<GenericConstraintSet> with member this.Equals other = this.Equals(other = other)

[<RequireQualifiedAccess>]
module GenericConstraintSet =
    let empty = GenericConstraintSet ImmutableHashSet.Empty
    let inline add constr (constraints: GenericConstraintSet) = constraints.Add constr
    let inline contains constr (constraints: GenericConstraintSet) = constraints.Contains constr

[<RequireQualifiedAccess>]
module GenericParam =
    let named name = GenericParam(name, GenericParamFlags.None, GenericConstraintSet.empty)

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type GenericParamList (parameters: ImmutableArray<GenericParam>) =
    member _.Parameters = parameters

[<RequireQualifiedAccess>]
module GenericParamList =
    let empty = GenericParamList ImmutableArray.Empty
    let singleton parameter = GenericParamList(ImmutableArray.Create<GenericParam>(item = parameter))
    let ofSeq parameters = GenericParamList(ImmutableArray.CreateRange parameters)

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

// TODO: How to deal with duplicate generic parameter names for nested classes?

[<Sealed>]
type GenericParamType (owner: GenericType, name, flags: GenericParamFlags, i: uint16, constraints: GenericConstraintSet) =
    inherit NamedType(ValueNone, ValueSome(owner :> NamedType), name)

    member _.Sequence = i
    member _.Flags = flags
    member _.Constraints = constraints
    member _.Owner = owner

    member _.RequiresDefaultConstructor = Flags.set GenericParamFlags.DefaultConstructorConstraint flags

    member _.Kind =
        match flags &&& GenericParamFlags.VarianceMask with
        | GenericParamFlags.Covariant -> Covariant
        | GenericParamFlags.Contravariant -> Contravariant
        | GenericParamFlags.None
        | _ -> Invariant

    member _.SpecialConstraint =
        if Flags.set GenericParamFlags.ReferenceTypeConstraint flags
        then ReferenceTypeConstraint
        elif Flags.set GenericParamFlags.NotNullableValueTypeConstraint flags
        then NonNullableValueTypeConstraint
        else NoSpecialConstriant

    override _.Equals obj = base.Equals obj
    override _.GetHashCode() = HashCode.Combine(owner, i)

    interface IComparable<GenericParamType> with member this.CompareTo other = compare this.Sequence other.Sequence
    interface IEquatable<GenericParamType> with
        member this.Equals other = this.Owner === (other.Owner :> NamedType) && this.Sequence = other.Sequence

[<AutoOpen>]
module GenericParamListPatterns =
    let inline (|GenericParamList|) (parameters: GenericParamList) = parameters.Parameters

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ModifierType (required: bool, modifier: NamedType) =
    member _.Required = required
    member _.Modifier = modifier

    interface IEquatable<ModifierType> with
        member this.Equals other = this.Required = other.Required && this.Modifier === other.Modifier

[<RequireQualifiedAccess>]
module ModifierType =
    let inline Req modifier = ModifierType(true, modifier)
    let inline Opt modifier = ModifierType(false, modifier)

/// Represents a type that is modified by custom modifiers (II.7.1.1).
[<Sealed>]
type ModifiedType =
    inherit NamedType

    val Modifiers: ImmutableArray<ModifierType>
    val Modified: NamedType

    new (modifiers, modified: NamedType) =
        { inherit NamedType(modified.TypeNamespace, modified.EnclosingType, modified.TypeName)
          Modifiers = modifiers
          Modified = modified }

[<Sealed>]
type PrimitiveType (encoded: EncodedType) =
    inherit NamedType(PrimitiveType.ns, ValueNone, Identifier.ofStr(encoded.GetType().Name))
    member _.Encoded = encoded

[<RequireQualifiedAccess>]
module PrimitiveType =
    /// <summary>The <c>System</c> namespace, which contains primitive types.</summary>
    let ns = ValueSome(Identifier.ofStr "System")
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

type ArrayType =
    inherit NamedType

    val ElementType: NamedType
    val Shape: ArrayShape

    new (element: NamedType, shape, name) =
        { inherit NamedType(ValueNone, ValueNone, name)
          ElementType = element
          Shape = shape }

    new (element: NamedType, shape) = ArrayType(element, shape, element.TypeName + "[" + System.String(',', int32(shape.Rank - 1u)) + "]")

[<Sealed>]
type SZArrayType (element: NamedType) =
    inherit ArrayType(element, ArrayShape.ofVector, element.TypeName + "[]")

[<AbstractClass>]
type GenericType (definition: NamedType, genericParameterList) =
    inherit NamedType(definition.TypeNamespace, definition.EnclosingType, definition.TypeName)

    member _.GenericParameters: ImmutableArray<GenericParamType> = genericParameterList
    member _.Definition = definition

[<Sealed>]
type GenericType<'Definition when 'Definition :> NamedType and 'Definition : not struct>
    (
        definition: 'Definition,
        genericParameterList: ImmutableArray<GenericParamType>
    )
    =
    inherit GenericType(definition, genericParameterList)

    member _.Definition = Unsafe.As<'Definition> base.Definition

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type TypeReferenceParent =
    | Null
    | Type of ReferencedType
    | Assembly of ReferencedAssembly

[<RequireQualifiedAccess>]
module TypeReferenceParent =
    let toType parent =
        match parent with
        | TypeReferenceParent.Null
        | TypeReferenceParent.Assembly _ -> ValueNone
        | TypeReferenceParent.Type(NamedType t) -> ValueSome t

type ReferencedType (resolutionScope, typeNamespace, typeName) =
    inherit NamedType(typeNamespace, TypeReferenceParent.toType resolutionScope, typeName)

    member _.ResolutionScope = resolutionScope

[<IsReadOnly; Struct>]
type ClassExtends (extends: NamedType voption) =
    member _.Extends = extends
    member this.IsNull = this.Extends.IsNone

[<IsReadOnly>]
type TypeVisibility = struct
    val Flag: TypeDefFlags
    val internal Parent: DefinedType

    internal new (flag, parent) =
        { Flag = flag
          Parent =
            match parent with
            | ValueSome parent' -> parent'
            | ValueNone -> Unchecked.defaultof<_> }

    member this.IsNested = this.Parent <> Unchecked.defaultof<_>
    member this.EnclosingClass = if this.Parent = Unchecked.defaultof<_> then ValueNone else ValueSome this.Parent
end

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

type DefinedType =
    inherit NamedType

    val Flags: TypeDefFlags
    val Extends: ClassExtends

    new (flags, extends, typeNamespace, enclosing, typeName) =
        { inherit NamedType(typeNamespace, Convert.unsafeValueOption<DefinedType, _> enclosing, typeName)
          Flags = flags
          Extends = extends }

    member this.EnclosingClass = Convert.unsafeValueOption<_, DefinedType> this.EnclosingType

    member this.Visibility = TypeVisibility(this.Flags &&& TypeDefFlags.VisibilityMask, this.EnclosingClass)

[<RequireQualifiedAccess>]
module DefinedType =
    let inline (|IsInterface|NotInterface|) (tdef: DefinedType) =
        if tdef.Flags &&& TypeDefFlags.Interface = TypeDefFlags.Interface
        then IsInterface
        else NotInterface

[<Struct>]
type InstantiatedTypeArgumentsEnumerator =
    val mutable index: int32
    val parameters: ImmutableArray<GenericParamType>
    val instantiator: GenericParamType -> NamedType

    new (parameters, instantiator) =
        { index = -1
          parameters = parameters
          instantiator = instantiator }

    member this.Current =
        if this.index < 0 then enumeratorNotStarted()
        elif this.index >= this.parameters.Length then enumeratorReachedEnd()
        else this.instantiator this.parameters.[this.index]

    member this.MoveNext() =
        if this.index < this.parameters.Length then
            this.index <- this.index + 1
        this.index < this.parameters.Length

    interface IEnumerator<NamedType> with
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.Reset() = this.index <- -1
        member this.MoveNext() = this.MoveNext()
        member _.Dispose() = ()

[<IsReadOnly>]
[<NoComparison; CustomEquality>]
type InstantiatedTypeArguments (gparams: ImmutableArray<GenericParamType>, instantiator: GenericParamType -> NamedType) = struct
    member _.Count = gparams.Length

    member _.Item with get index = instantiator gparams.[index]

    member _.GetEnumerator() = new InstantiatedTypeArgumentsEnumerator(gparams, instantiator)

    member this.Equals(other: InstantiatedTypeArguments) =
        let mutable equal, i = this.Count = other.Count, 0
        while not equal && i < this.Count do
            equal <- this.[i].Equals other.[i]
        equal

    override _.GetHashCode() = gparams.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? InstantiatedTypeArguments as other -> this.Equals(other = other)
        | _ -> false

    interface IEquatable<InstantiatedTypeArguments> with member this.Equals other = this.Equals(other = other)

    interface IReadOnlyList<NamedType> with
        member this.Count = this.Count
        member this.Item with get i = this.[i]
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
end

[<RequireQualifiedAccess>]
module GenericType =
    let instantiate gtype instantiator = InstantiatedType(instantiator, gtype)

    let inline (|Instantiation|) (gtype: InstantiatedType<_>) = struct(gtype.Instantiated, gtype.Arguments)

[<RequireQualifiedAccess>]
module ClassExtends =
    let inline (|Null|Defined|Referenced|Generic|) (extends: ClassExtends) =
        match extends.Extends with
        | ValueNone -> Null
        | ValueSome(:? ReferencedType as tref) -> Referenced tref
        | ValueSome(:? DefinedType as tdef) -> Defined tdef
        | ValueSome inst -> Generic(inst :?> GenericTypeInstantiation)

    let Null = ClassExtends ValueNone
    let Defined (NamedType extends: DefinedType) = ClassExtends(ValueSome extends)
    let Referenced (NamedType extends: ReferencedType) = ClassExtends(ValueSome extends)
    let Generic (NamedType extends: InstantiatedType<#GenericType>) = ClassExtends(ValueSome extends)

[<AutoOpen>]
module ModuleType =
    let ModuleType =
        DefinedType (
            Unchecked.defaultof<TypeDefFlags>,
            ClassExtends.Null, // According to ECMA-335, the module class "does not have a base type" (II.10.8).
            ValueNone,
            ValueNone,
            Identifier.ofStr "<Module>"
        )

[<RequireQualifiedAccess>]
module NamedType =
    let rec toElemType (t: NamedType) =
        match t with
        | :? PrimitiveType as prim -> EncodedType.toElemType prim.Encoded
        | :? SZArrayType as arr -> toElemType arr.ElementType
        // TODO: How to convert Enum types?
        | _ -> ValueNone

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

[<Sealed>]
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
    inherit DefinedType (
        visibility.Flag ||| Unchecked.defaultof<'Kind>.RequiredFlags ||| flags.Flags,
        extends,
        typeNamespace,
        enclosingClass,
        typeName
    )

type DefinedType with
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

[<Sealed>]
type TypeReference<'Kind when 'Kind :> IAttributeTag<TypeDefFlags>>
    (
        parent: TypeReferenceParent,
        typeNamespace: Identifier voption,
        typeName: Identifier
    )
    =
    inherit ReferencedType(parent, typeNamespace, typeName)

type ReferencedType with
    static member ConcreteClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.ConcreteClass>(resolutionScope, typeNamespace, typeName)




    static member SealedClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.SealedClass>(resolutionScope, typeNamespace, typeName)

    static member StaticClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.StaticClass>(resolutionScope, typeNamespace, typeName)

type GenericType with
    static member Defined
        (
            definition: 'Definition when 'Definition :> DefinedType and 'Definition : not struct,
            genericParameterList: ImmutableArray<_>
        )
        =
        GenericType<'Definition>(definition, genericParameterList)

[<IsReadOnly; Struct>]
type LocalVariableType =
    val CustomModifiers: ImmutableArray<ModifierType>
    val IsPinned: bool
    val Tag: LocalVariableTag
    val Type: NamedType voption

    new (modifiers, pinned, tag, ltype) =
        { CustomModifiers = modifiers
          IsPinned = pinned
          Tag = tag
          Type = ltype }

[<RequireQualifiedAccess>]
module LocalVariableType =
    let Type(pinned, localVarType) = LocalVariableType(ImmutableArray.Empty, pinned, LocalVariableTag.Type, ValueSome localVarType)
    let ByRef(modifiers, pinned, localVarType) = LocalVariableType(modifiers, pinned, LocalVariableTag.ByRef, ValueSome localVarType)
    let TypedByRef modifiers = LocalVariableType(modifiers, false, LocalVariableTag.TypedByRef, ValueNone)
    let TypedByRef' = TypedByRef ImmutableArray.Empty

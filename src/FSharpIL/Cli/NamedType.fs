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

[<Sealed>]
type InstantiatedType<'Inst when 'Inst :> GenericType> (instantiator: int32 -> GenericParam -> NamedType, t: 'Inst) =
    inherit NamedType(t.TypeNamespace, t.EnclosingType, t.TypeName)

    member val Arguments = InstantiatedTypeArguments(t.GenericParameters.Parameters, instantiator)
    member _.Instantiated = t

[<AbstractClass>]
type NamedType (ns: Identifier voption, parent: NamedType voption, tname: Identifier) =
    member _.TypeName = tname
    member _.TypeNamespace = ns
    member _.EnclosingType = parent
    member _.IsNested = parent.IsSome

    member private this.CaseEquals(other: NamedType) =
        match this, other with
        | (:? ModifiedType as mtype), _ -> mtype.Modified.Equals(other = other)
        | _, (:? ModifiedType as mtype) -> this.Equals(other = mtype.Modified)
        | (:? PrimitiveType as x), (:? PrimitiveType as y) -> x.Encoded = y.Encoded
        | (:? ArrayType), (:? SZArrayType)
        | (:? SZArrayType), (:? ArrayType) -> false
        | (:? ArrayType as x), (:? ArrayType as y) -> x.ElementType.Equals(other = y.ElementType)
        | (:? PrimitiveType), _
        | _, (:? PrimitiveType)
        | (:? DefinedType), (:? DefinedType)
        | (:? ReferencedType), (:? ReferencedType) -> true
        | (:? InstantiatedType<DefinedType> as x), (:? InstantiatedType<DefinedType> as y) ->
            x.Instantiated.Equals(other = y.Instantiated) && x.Arguments.Equals(other = y.Arguments)
        | (:? InstantiatedType<ReferencedType> as x), (:? InstantiatedType<ReferencedType> as y) ->
            x.Instantiated.Equals(other = y.Instantiated) && x.Arguments.Equals(other = y.Arguments)
        | _ -> false

    member this.Equals(other: NamedType) =
        this.TypeNamespace = other.TypeNamespace &&
        this.TypeName = other.TypeName &&
        this.EnclosingType = other.EnclosingType &&
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

    let inline (|IsSystemType|) expected (t: #NamedType) =
        t.TypeNamespace = PrimitiveType.ns && t.EnclosingType.IsNone && t.TypeName = Identifier expected

type GenericParamKind =
    | Invariant
    | Covariant
    | Contravariant

[<IsReadOnly; Struct>]
type GenericSpecialConstraint =
    | NoSpecialConstriant
    | ReferenceTypeConstraint
    | NonNullableValueTypeConstraint

[<NoComparison; CustomEquality>]
type GenericParam =
    { Name: Identifier
      SpecialConstraint: GenericSpecialConstraint
      RequiresDefaultConstructor: bool
      Constraints: ImmutableArray<NamedType> } // TODO: Constraint list should be a set.

    member this.Equals other = this.Name = other.Name

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals(obj: obj) =
        match obj with
        | :? GenericParam as other -> this.Equals(other = other)
        | _ -> false

    interface IEquatable<GenericParam> with member this.Equals other = this.Equals(other = other)

[<RequireQualifiedAccess>]
module GenericParam =
    let named name =
        { Name = name
          SpecialConstraint = NoSpecialConstriant
          RequiresDefaultConstructor = false
          Constraints = ImmutableArray.Empty }

// TODO: Include whether or not the parameters are covariant or contravariant. (GenericParamKind)
[<IsReadOnly>]
[<NoComparison; NoEquality>]
type GenericParamList (parameters: ImmutableArray<GenericParam>) = struct
    member _.Parameters = parameters
end

[<RequireQualifiedAccess>]
module GenericParamList =
    let empty = GenericParamList ImmutableArray.Empty

    let inline tryOfCollection capacity move current add finish =
        let mutable lookup = HybridHashSet<GenericParam> capacity
        let mutable duplicate: GenericParam voption = ValueNone

        while duplicate.IsNone && move() do
            let current = current()
            if lookup.Add current
            then add current
            else duplicate <- ValueSome current

        match duplicate with
        | ValueNone -> GenericParamList(finish()) |> Ok
        | ValueSome duplicate' -> Error duplicate'

    let tryOfSeq (parameters: seq<_>) =
        let builder = ImmutableArray.CreateBuilder()
        let mutable enumerator: IEnumerator<_> = parameters.GetEnumerator()
        tryOfCollection
            0
            (fun() -> enumerator.MoveNext())
            (fun() -> enumerator.Current)
            (fun gparam -> builder.Add gparam)
            (fun() ->
                if builder.Count = builder.Capacity
                then builder.MoveToImmutable()
                else builder.ToImmutable())

    let tryOfArray parameters =
        match parameters with
        | null
        | [||] -> Ok empty
        | _ ->
            let mutable parameters', i = Array.zeroCreate parameters.Length, -1
            tryOfCollection
                parameters.Length
                (fun() ->
                    i <- i + 1
                    i < parameters.Length)
                (fun() -> parameters.[i])
                (fun gparam -> parameters'.[i] <- gparam)
                (fun() -> Unsafe.As &parameters')

    let tryOfBlock (parameters: ImmutableArray<_>) =
        if parameters.IsDefaultOrEmpty
        then Ok empty
        else
            let mutable enumerator = parameters.GetEnumerator()
            tryOfCollection
                parameters.Length
                (fun() -> enumerator.MoveNext())
                (fun() -> enumerator.Current)
                ignore
                (fun() -> parameters)

    let inline duplicateGenericParam (duplicate: GenericParam) =
        invalidArg
            "parameters"
            (sprintf "A generic parameter with the same name \"%O\" already exists in the generic parameter list" duplicate.Name)

    let ofSeq parameters =
        match tryOfSeq parameters with
        | Ok parameters' -> parameters'
        | Error dup -> duplicateGenericParam dup

[<AutoOpen>]
module GenericParamListPatterns =
    let inline (|GenericParamList|) (parameters: GenericParamList) = parameters.Parameters

[<IsReadOnly; Struct>]
type ModifierType (required: bool, modifier: NamedType) =
    member _.Required = required
    member _.Modifier = modifier

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
type GenericType =
    inherit NamedType

    val GenericParameters: GenericParamList

    new (ns, parent, tname, gparams) =
        { inherit NamedType(ns, parent, tname)
          GenericParameters = gparams }

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

[<Sealed>]
type ReferencedType (rscope, ns, tname, gparams) =
    inherit GenericType(ns, TypeReferenceParent.toType rscope, tname, gparams)

    member _.ResolutionScope = rscope

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

[<Sealed>]
type DefinedType =
    inherit GenericType

    val Flags: TypeDefFlags
    val Extends: ClassExtends

    new (flags, extends, ns, enclosing, tname, gparams) =
        { inherit GenericType(ns, Convert.unsafeValueOption<DefinedType, _> enclosing, tname, gparams)
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

type InstantiatedTypeArgumentsEnumerator = struct
    val mutable internal Index: int32
    val internal Parameters: ImmutableArray<GenericParam>
    val internal Instantiator: int32 -> GenericParam -> NamedType

    new (parameters, instantiator) =
        { Index = -1
          Parameters = parameters
          Instantiator = instantiator }

    member this.Current =
        if this.Index < 0 then enumeratorNotStarted()
        elif this.Index >= this.Parameters.Length then enumeratorReachedEnd()
        else this.Instantiator this.Index this.Parameters.[this.Index]

    member this.MoveNext() =
        if this.Index < this.Parameters.Length then
            this.Index <- this.Index + 1
        this.Index < this.Parameters.Length

    interface IEnumerator<NamedType> with
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.Reset() = this.Index <- -1
        member this.MoveNext() = this.MoveNext()
        member _.Dispose() = ()
end

[<IsReadOnly>]
[<NoComparison; CustomEquality>]
type InstantiatedTypeArguments (gparams: ImmutableArray<GenericParam>, instantiator: int32 -> GenericParam -> NamedType) = struct
    member _.Count = gparams.Length

    member _.Item with get index = instantiator index gparams.[index]

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
    let inline (|Null|Defined|Referenced|DefinedGeneric|ReferencedGeneric|) (extends: ClassExtends) =
        match extends.Extends with
        | ValueNone -> Null
        | ValueSome(:? ReferencedType as tref) -> Referenced tref
        | ValueSome(:? DefinedType as tdef) -> Defined tdef
        | ValueSome(:? InstantiatedType<ReferencedType> as tref) -> ReferencedGeneric tref
        | ValueSome tdef -> DefinedGeneric(tdef :?> InstantiatedType<DefinedType>)

    let Null = ClassExtends ValueNone
    let Defined (NamedType extends: DefinedType) = ClassExtends(ValueSome extends)
    let DefinedGeneric (NamedType extends: InstantiatedType<DefinedType>) = ClassExtends(ValueSome extends)
    let Referenced (NamedType extends: ReferencedType) = ClassExtends(ValueSome extends)
    let ReferencedGeneric (NamedType extends: InstantiatedType<ReferencedType>) = ClassExtends(ValueSome extends)

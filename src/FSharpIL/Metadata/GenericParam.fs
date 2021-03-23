namespace FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type GenericParamOwnerTag =
    | TypeDef = 0uy
    | MethodDef = 1uy

/// <summary>Represents a <c>TypeOrMethodDef</c> coded index, which specifies the owner of a generic parameter</summary>
type GenericParamOwner = TaggedIndex<GenericParamOwnerTag>

[<RequireQualifiedAccess>]
module GenericParamOwner =
    let (|TypeDef|MethodDef|) (owner: GenericParamOwner) =
        match owner.Tag with
        | GenericParamOwnerTag.TypeDef -> TypeDef(owner.ToRawIndex<TypeDefRow>())
        | GenericParamOwnerTag.MethodDef -> MethodDef(owner.ToRawIndex<MethodDefRow>())
        | bad -> ArgumentOutOfRangeException("owner", bad, "Invalid generic parameter owner") |> raise

    let TypeDef (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex GenericParamOwnerTag.TypeDef
    let MethodDef (index: RawIndex<MethodDefRow>) = index.ToTaggedIndex GenericParamOwnerTag.MethodDef

/// <summary>(0x2A) Represents a row in the <c>GenericParam</c> table (II.22.20).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericParamRow internal (number: uint16, flags: GenericParameterAttributes, owner: GenericParamOwner, name: Identifier) =
    member _.Number = number
    member _.Flags = flags
    member _.Owner = owner
    member _.Name = name

    interface IEquatable<GenericParamRow> with
        member this.Equals other =
            this.Owner = other.Owner
            && this.Number = other.Number
            && this.Name = other.Name

[<IsReadOnly; Struct>]
type GenericParamFlags private (flags: GenericParameterAttributes) =
    member _.Value = flags

    static member private SetDefaultConstructor(flags: byref<GenericParameterAttributes>, set: bool) =
        if set then flags <- flags ||| GenericParameterAttributes.DefaultConstructorConstraint

    static member None = GenericParamFlags GenericParameterAttributes.None

    static member DefaultConstructor = GenericParamFlags GenericParameterAttributes.DefaultConstructorConstraint

    static member ReferenceType([<Optional; DefaultParameterValue(false)>] ctor: bool) =
        let mutable flags = GenericParameterAttributes.ReferenceTypeConstraint
        GenericParamFlags.SetDefaultConstructor(&flags, ctor)
        GenericParamFlags flags

    /// NotNullableValueTypeConstraint
    static member NonNullableValueType([<Optional; DefaultParameterValue(false)>] ctor: bool) =
        let mutable flags = GenericParameterAttributes.NotNullableValueTypeConstraint
        GenericParamFlags.SetDefaultConstructor(&flags, ctor)
        GenericParamFlags flags

    interface IFlags<GenericParameterAttributes> with member _.Value = flags

[<AbstractClass; Sealed>] type InvariantGenericParamFlags = class end
// TODO: These names are not good, figure out how to allow NonVariant generic params to be assigned to Covariant or Contravariant generic params.
[<AbstractClass; Sealed>] type CovariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type ContravariantGenericParamFlags = class end

type GenericParamConstraintTag =
    | DefConcrete = 0uy
    | DefAbstract = 1uy
    | DefInterface = 2uy
    | Ref = 3uy
    | Spec = 4uy

[<IsReadOnly; Struct>]
[<CustomComparison; CustomEquality>]
type GenericParamConstraint internal (tag: GenericParamConstraintTag, value: int32) =
    member _.Tag = tag
    member _.Value = value

    member _.IsTypeDef = tag < GenericParamConstraintTag.DefInterface

    member internal _.ToRawIndex() = RawIndex value

    member private this.Equals(other: GenericParamConstraint) =
        (this.IsTypeDef && other.IsTypeDef) || (tag = other.Tag && value = other.Value)

    override this.GetHashCode() = (this.Value <<< 3) ||| int32 tag

    override this.Equals obj =
        match obj with
        | :? GenericParamConstraint as other -> this.Equals other
        | _ -> false

    interface IComparable<GenericParamConstraint> with
        member this.CompareTo other =
            if this.IsTypeDef && other.IsTypeDef
            then compare value other.Value
            else
                match compare tag other.Tag with
                | 0 -> compare value other.Value
                | i -> i

    interface IEquatable<GenericParamConstraint> with member this.Equals other = this.Equals other

    interface IComparable with
        member this.CompareTo other = (this :> IComparable<GenericParamConstraint>).CompareTo(other :?> GenericParamConstraint)

[<RequireQualifiedAccess>]
module GenericParamConstraint =
    let (|Class|AbstractClass|Interface|TypeRef|TypeSpec|) (constr: GenericParamConstraint) =
        match constr.Tag with
        | GenericParamConstraintTag.DefConcrete -> Class(constr.ToRawIndex<ConcreteClassDef>())
        | GenericParamConstraintTag.DefAbstract -> AbstractClass(constr.ToRawIndex<AbstractClassDef>())
        | GenericParamConstraintTag.DefInterface -> Interface(constr.ToRawIndex<InterfaceDef>())
        | GenericParamConstraintTag.Ref -> TypeRef(constr.ToRawIndex<TypeRef>())
        | GenericParamConstraintTag.Spec -> TypeSpec(constr.ToRawIndex<TypeSpecRow>())
        | bad -> ArgumentOutOfRangeException("constr", bad, "Invalid generic parameter constraint tag") |> raise

    let Class (Index index: RawIndex<ConcreteClassDef>) = GenericParamConstraint(GenericParamConstraintTag.DefConcrete, index)
    let AbstractClass (Index index: RawIndex<AbstractClassDef>) = GenericParamConstraint(GenericParamConstraintTag.DefAbstract, index)
    let Interface (Index index: RawIndex<InterfaceDef>) = GenericParamConstraint(GenericParamConstraintTag.DefInterface, index)
    let TypeRef (Index index: RawIndex<TypeRef>) = GenericParamConstraint(GenericParamConstraintTag.Ref, index)
    let TypeSpec (Index index: RawIndex<TypeSpecRow>) = GenericParamConstraint(GenericParamConstraintTag.Spec, index)

// TODO: Figure out how to represent covariant or contravariant generic parameters.

[<IsReadOnly; Struct>]
type GenericParamConstraintSet internal (constraints: ImmutableArray<GenericParamConstraint>) =
    member _.GetEnumerator() = constraints.GetEnumerator()
    member _.ToImmutableArray() = constraints
    static member val Empty = GenericParamConstraintSet ImmutableArray.Empty

/// Contains operations for creating sets of generic parameter constraints.
[<RequireQualifiedAccess>]
module ConstraintSet =
    let empty = GenericParamConstraintSet.Empty
    let singleton(constr: GenericParamConstraint) = GenericParamConstraintSet(ImmutableArray.Create constr)
    let ofSeq (constraints: seq<GenericParamConstraint>) =
        let lookup, constraints' = HashSet(), ImmutableArray.CreateBuilder()
        let mutable enumerator, success = constraints.GetEnumerator(), true
        while success && enumerator.MoveNext() do
            let current = enumerator.Current
            if lookup.Add current
            then constraints'.Add current
            else success <- false
        if success
        then GenericParamConstraintSet(constraints'.ToImmutable()) |> ValueSome
        else ValueNone
    let inline private create (constraints: seq<_>) = GenericParamConstraintSet(constraints.ToImmutableArray())
    let ofSet (constraints: Set<_>) = create constraints
    let ofHashSet (constraints: HashSet<_>) = create constraints
    let ofImmutableSet (constraints: IImmutableSet<_>) = create constraints

/// <summary>(0x2C) Represents a row in the <c>GenericParamConstraint</c> table (II.22.21).</summary>
[<IsReadOnly; Struct>]
type GenericParamConstraintRow internal (owner: RawIndex<GenericParamRow>, constr: GenericParamConstraint) =
    member _.Owner = owner
    member _.Constraint = constr

[<Sealed>]
type GenericParamTableBuilder internal () =
    let rows = ImmutableArray.CreateBuilder<GenericParamRow>()
    let constraints' = ImmutableArray.CreateBuilder<GenericParamConstraintRow>()
    let lookup = Dictionary<GenericParamOwner, Dictionary<GenericParamRow, ImmutableArray<GenericParamConstraint>>>()

    member _.Count = lookup.Count

    member internal _.ToImmutable() = MetadataTable(rows.ToImmutable())
    member internal _.GetConstraints() = MetadataTable(constraints'.ToImmutable())

    // TODO: Ensure that owner of generic parameters cannot be a non-nested enum type.
    member private _.TryAdd(flags, owner, name, constraints: ImmutableArray<GenericParamConstraint>) =
        let gparams =
            match lookup.TryGetValue owner with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = Dictionary<GenericParamRow, _>()
                lookup.[owner] <- empty
                empty
        let row = GenericParamRow(uint16 gparams.Count,flags, owner, name)
        if gparams.TryAdd(row, constraints) then
            rows.Add row
            let parami = RawIndex<GenericParamRow> rows.Count
            let constraints'' =
                Array.init
                    constraints.Length
                    (fun i ->
                        let i' = RawIndex<GenericParamConstraintRow>(constraints'.Count + 1)
                        constraints'.Add(GenericParamConstraintRow(parami, constraints.[i]))
                        i')
            struct(parami, constraints'') |> ValueSome
        else ValueNone

    member this.TryAddNonvariant(Flags flags: GenericParamFlags, owner, name, constraints: GenericParamConstraintSet) =
        this.TryAdd(flags, owner, name, constraints.ToImmutableArray())

/// <summary>
/// Error used when a duplicate generic parameter was added to the <c>GenericParam</c> table (10, 11).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateGenericParamError (owner: GenericParamOwner, name: Identifier) =
    inherit ValidationError()
    member _.Owner = owner
    member _.Name = name
    override _.ToString() =
        sprintf
            "Unable to add duplicate generic parameter \"%O\", a generic parameter with the same name and owner \"%O\" already exists"
            name
            owner

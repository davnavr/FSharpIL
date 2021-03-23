namespace FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type GenericParamOwnerTag =
    | TypeDef = 1uy
    | MethodDef = 2uy

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

// TODO: Make new constraint struct type, since TypeDef constraints should be equal to prevent more than one class constraint.
type GenericParamConstraint = TaggedIndex<GenericParamConstraintTag>

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

    let Class (index: RawIndex<ConcreteClassDef>) = index.ToTaggedIndex GenericParamConstraintTag.DefConcrete
    let AbstractClass (index: RawIndex<AbstractClassDef>) = index.ToTaggedIndex GenericParamConstraintTag.DefAbstract
    let Interface (index: RawIndex<InterfaceDef>) = index.ToTaggedIndex GenericParamConstraintTag.DefInterface
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex GenericParamConstraintTag.Ref
    let TypeSpec (index: RawIndex<TypeSpecRow>) = index.ToTaggedIndex GenericParamConstraintTag.Spec

// TODO: Figure out how to represent covariant or contravariant generic parameters.
//[<Sealed>]
//type GenericParamConstraintBuilder internal () =
//    let lookup = HashSet<GenericParamConstraint>()
//    member _.TryAdd constr = lookup.Add constr

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
    // TODO: Ensure that constraint specifies only zero or one class, since multi-class inheritance does not exist.
    member internal _.TryAdd(flags, owner, name, constraints: ImmutableArray<GenericParamConstraint>) =
        let gparams =
            match lookup.TryGetValue owner with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = Dictionary<GenericParamRow, _>()
                lookup.[owner] <- empty
                empty
        let row = GenericParamRow(uint16 gparams.Count,flags, owner, name)
        if gparams.TryAdd(row, constraints) then
            let parami = RawIndex<GenericParamRow> rows.Count
            rows.Add row
            let constraints'' =
                Array.init
                    constraints.Length
                    (fun i ->
                        let i' = RawIndex<GenericParamConstraintRow> constraints'.Count
                        constraints'.Add(GenericParamConstraintRow(parami, constraints.[i]))
                        i')
            struct(parami, constraints'') |> ValueSome
        else ValueNone

namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection

// II.22.21
[<StructuralComparison; StructuralEquality>]
type GenericParamConstraint =
    | ClassConstraint of TypeIndex<ConcreteClassDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeDef</c> representing an abstract class.
    /// </summary>
    | AbstractClassConstraint of TypeIndex<AbstractClassDef>
    | InterfaceConstraint of TypeIndex<InterfaceDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeRef</c> representing a class or interface.
    /// </summary>
    | TypeRefConstraint of SimpleIndex<TypeRef>
    // | TypeSpecConstraint

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | ClassConstraint (SimpleIndex concrete) -> IndexOwner.checkIndex owner concrete
            | AbstractClassConstraint (SimpleIndex abst) -> IndexOwner.checkIndex owner abst
            | InterfaceConstraint (SimpleIndex intf) -> IndexOwner.checkIndex owner intf
            | TypeRefConstraint tref -> IndexOwner.checkIndex owner tref

// NOTE: It is an error to have duplicate constraints.
/// <summary>Represents the constraints of a generic parameter, contained in the <c>GenericParamConstraint</c> table (II.22.21).</summary>
[<Sealed>]
type GenericParamConstraintSet private (constraints: IReadOnlyCollection<GenericParamConstraint>) =
    new (constraints: IImmutableSet<_>) = GenericParamConstraintSet(constraints :> IReadOnlyCollection<_>)
    new (constraints: Set<_>) = GenericParamConstraintSet(constraints :> IReadOnlyCollection<_>)

    member _.Count = constraints.Count

    interface IIndexValue with
        member _.CheckOwner owner =
            for constr in constraints do IndexOwner.checkOwner owner constr

[<AutoOpen>]
module GenericParamExtensions =
    type GenericParam with
        /// <summary>Represents the corresponding rows in the <c>GenericParamConstraint</c> table for this generic parameter.</summary>
        member this.Constraints = this.ConstraintSet :?> GenericParamConstraintSet

[<Sealed>]
type internal GenericParamList<'Flags> (owner: IndexOwner) =
    let lookup = HashSet<Identifier>()
    let gparams = ImmutableArray.CreateBuilder<GenericParam>()

    member _.Count = gparams.Count
    member _.ToImmutable() = IndexedList(owner, gparams)

    member _.Add(Flags flags: ValidFlags<'Flags, GenericParameterAttributes>, name, constraints: GenericParamConstraintSet) =
        IndexOwner.checkOwner owner constraints
        if lookup.Add name then
            let i = gparams.Count
            GenericParam(flags, name, constraints) |> gparams.Add
            ValueSome i
        else ValueNone

(*
[<RequireQualifiedAccess>]
module GenericParam =
    let addInvariant name constraints flags (list: GenericParamList<InvariantGenericParamFlags>) = list.Add(flags, name, constraints)
    // let addCovariant
    ()
*)

namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection

// II.22.21
[<StructuralComparison; StructuralEquality>]
type GenericParamConstraint = // TODO: Replace generic constraint with a struct.
    | ClassConstraint of RawIndex<ConcreteClassDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeDef</c> representing an abstract class.
    /// </summary>
    | AbstractClassConstraint of RawIndex<AbstractClassDef>
    | InterfaceConstraint of RawIndex<InterfaceDef>
    /// <summary>
    /// Indicates that the generic parameter is constrainted to derive from a <c>TypeRef</c> representing a class or interface.
    /// </summary>
    | TypeRefConstraint of RawIndex<TypeRef>
    // | TypeSpecConstraint

// NOTE: It is an error to have duplicate constraints.
/// <summary>Represents the constraints of a generic parameter, contained in the <c>GenericParamConstraint</c> table (II.22.21).</summary>
[<Sealed>]
type GenericParamConstraintSet private (constraints: IReadOnlyCollection<GenericParamConstraint>) =
    new (constraints: IImmutableSet<_>) = GenericParamConstraintSet(constraints :> IReadOnlyCollection<_>)
    new (constraints: Set<_>) = GenericParamConstraintSet(constraints :> IReadOnlyCollection<_>)

    member _.Count = constraints.Count

[<AutoOpen>]
module GenericParamExtensions =
    type GenericParam with
        /// <summary>Represents the corresponding rows in the <c>GenericParamConstraint</c> table for this generic parameter.</summary>
        member this.Constraints = this.ConstraintSet :?> GenericParamConstraintSet

[<Sealed>]
type internal GenericParamList<'Flags> () =
    let lookup = HashSet<Identifier>()
    let gparams = ImmutableArray.CreateBuilder<GenericParam>()

    member _.Count = gparams.Count
    member _.ToImmutable() = IndexedList(gparams.ToImmutable())

    member _.Add(Flags flags: ValidFlags<'Flags, GenericParameterAttributes>, name, constraints: GenericParamConstraintSet) =
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

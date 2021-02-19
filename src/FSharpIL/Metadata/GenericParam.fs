namespace FSharpIL.Metadata

// TODO: Create GenericParamModule that contains constructor functions.
// TODO: Since GenericParam will have constructor functions, its Flags should just be GenericParameterAttributes, and it should have a 'Tag instead.

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

[<Interface>] type internal IGenericParamConstraint = inherit IIndexValue

/// <summary>(0x2A) Represents a row in the <c>GenericParam</c> table (II.22.20).</summary>
[<IsReadOnly>]
[<NoComparison; CustomEquality>]
type GenericParam<'Flags> =
    struct
        val Flags: ValidFlags<'Flags, GenericParameterAttributes>
        val Name: Identifier
        // <summary>Represents the corresponding rows in the <c>GenericParamConstraint</c> table for this generic parameter.</summary>
        val internal ConstraintSet: ImmutableHashSet<IGenericParamConstraint> // Constraints

        internal new (flags, name, constraints) =
            { Flags = flags
              Name = name
              ConstraintSet = constraints }

        member internal this.CheckOwner owner =
            for constr in this.ConstraintSet do constr.CheckOwner owner

        interface IIndexValue with member this.CheckOwner owner = this.CheckOwner owner

        interface IEquatable<GenericParam<'Flags>> with
            member this.Equals other = this.Name = other.Name
    end

[<IsReadOnly; Struct>]
type GenericParamFlags =
    { ReferenceType: bool
      /// <summary><c>NotNullableValueType</c></summary>
      ValueType: bool
      DefaultConstructor: bool }

    member this.Value =
        let mutable flags = GenericParameterAttributes.None
        if this.ReferenceType then flags <- GenericParameterAttributes.ReferenceTypeConstraint
        if this.ValueType then flags <- GenericParameterAttributes.NotNullableValueTypeConstraint
        if this.DefaultConstructor then flags <- GenericParameterAttributes.DefaultConstructorConstraint
        flags

    static member None = { ReferenceType = false; ValueType = false; DefaultConstructor = false }

    interface IFlags<GenericParameterAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type NonVariantGenericParamFlags = class end
// TODO: These names are not good, figure out how to allow NonVariant generic params to be assigned to Covariant or Contravariant generic params.
[<AbstractClass; Sealed>] type CovariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type ContravariantGenericParamFlags = class end

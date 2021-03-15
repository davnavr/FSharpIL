namespace FSharpIL.Metadata

open System
open System.Reflection
open System.Runtime.CompilerServices

/// <summary>(0x2A) Represents a row in the <c>GenericParam</c> table (II.22.20).</summary>
[<IsReadOnly>]
[<NoComparison; CustomEquality>]
type GenericParam = struct
    val Flags: GenericParameterAttributes
    val Name: Identifier
    val internal ConstraintSet: obj // Constraints

    internal new (flags, name, constraints) =
        { Flags = flags
          Name = name
          ConstraintSet = constraints }

    interface IEquatable<GenericParam> with
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

[<AbstractClass; Sealed>] type InvariantGenericParamFlags = class end
// TODO: These names are not good, figure out how to allow NonVariant generic params to be assigned to Covariant or Contravariant generic params.
[<AbstractClass; Sealed>] type CovariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type ContravariantGenericParamFlags = class end

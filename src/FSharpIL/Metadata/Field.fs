namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

/// <summary>Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags, name, signature) = // TODO: How to allow different types for signature.
    member _.Flags: FieldAttributes = flags
    member _.Name: Identifier = name
    member _.Signature = signature

    member internal _.SkipDuplicateChecking = flags &&& FieldAttributes.FieldAccessMask = FieldAttributes.PrivateScope

    override this.Equals obj =
        match obj with
        | :? FieldRow as other -> (this :> IEquatable<_>).Equals other
        | _ -> false

    override _.GetHashCode() = hash(name, signature)

    interface IEquatable<FieldRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else name = other.Name && signature = other.Signature

    interface IIndexValue with
        member this.CheckOwner actual = invalidOp "bad"

type IField =
    inherit IIndexValue
    abstract Row : unit -> FieldRow

[<AutoOpen>]
module FieldHelpers =
    let inline internal (|FieldRow|) (f: #IField) = f.Row()

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type FieldFlags<'Visibility when 'Visibility :> IFlags<FieldAttributes>> =
    { Visibility: 'Visibility
      NotSerialized: bool
      /// Sets the `SpecialName` and `RTSpecialName` flags. // TODO: SpecialName is required to be set if RTSpecialName is set, so allow fields that set special name but don't set RTSpecialName.
      SpecialName: bool }

    member this.Value =
        let mutable flags = this.Visibility.Value
        if this.NotSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        if this.SpecialName then flags <- flags ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        flags

    interface IFlags<FieldAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type InstanceFieldFlags = class end
[<AbstractClass; Sealed>] type StaticFieldFlags = class end
[<AbstractClass; Sealed>] type GlobalFieldFlags = class end

[<StructuralComparison; StructuralEquality>]
type Field<'Flags, 'Signature when 'Signature : equality and 'Signature :> IIndexValue> =
    { Flags: ValidFlags<'Flags, FieldAttributes>
      FieldName: Identifier
      Signature: 'Signature }

    interface IField with
        member this.CheckOwner actual = this.Signature.CheckOwner actual
        member this.Row() = FieldRow(this.Flags.Value, this.FieldName, ())

/// Captures the definition of a field or global variable (II.23.2.4).
[<IsReadOnly; Struct>]
type FieldSignature =
    { CustomMod: ImmutableArray<CustomModifier>
      FieldType: ReturnTypeItem }

    interface IIndexValue with
        member this.CheckOwner owner =
            for cmod in this.CustomMod do cmod.CheckOwner owner
            this.FieldType.CheckOwner owner

/// <summary>Represents a non-static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
type InstanceField = Field<InstanceFieldFlags, FieldSignature>

/// <summary>Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/>.</summary>
type StaticField = Field<StaticFieldFlags, FieldSignature>

// TODO: Come up with a better name for the type.
type FieldChoice =
    | InstanceField of InstanceField
    | StaticField of StaticField

    interface IField with
        member this.CheckOwner actual =
            match this with
            | InstanceField ifield -> IndexOwner.checkOwner actual ifield
            | StaticField sfield -> IndexOwner.checkOwner actual sfield

        member this.Row() =
            match this with
            | InstanceField (FieldRow row)
            | StaticField (FieldRow row) -> row

/// <summary>
/// Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/> defined inside of the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
type GlobalField = Field<GlobalFieldFlags, FieldSignature>

namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

/// <summary>
/// Represents a <c>FieldSig</c> item, which captures the definition of a field or global variable (II.23.2.4).
/// </summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type FieldSignature =
    struct
        val CustomMod: ImmutableArray<CustomModifier>
        val internal Type: IEncodedType // FieldType
        internal new (modifiers, fieldType) = { CustomMod = modifiers; Type = fieldType }

        member internal this.CheckOwner owner =
            for modifier in this.CustomMod do modifier.CheckOwner owner
            this.Type.CheckOwner owner

        override this.ToString() = this.Type.ToString()
    end

/// <summary>Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags, name, signature) =
    member _.Flags: FieldAttributes = flags
    member _.Name: Identifier = name
    member _.Signature: FieldSignature = signature

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
        member _.CheckOwner owner = signature.CheckOwner owner

type FieldIndex<'Tag> = TaggedIndex<'Tag, FieldRow>

/// <summary>
/// Error used when there is a duplicate row in the <c>Field</c> table (17).
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateFieldError (field: FieldRow) =
    inherit ValidationError()
    member _.Field = field

type IField<'Parent> =
    inherit IIndexValue
    abstract Row : unit -> FieldRow

[<AutoOpen>]
module internal FieldHelpers =
    let inline (|FieldRow|) (f: #IField<_>) = f.Row()

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type FieldFlags<'Visibility when 'Visibility :> IFlags<FieldAttributes>> =
    { Visibility: 'Visibility
      NotSerialized: bool
      // TODO: SpecialName is required to be set if RTSpecialName is set, so allow fields that set special name but don't set RTSpecialName.
      /// <summary>Sets the <c>SpecialName</c> and <c>RTSpecialName</c> flags.</summary>
      SpecialName: bool }

    member this.Value =
        let mutable flags = this.Visibility.Value
        if this.NotSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        if this.SpecialName then flags <- flags ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        flags

    interface IFlags<FieldAttributes> with member this.Value = this.Value

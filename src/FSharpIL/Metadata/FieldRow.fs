namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

/// <summary>
/// Represents a <c>FieldSig</c> item, which captures the definition of a field or global variable (II.23.2.4).
/// </summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type FieldSignature = struct
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

    override _.ToString() =
        let visibility =
            match flags ||| FieldAttributes.FieldAccessMask with
            | FieldAttributes.Private -> "private"
            | FieldAttributes.Assembly -> "assembly"
            | FieldAttributes.FamANDAssem -> "famandassem"
            | FieldAttributes.Family -> "family"
            | FieldAttributes.FamORAssem -> "famorassem"
            | FieldAttributes.Public -> "public"
            | _ -> "compilercontrolled"

        let fstatic =
            if flags.HasFlag FieldAttributes.Static
            then " static" // + " class"
            else String.Empty

        // TODO: Add other flags when printing fields.
        sprintf ".field %s%s %O %O" visibility fstatic signature name

    interface IEquatable<FieldRow> with
        member this.Equals other =
            if this.SkipDuplicateChecking || other.SkipDuplicateChecking
            then false
            else name = other.Name && signature = other.Signature

    interface IIndexValue with
        member _.CheckOwner owner = signature.CheckOwner owner

type FieldIndex<'Tag> = TaggedIndex<'Tag, FieldRow>

/// <summary>Error used when there is a duplicate row in the <c>Field</c> table (17).</summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateFieldError (field: FieldRow) =
    inherit ValidationError()
    member _.Field = field

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type FieldFlags<'Visibility when 'Visibility :> IFlags<FieldAttributes>> = struct
    val Visibility: 'Visibility
    val NotSerialized: bool
    /// <summary>Gets a value indicating whether the <c>SpecialName</c> or <c>RTSpecialName</c> flags are set.</summary>
    val SpecialName: SpecialName

    /// <param name="visibility">Specifies from where this field can be accessed.</param>
    /// <param name="specialName">Sets the <c>SpecialName</c> or <c>RTSpecialName</c> flags.</param>
    /// <param name="notSerialized">
    /// If set to <see langword="true"/>, sets the <c>NotSerialized</c> flag. Defaults to <see langword="false"/>.
    /// </param>
    new (visibility, specialName, [<Optional; DefaultParameterValue(false)>] notSerialized) =
        { Visibility = visibility
          NotSerialized = notSerialized
          SpecialName = specialName }

    new
        (
            visibility,
            [<Optional; DefaultParameterValue(false)>] isSpecialName,
            [<Optional; DefaultParameterValue(true)>] notSerialized: bool
        ) =
        let specialName = if isSpecialName then SpecialName else NoSpecialName
        FieldFlags(visibility, specialName, notSerialized)

    member this.Value =
        let (Flags (specialName: FieldAttributes)) = this.SpecialName
        let mutable flags = this.Visibility.Value ||| specialName
        if this.NotSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        flags

    interface IFlags<FieldAttributes> with member this.Value = this.Value
end

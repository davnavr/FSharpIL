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
type FieldSignature = struct // TODO: Move to Items.fs, because of Blob`1
    val CustomMod: ImmutableArray<CustomModifier>
    val internal Type: IEncodedType // FieldType
    internal new (modifiers, fieldType) = { CustomMod = modifiers; Type = fieldType }
    override this.ToString() = this.Type.ToString()
end

//type FieldSig = class end

/// <summary>Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags, name, signature) =
    member _.Flags: FieldAttributes = flags
    member _.Name: Identifier = name
    member _.Signature: Blob<FieldSignature> = signature

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

/// <summary>Error used when there is a duplicate row in the <c>Field</c> table (17).</summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateFieldError (field: FieldRow) =
    inherit ValidationError()
    member _.Field = field

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type FieldFlags<'Visibility when 'Visibility :> IFlags<FieldAttributes>> = struct
    val Value: FieldAttributes

    /// <param name="visibility">Specifies from where this field can be accessed.</param>
    /// <param name="specialName">Sets the <c>SpecialName</c> or <c>RTSpecialName</c> flags.</param>
    /// <param name="initOnly">Sets the <c>InitOnly</c> flag, which is used for <see langword="readonly"/> fields in C#.</param>
    /// <param name="notSerialized">
    /// If set to <see langword="true"/>, sets the <c>NotSerialized</c> flag. Defaults to <see langword="false"/>.
    /// </param>
    new
        (
            visibility: 'Visibility,
            specialName: SpecialName,
            [<Optional; DefaultParameterValue(false)>] initOnly,
            [<Optional; DefaultParameterValue(false)>] notSerialized
        ) =
        let (Flags (specialName: FieldAttributes)) = specialName
        let mutable flags = visibility.Value ||| specialName
        if initOnly then flags <- flags ||| FieldAttributes.InitOnly
        if notSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        { Value = flags }

    new
        (
            visibility,
            [<Optional; DefaultParameterValue(false)>] isSpecialName,
            [<Optional; DefaultParameterValue(false)>] initOnly: bool,
            [<Optional; DefaultParameterValue(false)>] notSerialized: bool
        ) =
        let specialName = if isSpecialName then SpecialName else NoSpecialName
        FieldFlags(visibility, specialName, initOnly, notSerialized)

    interface IFlags<FieldAttributes> with member this.Value = this.Value
end

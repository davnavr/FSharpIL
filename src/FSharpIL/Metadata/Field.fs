namespace FSharpIL.Metadata

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Reflection

[<IsReadOnly>]
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

[<IsReadOnly>]
type LiteralFieldFlags = struct
    val Value: FieldAttributes
    new (visibility: Visibility, specialName: SpecialName) = { Value = (|Flags|) visibility ||| (|Flags|) specialName }
    new (visibility) = LiteralFieldFlags(visibility, NoSpecialName)
    interface IFlags<FieldAttributes> with member this.Value = this.Value
end

type IFieldTag = abstract Flags: FieldAttributes

type InstanceFieldTag = struct
    interface IFieldTag with member _.Flags = FieldAttributes.PrivateScope
end

type StaticFieldTag = struct
    interface IFieldTag with member _.Flags = FieldAttributes.Static
end

type LiteralFieldTag = struct
    interface IFieldTag with member _.Flags = FieldAttributes.Static ||| FieldAttributes.Literal ||| FieldAttributes.HasDefault
end

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type Field<'Flags, 'Tag when 'Flags :> IFlags<FieldAttributes> and 'Tag :> IFieldTag and 'Tag : struct> =
    { Flags: 'Flags
      FieldName: Identifier
      Signature: Blob<FieldSignature> }

    // TODO: Figure out how to set HasDefault flag when corresponding row in constant table exists.
    member internal this.Row() = FieldRow(this.Flags.Value ||| Unchecked.defaultof<'Tag>.Flags, this.FieldName, this.Signature)

/// <summary>
/// Represents a static <see cref="T:FSharpIL.Metadata.FieldRow"/> defined inside of the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
type GlobalField = unit // Field<GlobalVisibility, >

/// <summary>Represents a non-static field in the <c>Field</c> table.</summary>
type InstanceField = Field<FieldFlags<Visibility>, InstanceFieldTag>

/// <summary>Represents a static field in the <c>Field</c> table.</summary>
type StaticField = Field<FieldFlags<Visibility>, StaticFieldTag>

type LiteralField = Field<LiteralFieldFlags, LiteralFieldTag>

// NOTE: Static fields should also be allowed in interfaces.

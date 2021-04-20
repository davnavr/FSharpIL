namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type ParamFlags = struct
    val Value: ParameterAttributes

    new
        (
            [<Optional; DefaultParameterValue(false)>] isIn,
            [<Optional; DefaultParameterValue(false)>] isOut,
            [<Optional; DefaultParameterValue(false)>] isOptional
        ) =
        let mutable flags = ParameterAttributes.None
        if isIn then flags <- flags ||| ParameterAttributes.In
        if isOut then flags <- flags ||| ParameterAttributes.Out
        if isOptional then flags <- flags ||| ParameterAttributes.Optional
        { Value = flags }

    member this.In = this.Value.HasFlag ParameterAttributes.In
    member this.Out = this.Value.HasFlag ParameterAttributes.Out
    member this.Optional = this.Value.HasFlag ParameterAttributes.Optional

    interface IFlags<ParameterAttributes> with member this.Value = this.Value
end

[<IsReadOnly; Struct>]
type Parameter (flags: ParamFlags, name: string, value: ConstantBlob voption) =
    new (flags, name) = Parameter(flags, name, ValueNone)
    new (name) = Parameter(ParamFlags(), name)
    member _.Flags = flags
    member _.Name = name
    member _.Value = value

/// <summary>Represents a row in the <c>Param</c> table (II.22.33).</summary>
[<IsReadOnly; Struct>]
type ParamRow internal (flags: ParamFlags, sequence: uint16, name: string) =
    member _.Flags: ParameterAttributes = (|Flags|) flags
    member _.Sequence = sequence
    member _.Name = name
    override _.ToString() = name

type ParamItemTag =
    | Param = 0uy
    | ByRef = 1uy
    | TypedByRef = 2uy

/// <summary>Represents a <c>Param</c> item used in signatures (II.23.2.10).</summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type ParamItem = struct
    val Tag: ParamItemTag
    val CustomMod: ImmutableArray<CustomModifier>
    val internal ParamType: IEncodedType voption
    internal new (tag, modifiers, paramType) = { Tag = tag; CustomMod = modifiers; ParamType = paramType }
    override this.ToString() = this.ParamType.ToString()
end

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

/// Represents a parameter.
[<IsReadOnly; Struct>]
type Param =
    { Flags: ParamFlags
      /// The name of the parameter.
      ParamName: string }

/// <summary>Represents a row in the <c>Param</c> table (II.22.33)</summary>
type ParamRow =
    | Param of Param
    // | SomeParamWithADefaultValueInTheConstantTable // of Param * ?

    member this.Flags =
        match this with
        | Param { Flags = name } -> name

    member this.ParamName =
        match this with
        | Param { ParamName = name } -> name

    override this.ToString() = this.ParamName

/// <summary>Represents a <c>Param</c> item used in signatures (II.23.2.10).</summary>
[<IsReadOnly>]
[<NoComparison; StructuralEquality>]
type ParamItem = struct
    val CustomMod: ImmutableArray<CustomModifier>
    val internal Type: IEncodedType // ParamType
    internal new (modifiers, paramType) = { CustomMod = modifiers; Type = paramType }
    override this.ToString() = this.Type.ToString()
end

[<RequireQualifiedAccess>]
module ParamList =
    /// <exception cref="T:System.InvalidOperationException">Thrown when this function is called.</exception>
    let empty (_: ParamItem) (_: int32) = invalidOp "The parameter list was expected to be empty."
    let singleton (param: ParamRow) = fun (_: ParamItem) (_: int32)-> param
    let noname (_: ParamItem) (_: int32) = Param { Flags = ParamFlags(); ParamName = "" }

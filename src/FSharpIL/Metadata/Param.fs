namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type ParamFlags =
    { In: bool
      Out: bool
      Optional: bool }

    member this.Value =
        let mutable flags = ParameterAttributes.None
        if this.In then flags <- flags ||| ParameterAttributes.In
        if this.Out then flags <- flags ||| ParameterAttributes.Out
        if this.Optional then flags <- flags ||| ParameterAttributes.Optional
        flags

    interface IFlags<ParameterAttributes> with member this.Value = this.Value

    static member None = { In = false; Out = false; Optional = false }

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

/// <summary>Represents a <c>Param</c> item used in signatures (II.23.2.10).</summary>
[<IsReadOnly>]
type ParamItem =
    struct
        val CustomMod: ImmutableArray<CustomModifier>
        val internal Type: IEncodedType // ParamType
        internal new(modifiers, paramType) = { CustomMod = modifiers; Type = paramType }
        member internal this.CheckOwner owner =
            for modifier in this.CustomMod do modifier.CheckOwner owner
            this.Type.CheckOwner owner
    end

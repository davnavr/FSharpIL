namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

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
    end

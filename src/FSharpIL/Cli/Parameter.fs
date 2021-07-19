namespace FSharpIL.Cli

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ParameterType =
    | T of CliType
    | ByRef of modifiers: ImmutableArray<ModifierType> * CliType
    | TypedByRef of modifiers: ImmutableArray<ModifierType>

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption } // TODO: Have field that allows setting of Optional flag.

[<RequireQualifiedAccess>]
module ParameterType =
    let TypedByRef' = ParameterType.TypedByRef ImmutableArray.Empty

type ParameterList = int32 -> ParameterType -> Parameter

[<RequireQualifiedAccess>]
module Parameter =
    let flags (parameter: inref<_>) =
        let mutable value =
            match parameter.Kind with // TODO: Are In AND Out flags ever both set at the same time?
            | ParameterKind.Default -> ParamFlags.None
            | ParameterKind.InRef -> ParamFlags.In
            | ParameterKind.OutRef -> ParamFlags.Out

        // TODO: Set flag if parameter is Optional

        if parameter.DefaultValue.IsSome then value <- value ||| ParamFlags.HasDefault

        value

    let named name =
        { Kind = ParameterKind.Default
          DefaultValue = ValueNone
          ParamName = ValueSome name }

    let emptyList: ParameterList = fun _ _ -> Unchecked.defaultof<Parameter>

// TODO: Allow more efficient ways of generating parameter list
(*
type IParameterList = interface
    abstract GetParameter: index: int32 * parameterType: inref<ParamItem>
end

type ParameterList = delegate of int32 * inref<ParamItem> -> Parameter voption
*)

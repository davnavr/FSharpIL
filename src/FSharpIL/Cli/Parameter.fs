namespace FSharpIL.Cli

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli.Signatures

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption } // TODO: Have field that allows setting of Optional flag.

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

type ParameterList = int32 -> ParamItem -> Parameter

// TODO: Allow more efficient ways of generating parameter list
(*
type IParameterList = interface
    abstract GetParameter: index: int32 * parameterType: inref<ParamItem>
end

type ParameterList = delegate of int32 * inref<ParamItem> -> Parameter voption
*)

namespace FSharpIL.Writing.Abstractions

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables
open FSharpIL.Writing

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef
    | ByRef

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: ConstantOffset voption
      ParamName: Identifier voption
      Type: EncodedType } // TODO: How to prevent invalid parameter type when default value is specified?

[<RequireQualifiedAccess>]
module Parameter =
    let create name paramType =
        { Kind = ParameterKind.Default
          DefaultValue = ValueNone
          ParamName = ValueSome name
          Type = paramType }

    let row sequence (param: inref<Parameter>) (strings: StringsStreamBuilder) =
        let mutable flags =
            match param.Kind with
            | ParameterKind.Default -> ParamFlags.None
            | ParameterKind.InRef -> ParamFlags.In
            | ParameterKind.OutRef -> ParamFlags.Out
            | ParameterKind.ByRef -> ParamFlags.In ||| ParamFlags.Out
        if param.DefaultValue.IsSome then flags <- flags ||| ParamFlags.HasDefault
        { Flags = flags
          Sequence = sequence
          Name = strings.Add param.ParamName }

    let item (param: inref<Parameter>) =
        match param.Kind with
        | ParameterKind.Default -> ParamItem.Param(ImmutableArray.Empty, param.Type)
        | ParameterKind.InRef
        | ParameterKind.OutRef
        | ParameterKind.ByRef -> ParamItem.ByRef(ImmutableArray.Empty, param.Type)

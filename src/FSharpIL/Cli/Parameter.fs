namespace FSharpIL.Cli

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

[<IsReadOnly>]
type MethodParameterType = struct // TODO: Avoid code duplication with FSharpIL.Metadata.Signatures.ParamItem and MethodReturnType
    val Tag: FSharpIL.Metadata.Signatures.ParamItemTag
    val Type: NamedType voption

    internal new (tag, argType) = { Tag = tag; Type = argType }
end

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption } // TODO: Have field that allows setting of Optional flag.
      //ParamType: MethodParameterType // This will make ParameterList obsolete.

[<RequireQualifiedAccess>]
module MethodParameterType =
    let Type argType = MethodParameterType(FSharpIL.Metadata.Signatures.ParamItemTag.Param, ValueSome argType)
    let ByRef argType = MethodParameterType(FSharpIL.Metadata.Signatures.ParamItemTag.ByRef, ValueSome argType)
    let TypedByRef = MethodParameterType(FSharpIL.Metadata.Signatures.ParamItemTag.TypedByRef, ValueNone)

type ParameterList = int32 -> MethodParameterType -> Parameter

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

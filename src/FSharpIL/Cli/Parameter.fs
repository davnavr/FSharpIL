﻿namespace FSharpIL.Cli

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

[<IsReadOnly>]
type MethodParameterType = struct // TODO: Avoid code duplication with FSharpIL.Metadata.Signatures.ParamItem and MethodReturnType
    val Tag: ParamItemTag
    val CustomModifiers: ImmutableArray<ModifierType>
    val private argType: NamedType

    internal new (tag, modifiers, argType) = { Tag = tag; CustomModifiers = modifiers; argType = argType }

    member this.Type =
        if this.argType <> Unchecked.defaultof<_>
        then ValueSome this.argType
        else ValueNone
end

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption } // TODO: Have field that allows setting of Optional flag.

[<RequireQualifiedAccess>]
module MethodParameterType =
    let inline (|Type|ByRef|TypedByRef|) (ptype: MethodParameterType) =
        match ptype.Tag with
        | ParamItemTag.Param -> Type(ptype.Type.Value)
        | ParamItemTag.ByRef -> ByRef(struct(ptype.CustomModifiers, ptype.Type.Value))
        | ParamItemTag.TypedByRef
        | _ -> TypedByRef

    let Type argType = MethodParameterType(ParamItemTag.Param, ImmutableArray.Empty, argType)
    let ByRef(modifiers, argType) = MethodParameterType(ParamItemTag.ByRef, modifiers, argType)
    let TypedByRef modifiers = MethodParameterType(ParamItemTag.TypedByRef, modifiers, Unchecked.defaultof<NamedType>)
    let TypedByRef' = TypedByRef ImmutableArray.Empty

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

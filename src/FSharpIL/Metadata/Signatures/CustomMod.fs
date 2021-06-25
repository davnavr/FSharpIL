namespace FSharpIL.Metadata.Signatures

/// <summary>Represents a custom modifier (II.7.1.1 and II.23.2.7).</summary>
/// <typeparam name="ModifierType">
/// A <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c> that is the type of the custom modifier.
/// </typeparam>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CustomMod<'ModifierType> = { Required: bool; ModifierType: 'ModifierType }

(*
type CustomMod<'ModifierType> =
    | ModOpt of ModifierType: 'ModifierType
    | ModReq of ModifierType: 'ModifierType
*)

[<AutoOpen>]
module CustomModPatterns =
    let inline (|ModOpt|ModReq|) { Required = req; ModifierType = mtype } =
        if req then ModReq mtype else ModOpt mtype

    let inline ModOpt mtype = { Required = false; ModifierType = mtype }
    let inline ModReq mtype = { Required = true; ModifierType = mtype }

type CustomModifiers<'ModifierType> = System.Collections.Immutable.ImmutableArray<CustomMod<'ModifierType>>

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

type CustomModifiers<'ModifierType> = System.Collections.Immutable.ImmutableArray<CustomMod<'ModifierType>>

[<AutoOpen>]
module CustomModPatterns =
    let inline (|ModOpt|ModReq|) { Required = req; ModifierType = mtype } =
        if req then ModReq mtype else ModOpt mtype

    let inline (|NoRequiredModifiers|HasRequiredModifiers|) (modifiers: CustomModifiers<_>) =
        let mutable i, allOptionalModifiers = 0, true
        while allOptionalModifiers && i < modifiers.Length do
            allOptionalModifiers <- not (modifiers.ItemRef i).Required
            i <- i + 1
        if allOptionalModifiers then NoRequiredModifiers else HasRequiredModifiers

    let inline ModOpt mtype = { Required = false; ModifierType = mtype }
    let inline ModReq mtype = { Required = true; ModifierType = mtype }

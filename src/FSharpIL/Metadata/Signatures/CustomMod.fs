namespace FSharpIL.Metadata.Signatures

/// <summary>Represents an index into the <c>TypeDef</c>, <c>TypeRef</c> or <c>TypeSpec</c> table (II.23.2.8).</summary>
type TypeDefOrRefOrSpecEncoded = FSharpIL.Metadata.Tables.TypeDefOrRef

/// <summary>Represents a custom modifier (II.7.1.1 and II.23.2.7).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CustomMod = { Required: bool; ModifierType: TypeDefOrRefOrSpecEncoded }

(*
type CustomMod =
    | ModOpt of ModifierType: TypeDefOrRefOrSpecEncoded
    | ModReq of ModifierType: TypeDefOrRefOrSpecEncoded
*)

[<System.Obsolete>]
type CustomModifiers = System.Collections.Immutable.ImmutableArray<CustomMod>

[<AutoOpen>]
module CustomModPatterns =
    let inline (|ModOpt|ModReq|) { Required = req; ModifierType = mtype } =
        if req then ModReq mtype else ModOpt mtype

    let inline (|NoRequiredModifiers|HasRequiredModifiers|) modifiers =
        let rec inner =
            function
            | [] -> NoRequiredModifiers
            | { CustomMod.Required = true } :: _ -> HasRequiredModifiers
            | _ :: remaining -> inner remaining
        inner modifiers

    let inline ModOpt mtype = { Required = false; ModifierType = mtype }
    let inline ModReq mtype = { Required = true; ModifierType = mtype }

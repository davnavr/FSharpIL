namespace FSharpIL.Metadata.Signatures

open FSharpIL.Metadata.Tables

/// Represents a custom modifier (II.7.1.1 and II.23.2.7).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CustomMod = { Required: bool; ModifierType: TypeDefOrRef }

(*
type CustomMod =
    | ModOpt of ModifierType: TypeDefOrRef
    | ModReq of ModifierType: TypeDefOrRef
*)

[<AutoOpen>]
module CustomModPatterns =
    let inline (|ModOpt|ModReq|) { Required = req; ModifierType = mtype } =
        if req then ModReq mtype else ModOpt mtype

    let inline ModOpt mtype = { Required = false; ModifierType = mtype }
    let inline ModReq mtype = { Required = true; ModifierType = mtype }

type CustomModifiers = System.Collections.Immutable.ImmutableArray<CustomMod>

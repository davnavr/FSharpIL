namespace FSharpIL.Metadata.Signatures

open System.Collections.Immutable
open System.Runtime.CompilerServices

type LocalVariableTag =
    | TypedByRef = 0uy
    | Type = 1uy
    | ByRef = 2uy

/// <summary>Represents a <c>Constraint</c> item (II.23.2.9).</summary>
[<RequireQualifiedAccess>]
type Constraint =
    | Pinned

/// <summary>Represents a single local variable in a <c>LocalVarSig</c> item (II.23.2.6).</summary>
[<IsReadOnly; Struct>]
type LocalVariable<'TDefOrRef, 'TDefOrRefOrSpec> internal
    (
        modifiers: CustomModifiers<'TDefOrRefOrSpec>,
        constraints: Constraint list,
        tag: LocalVariableTag,
        ltype: EncodedType<'TDefOrRef, 'TDefOrRefOrSpec> voption
    )
    =
    member _.CustomMod = modifiers
    member _.Constraints = constraints
    member _.Tag = tag
    member _.Type = ltype
    static member val TypedByRef =
        LocalVariable<'TDefOrRef, 'TDefOrRefOrSpec>(ImmutableArray.Empty, List.empty, LocalVariableTag.TypedByRef, ValueNone)

(*
[<RequireQualifiedAccess>]
[<IsReadOnly; Struct>]
type LocalVariable =
    | Local of CustomMod: CustomModifiers * Constraints: Constraint list * Type: EncodedType
    | ByRef of CustomMod: CustomModifiers * Constraints: Constraint list * Type: EncodedType
    | TypedByRef
*)

[<RequireQualifiedAccess>]
module LocalVariable =
    let inline (|Local|ByRef|TypedByRef|) (local: LocalVariable<_, _>) =
        let inline info() = struct(local.CustomMod, local.Constraints, local.Type.Value)
        match local.Tag with
        | LocalVariableTag.TypedByRef -> TypedByRef
        | LocalVariableTag.ByRef -> ByRef(info())
        | LocalVariableTag.Type
        | _ -> Local(info())

    let Local(modifiers, constraints, localType) =
        LocalVariable(modifiers, constraints, LocalVariableTag.Type, ValueSome localType)

    let ByRef(modifiers, constraints, localType) =
        LocalVariable(modifiers, constraints, LocalVariableTag.ByRef, ValueSome localType)

/// <summary>
/// Represents a <c>LocalVarSig</c> item, which describes the types of all of the local variables of a method (II.23.2.6).
/// </summary>
type LocalVarSig<'TDefOrRef, 'TDefOrRefOrSpec> = ImmutableArray<LocalVariable<'TDefOrRef, 'TDefOrRefOrSpec>>

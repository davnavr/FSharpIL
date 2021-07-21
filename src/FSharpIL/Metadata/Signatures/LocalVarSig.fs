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
    /// Indicates that the value pointed to by this local variable should not be moved "by the actions of garbage collection"
    /// (II.23.2.9).
    | Pinned

/// <summary>Represents a single local variable in a <c>LocalVarSig</c> item (II.23.2.6).</summary>
[<IsReadOnly; Struct>]
type LocalVariable internal
    (
        modifiers: CustomMod list,
        constraints: Constraint list,
        tag: LocalVariableTag,
        ltype: EncodedType voption
    )
    =
    member _.CustomMod = modifiers
    member _.Constraints = constraints
    member _.Tag = tag
    member _.Type = ltype

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
    /// <summary>A constraint list containing the currently only kind of constraint, <c>PINNED</c> (II.23.2.9).</summary>
    let pinned = [ Constraint.Pinned ]

    let inline (|Type|ByRef|TypedByRef|) (local: LocalVariable) =
        let inline info() = struct(local.CustomMod, local.Constraints, local.Type.Value)
        match local.Tag with
        | LocalVariableTag.TypedByRef -> TypedByRef
        | LocalVariableTag.ByRef -> ByRef(info())
        | LocalVariableTag.Type
        | _ -> Type(info())

    let Type(modifiers, constraints, localType) =
        LocalVariable(modifiers, constraints, LocalVariableTag.Type, ValueSome localType)

    let ByRef(modifiers, constraints, localType) =
        LocalVariable(modifiers, constraints, LocalVariableTag.ByRef, ValueSome localType)

    let TypedByRef modifiers = LocalVariable(modifiers, List.empty, LocalVariableTag.TypedByRef, ValueNone)

/// <summary>
/// Represents a <c>LocalVarSig</c> item, which describes the types of all of the local variables of a method (II.23.2.6).
/// </summary>
type LocalVarSig = ImmutableArray<LocalVariable>

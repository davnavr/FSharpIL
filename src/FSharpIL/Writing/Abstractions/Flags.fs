namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

type [<IsReadOnly; Struct>] Serializable = | Serializable | NotSerializable
type [<IsReadOnly; Struct>] StringFormatting = | AnsiClass | UnicodeClass | AutoClass

[<IsReadOnly; Struct>]
type VTableLayout =
    /// Default value used by most methods.
    | ReuseSlot
    | NewSlot

[<IsReadOnly; Struct; RequireQualifiedAccess>]
type CheckAccessOnOverride = | Strict | NotStrict

[<RequireQualifiedAccess>]
module Flags =
    let serializable =
        function
        | Serializable -> TypeDefFlags.Serializable
        | NotSerializable -> Unchecked.defaultof<_>

    let stringFormatting format =
        match format with
        | AnsiClass -> TypeDefFlags.AnsiClass
        | UnicodeClass -> TypeDefFlags.UnicodeClass
        | AutoClass -> TypeDefFlags.AutoClass

    let vTableLayout layout =
        match layout with
        | ReuseSlot -> MethodDefFlags.ReuseSlot
        | NewSlot -> MethodDefFlags.NewSlot

    let strict =
        function
        | CheckAccessOnOverride.Strict -> MethodDefFlags.Strict
        | CheckAccessOnOverride.NotStrict -> Unchecked.defaultof<_>

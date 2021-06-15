namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing

[<IsReadOnly>]
type EntryPointKind = struct
    /// Gets a value indicating whether or not the entry point method returns an integer exit code.
    val HasExitCode: bool
    val ArgumentsParamName: Identifier option
    internal new (hasExitCode, argsParamName) = { HasExitCode = hasExitCode; ArgumentsParamName = argsParamName }
end

(*
[<IsReadOnly; Struct>]
type EntryPointKind =
    | ExitWithArgs of Arguments: Identifier
    | VoidWithArgs of Arguments: Identifier
    | ExitNoArgs
    | VoidNoArgs
*)

[<AutoOpen>]
module EntryPointKind =
    let (|ExitWithArgs|VoidWithArgs|ExitNoArgs|VoidNoArgs|) (kind: EntryPointKind) =
        match kind.HasExitCode, kind.ArgumentsParamName with
        | true, Some argsParamName -> ExitWithArgs argsParamName
        | false, Some argsParamName -> VoidWithArgs argsParamName
        | true, None -> ExitNoArgs
        | false, None -> VoidNoArgs

    /// An entry point method that takes an array of string arguments and returns an integer exit code.
    let ExitWithArgs argsParamName = EntryPointKind(true, Some argsParamName)
    /// An entry point method that takes an array of string arguments and returns nothing.
    let VoidWithArgs argsParamName = EntryPointKind(false, Some argsParamName)
    /// An entry point method that takes no arguments and returns an integer exit code.
    let ExitNoArgs = EntryPointKind(true, None)
    /// An entry point method that takes no arguments and returns nothing.
    let VoidNoArgs = EntryPointKind(false, None)

[<IsReadOnly>]
type EntryPointDef = struct
    val Visibility: MemberVisibility
    val MethodName: Identifier
    val Kind: EntryPointKind
    val Body: MethodBodyLocation
    new (visibility, name, kind, body) =
        { Visibility = visibility
          MethodName = name
          Kind = kind
          Body = body }
end

[<IsReadOnly>]
type EntryPointFile = struct
    val Name: Identifier
    val HashValue: BlobOffset
    new (name, hash) = { Name = name; HashValue = hash }
end

[<RequireQualifiedAccess>]
type ValidEntryPoint =
    | Method of EntryPointDef
    | File of EntryPointFile

[<RequireQualifiedAccess>]
module ValidEntryPoint =
    ()

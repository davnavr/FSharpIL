/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings, throwing an exception on any errors.
/// </summary>
[<RequireQualifiedAccess; System.Obsolete>]
module FSharpIL.Metadata.UncheckedExn

open System

[<Obsolete>]
/// Helper function for throwing exceptions when validation fails.
let inline throwOnError (result: Result<_, ValidationError>) =
    match result with
    | Ok item -> item
    | Error err -> raise(ValidationErrorException err)

[<RequireQualifiedAccess>]
module GenericParam =
    let addNonvariant builder flags owner name constraints =
        Unchecked.GenericParam.addNonvariant builder flags owner name constraints |> throwOnError

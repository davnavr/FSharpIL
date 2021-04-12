/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings.
/// </summary>
[<RequireQualifiedAccess; System.Obsolete>]
module FSharpIL.Metadata.Unchecked // TODO: Move GenericParam module to its own file.

// TODO: Move functions for manipulating generic parameters to Unsafe.
[<RequireQualifiedAccess>]
module GenericParam =
    let addNonvariant (builder: CliMetadataBuilder) flags owner name constraints =
        let result = builder.GenericParam.TryAddNonvariant(flags, owner, name, constraints)
        match result with
        | ValueSome info -> Ok info
        | ValueNone -> DuplicateGenericParamError(owner ,name).ToResult()

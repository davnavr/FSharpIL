[<AutoOpen>]
module FSharpIL.Builders

// TODO: Move to FSharpIL.Metadata namespace.
// TODO: Rename to metadata
/// Builds .NET metadata using a computation expression.
let metadataBuilder mdle = FSharpIL.Metadata.MetadataBuilder mdle

/// Builds a .NET assembly using a high-level computation expression syntax.
let assembly = null

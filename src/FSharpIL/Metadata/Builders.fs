[<AutoOpen>]
module FSharpIL.Metadata.Builders

/// <summary>
/// Builds .NET metadata using computation expression syntax.
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.CliMetadata"/>
let metadata = CliMetadataBuilder()

/// <summary>Computation Expression used for building the methods of a <see cref="T:FSharpIL.Metadata.TypeDef"/>.</summary>
[<GeneralizableValue>]
let methods<'Method when 'Method :> IMethod> = MemberListBuilder<'Method, _> (fun mthd -> mthd.Definition())

/// <summary>Computation Expression used for building the fields of a <see cref="T:FSharpIL.Metadata.TypeDef"/>.</summary>
[<GeneralizableValue>]
let fields<'Field when 'Field :> IField> = MemberListBuilder<'Field, _> (fun field -> field.Row())

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.EntryPoint

/// <summary>Sets the entrypoint of the assembly.</summary>
let setToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
let set builder main = setToken builder (EntryPointToken.ValidEntryPoint main)

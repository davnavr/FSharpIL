[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.EntryPoint

/// <summary>Sets the entrypoint of the assembly.</summary>
let inline setToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
let inline set builder main = setToken builder (EntryPointToken.ValidEntryPoint main)

let inline addRow builder owner (entryPoint: EntryPointMethod) =
    let row = entryPoint.Definition()
    Unsafe.tryAddMethodDefRow builder owner &row

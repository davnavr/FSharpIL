[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.EntryPoint

let methodIndex (method: RawIndex<EntryPointMethod>) = method.ChangeTag<MethodDefRow>()

/// <summary>Sets the entrypoint of the assembly.</summary>
let inline setToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
let inline set builder main = setToken builder (EntryPointToken.ValidEntryPoint main)

// TODO: Make tryAddRow for EntryPoint use inref.
let tryAddRow builder owner (entryPoint: EntryPointMethod) =
    entryPoint.Definition() |> Unsafe.tryAddMethodDefRow builder owner

let inline addRow builder owner entryPoint = tryAddRow builder owner entryPoint |> ValidationError.check

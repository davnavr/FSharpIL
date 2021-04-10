[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.AssemblyRef

let addRow (builder: CliMetadataBuilder) (assembly: inref<AssemblyRef>) =
    let mutable dup = false
    let i = builder.AssemblyRef.Add(&assembly, &dup)
    struct(i, dup)

let inline addReflectedRow (builder: CliMetadataBuilder) (assembly: System.Reflection.Assembly) =
    let name = assembly.GetName()
    let assem =
        AssemblyRef (
            name.Version,
            AssemblyName.ofStr name.Name,
            invalidOp "get public key token of assembly",
            CustomCulture(Identifier.ofStr name.CultureName),
            invalidOp "get hash value of assembly if available"
        )
    addRow builder &assem

let addRowChecked builder assembly (warnings: WarningsBuilder) =
    let struct (i, duplicate) = addRow builder &assembly
    if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
    i

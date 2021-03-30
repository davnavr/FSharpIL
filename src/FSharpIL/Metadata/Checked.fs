/// Contains functions for modifying the CLI metadata with CLS checks and warnings.
module FSharpIL.Metadata.Checked

let referenceModule (builder: CliMetadataBuilder) moduleRef (warnings: WarningsBuilder) =
    let i, dup = Unchecked.referenceModule builder moduleRef
    if dup then warnings.Add(DuplicateModuleRefWarning moduleRef)
    i

let referenceAssembly (builder: CliMetadataBuilder) assembly (warnings: WarningsBuilder) =
    let struct(i, duplicate) = Unchecked.referenceAssembly builder assembly
    if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
    i

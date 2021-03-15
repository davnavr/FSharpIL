/// Contains functions for modifying the CLI metadata with CLS checks and warnings.
module FSharpIL.Metadata.Checked

let private referenceMethod (builder: CliMetadataBuilder) (row: MemberRefRow) (warnings: WarningsBuilder) =
    let struct(i, duplicate) = Unchecked.referenceMethod builder row
    if duplicate then warnings.Add(DuplicateMemberRefWarning row)
    i

/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let referenceDefaultMethod builder method warnings: MemberRefIndex<MethodRefDefault> =
    referenceMethod builder (MethodRefDefault method) warnings

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGenericMethod builder method warnings: MemberRefIndex<MethodRefGeneric> =
    referenceMethod builder (MethodRefGeneric method) warnings

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArgMethod builder method warnings: MemberRefIndex<MethodRefVarArg> =
    referenceMethod builder (MethodRefVarArg method) warnings

let referenceModule (builder: CliMetadataBuilder) moduleRef (warnings: WarningsBuilder) =
    let i, dup = Unchecked.referenceModule builder moduleRef
    if dup then warnings.Add(DuplicateModuleRefWarning moduleRef)
    i

let referenceAssembly (builder: CliMetadataBuilder) assembly (warnings: WarningsBuilder) =
    let i, duplicate = Unchecked.referenceAssembly builder assembly
    if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
    i

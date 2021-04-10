/// Contains functions for modifying the CLI metadata with CLS checks and warnings.
[<RequireQualifiedAccess; System.Obsolete>]
module FSharpIL.Metadata.Checked

[<System.ObsoleteAttribute>]
let referenceModule (builder: CliMetadataBuilder) moduleRef (warnings: WarningsBuilder) =
    let i, dup = Unchecked.referenceModule builder moduleRef
    if dup then warnings.Add(DuplicateModuleRefWarning moduleRef)
    i
[<System.ObsoleteAttribute>]
let referenceAssembly (builder: CliMetadataBuilder) assembly (warnings: WarningsBuilder) =
    let struct(i, duplicate) = Unchecked.referenceAssembly builder assembly
    if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
    i
[<System.ObsoleteAttribute>]
let private referenceMethod (row: MemberRef<_>) struct(i, duplicate) (warnings: WarningsBuilder) =
    if duplicate then warnings.Add(DuplicateMemberRefWarning(row.Class, row.MemberName))
    i
[<System.ObsoleteAttribute>]
/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let referenceDefaultMethod (builder: CliMetadataBuilder) (method: MethodRefDefault) warnings =
    referenceMethod method (builder.MemberRef.Add &method) warnings
[<System.ObsoleteAttribute>]
/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGenericMethod (builder: CliMetadataBuilder) (method: MethodRefGeneric) warnings =
    referenceMethod method (builder.MemberRef.Add &method) warnings
[<System.ObsoleteAttribute>]
/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArgMethod  (builder: CliMetadataBuilder) (method: MethodRefVarArg) warnings =
    referenceMethod method (builder.MemberRef.Add &method) warnings

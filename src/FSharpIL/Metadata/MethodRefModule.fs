[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.MethodRef

/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let inline addRowDefault (builder: CliMetadataBuilder) (method: MethodRefDefault) =
    builder.MemberRef.Add &method

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let inline addRowGeneric (builder: CliMetadataBuilder) (method: MethodRefGeneric): struct(RawIndex<MethodRefGeneric> * _) =
    builder.MemberRef.Add &method

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let inline addRowVarArg (builder: CliMetadataBuilder) (method: MethodRefVarArg): struct(RawIndex<MethodRefVarArg> * _) =
    builder.MemberRef.Add &method

// TODO: Add methodref-adding function variants that don't use inref.
// TODO: Add variants that have checks.
let inline private check (row: inref<MemberRef<_>>) struct(i, duplicate) (warnings: WarningsBuilder) =
    if duplicate then warnings.Add(DuplicateMemberRefWarning(row.Class, row.MemberName))
    i

let inline addRowDefaultChecked builder method warnings = check &method (addRowDefault builder method) warnings
let inline addRowGenericChecked builder method warnings = check &method (addRowGeneric builder method) warnings
let inline addRowVarArgChecked builder method warnings = check &method (addRowVarArg builder method) warnings

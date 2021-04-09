[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.MethodRef

/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let addRowDefault (builder: CliMetadataBuilder) (method: inref<MethodRefDefault>) =
    builder.MemberRef.Add &method

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let addRowGeneric (builder: CliMetadataBuilder) (method: inref<MethodRefGeneric>): struct(RawIndex<MethodRefGeneric> * _) =
    builder.MemberRef.Add &method

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let addRowVarArg (builder: CliMetadataBuilder) (method: inref<MethodRefVarArg>): struct(RawIndex<MethodRefVarArg> * _) =
    builder.MemberRef.Add &method

// TODO: Add methodref-adding function variants that don't use inref.

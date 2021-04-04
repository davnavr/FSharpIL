[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.MethodRef

/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let referenceDefault (builder: CliMetadataBuilder) (method: MethodRefDefault) =
    builder.MemberRef.Add method

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGeneric (builder: CliMetadataBuilder) (method: MethodRefGeneric): struct(RawIndex<MethodRefGeneric> * _) =
    builder.MemberRef.Add method

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArg (builder: CliMetadataBuilder) (method: MethodRefVarArg): struct(RawIndex<MethodRefVarArg> * _) =
    builder.MemberRef.Add method

// TODO: Add methodref-adding function variants that use inref.

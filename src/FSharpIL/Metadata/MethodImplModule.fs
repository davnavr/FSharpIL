[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.MethodImpl

let add (builder: CliMetadataBuilder) owner body decl=
    if builder.MethodImpl.TryAdd(owner, body, decl)
    then ()
    else invalidOp "Duplicate MethodImpl row detected."

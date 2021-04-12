[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.FieldRef

let inline addRow (builder: CliMetadataBuilder) (field: FieldRef) =
    builder.MemberRef.Add &field

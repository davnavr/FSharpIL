[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.File

let inline addRow (builder: CliMetadataBuilder) (file: inref<File>) = builder.File.Add file

let inline createFile builder isMetadata hashValue name  =
    let file = { ContainsMetadata = isMetadata; FileName = name; HashValue = hashValue }
    addRow builder &file

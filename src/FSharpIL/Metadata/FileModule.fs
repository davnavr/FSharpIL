[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.File

let inline addRow (builder: CliMetadataBuilder) (file: inref<File>) =
    let mutable duplicate = false
    struct(builder.File.Add(&file, &duplicate), duplicate)

let inline createRow builder isMetadata hashValue name  =
    let file = { ContainsMetadata = isMetadata; FileName = name; HashValue = hashValue }
    addRow builder &file

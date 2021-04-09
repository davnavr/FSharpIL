[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.ModuleRef

let inline addRow (builder: CliMetadataBuilder) moduleRef = builder.ModuleRef.Add moduleRef

let inline createRow (builder: CliMetadataBuilder) hashValue moduleName =
    let file, _ = File.createFile builder true hashValue moduleName
    addRow builder { ModuleRef.File = file }

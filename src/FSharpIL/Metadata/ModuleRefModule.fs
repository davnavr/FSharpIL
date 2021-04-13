[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.ModuleRef

let inline addRow (builder: CliMetadataBuilder) (moduleRef: inref<_>) = builder.ModuleRef.Add &moduleRef

let inline createRow (builder: CliMetadataBuilder) hashValue moduleName =
    let struct (file, _) = File.createRow builder true hashValue moduleName
    let mdle = { ModuleRef.File = file }
    addRow builder &mdle

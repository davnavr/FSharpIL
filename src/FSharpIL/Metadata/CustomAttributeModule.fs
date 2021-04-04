[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.CustomAttribute

let addRef (builder: CliMetadataBuilder) (attribute: inref<_>) = builder.CustomAttribute.Add &attribute
let add builder (attribute: CustomAttribute) = addRef builder &attribute

let addTo builder parent ctor value =
    let attribute = { Parent = parent; Type = ctor; Value = value }
    addRef builder &attribute

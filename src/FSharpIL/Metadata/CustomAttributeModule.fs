[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.CustomAttribute

/// <summary>Adds a row to the <c>CustomAttribute</c> table (II.22.10).</summary>
let inline addRow (builder: CliMetadataBuilder) (attribute: inref<_>) = builder.CustomAttribute.Add &attribute

let inline createRow builder parent ctor value =
    let attribute = { Parent = parent; Type = ctor; Value = value }
    addRow builder &attribute

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.TypeSpec

open FSharpIL

let addBlob (builder: CliMetadataBuilder) spec =
    builder.Blobs.TypeSpec.TryAdd spec |> Result.any

let inline tryAddRow (builder: CliMetadataBuilder) (spec: TypeSpecRow) =
    match builder.TypeSpec.TryAdd &spec with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateTypeSpecError(spec).ToResult()

let inline addRow builder spec = tryAddRow builder spec |> ValidationError.check

let inline tryCreateRow builder spec = addBlob builder spec |> TypeSpecRow |> tryAddRow builder

let inline createRow builder spec = tryCreateRow builder spec |> ValidationError.check

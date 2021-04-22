[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.MethodSpec

open FSharpIL

let addBlob (builder: CliMetadataBuilder) (spec: inref<MethodSpec>) =
    builder.Blobs.MethodSpec.TryAdd &spec |> Result.any

let inline createBlob builder (garguments: seq<_>) =
    let spec = MethodSpec garguments
    addBlob builder &spec

let inline tryAddRow (builder: CliMetadataBuilder) (spec: inref<MethodSpecRow>) =
    match builder.MethodSpec.TryAdd &spec with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateMethodSpecError(spec).ToResult()

let inline tryCreateRow (builder: CliMetadataBuilder) method (garguments: seq<_>) =
    let spec' = MethodSpecRow(method, createBlob builder garguments)
    tryAddRow builder &spec'

let inline addRow builder (spec: inref<_>) = tryAddRow builder &spec |> ValidationError.check
let inline createRow builder method garguments = tryCreateRow builder method garguments |> ValidationError.check

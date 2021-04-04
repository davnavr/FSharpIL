[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.TypeRef

let tryAddRef (builder: CliMetadataBuilder) (typeRef: inref<_>) =
    match builder.TypeRef.TryAdd &typeRef with
    | ValueSome i -> Ok i
    | ValueNone -> DuplicateTypeRefError(typeRef).ToResult()

let addRef builder (typeRef: inref<TypeRef>) = tryAddRef builder &typeRef |> ValidationError.check

// TODO: Add variant of TypeRef.add with warning/cls checks.
//let tryAddChecked

let tryAdd builder (typeRef: TypeRef) = tryAddRef builder &typeRef

let add builder (typeRef: TypeRef) = addRef builder &typeRef

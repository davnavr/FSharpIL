[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.TypeRef

let inline tryAddRow (builder: CliMetadataBuilder) typeRef =
    match builder.TypeRef.TryAdd &typeRef with
    | ValueSome i -> Ok i
    | ValueNone -> DuplicateTypeRefError(typeRef).ToResult()

let inline addRow builder typeRef = tryAddRow builder typeRef |> ValidationError.check

let inline tryCreateRow builder resolutionScope typeName typeNamespace =
    TypeRef(resolutionScope, typeName, typeNamespace) |> tryAddRow builder

let inline createRow builder resolutionScope typeName typeNamespace =
    tryCreateRow builder resolutionScope typeName typeNamespace |> ValidationError.check

let inline tryAddRowChecked (builder: CliMetadataBuilder) (typeRef: TypeRef) (warnings: WarningsBuilder) =
    match tryAddRow builder typeRef with
    | Ok i ->
        if typeRef.ResolutionScope.Tag = ResolutionScopeTag.Module then
            warnings.Add(TypeRefUsesModuleResolutionScope typeRef)
        Ok i
    | err -> err

let inline ofReflectedType resolutionScope (typeRef: System.Type) =
    if typeRef.IsGenericTypeParameter then invalidArg "typeRef" "Cannot reference a generic type parameter"
    // TODO: Check to ensure that the type is not a closed constructed type (ex: List<int> is invalid).
    TypeRef(resolutionScope, Identifier.ofStr typeRef.Name, typeRef.Namespace)

let inline tryCreateReflectedRow builder resolutionScope typeRef = ofReflectedType resolutionScope typeRef |> tryAddRow builder

let inline createReflectedRow builder resolutionScope typeRef =
    tryCreateReflectedRow builder resolutionScope typeRef |> ValidationError.check

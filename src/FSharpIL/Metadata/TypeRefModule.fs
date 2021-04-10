[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.TypeRef

let inline tryAddRow (builder: CliMetadataBuilder) (typeRef: inref<TypeRef>) =
    match builder.TypeRef.TryAdd &typeRef with
    | ValueSome i -> Ok i
    | ValueNone -> DuplicateTypeRefError(typeRef).ToResult()

let inline addRef builder (typeRef: inref<TypeRef>) = tryAddRow builder &typeRef |> ValidationError.check

let tryAddRowChecked (builder: CliMetadataBuilder) (typeRef: TypeRef) (warnings: WarningsBuilder) =
    match tryAddRow builder &typeRef with
    | Ok i ->
        if typeRef.ResolutionScope.Tag = ResolutionScopeTag.Module then
            warnings.Add(TypeRefUsesModuleResolutionScope typeRef)
        Ok i
    | err -> err

let inline ofReflectedType resolutionScope (typeRef: System.Type) =
    if typeRef.IsGenericTypeParameter then invalidArg "typeRef" "Cannot reference a generic type parameter"
    TypeRef(resolutionScope, Identifier.ofStr typeRef.Name, typeRef.Namespace)

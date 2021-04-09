[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.TypeRef

let tryAddRowChecked (builder: CliMetadataBuilder) warnings (typeRef: inref<TypeRef>) =
    match builder.TypeRef.TryAdd &typeRef with
    | ValueSome i ->
        match warnings with
        | ValueSome(warnings': WarningsBuilder) ->
            if typeRef.ResolutionScope.Tag = ResolutionScopeTag.Module then
                warnings'.Add(TypeRefUsesModuleResolutionScope typeRef)
        | ValueNone -> ()
        Ok i
    | ValueNone -> DuplicateTypeRefError(typeRef).ToResult()

let inline tryAddRow builder (typeRef: inref<TypeRef>) = tryAddRowChecked builder ValueNone &typeRef

let inline addRef builder (typeRef: inref<TypeRef>) = tryAddRow builder &typeRef |> ValidationError.check

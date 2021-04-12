[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.GenericParam

let tryAddRow (builder: CliMetadataBuilder) owner (parameter: inref<GenericParam>) =
    match builder.GenericParam.TryAdd(owner, &parameter) with
    | ValueSome result -> Ok result
    | ValueNone -> DuplicateGenericParamError(owner, parameter.Name).ToResult()

let inline addRow builder owner parameter = tryAddRow builder owner &parameter |> ValidationError.check

let tryCreateRow builder owner flags name variance constraints =
    let parameter = GenericParam(flags, name, variance, constraints)
    tryAddRow builder owner &parameter

let inline createRow builder owner flags name variance constraints =
    tryCreateRow builder owner flags name variance constraints |> ValidationError.check

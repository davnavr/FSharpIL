[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module FSharpIL.Metadata.GenericParam

let tryAddInvariantRow (builder: CliMetadataBuilder) owner (parameter: inref<InvariantGenericParam>) =
    match builder.GenericParam.TryAdd(owner, &parameter) with
    | ValueSome result -> Ok result
    | ValueNone -> DuplicateGenericParamError(owner, parameter.Name).ToResult()

let inline addInvariantRow builder owner parameter = tryAddInvariantRow builder owner &parameter |> ValidationError.check

let inline tryCreateInvariantRow builder owner flags name constraints =
    let parameter = InvariantGenericParam(flags, name, constraints)
    tryAddInvariantRow builder owner &parameter

let inline createInvariantRow builder owner flags name constraints =
    tryCreateInvariantRow builder owner flags name constraints |> ValidationError.check

// TODO: Figure out if normal TypeDefs can have Covariant type parameters, since it might inherit it from an interface.

//let addRow

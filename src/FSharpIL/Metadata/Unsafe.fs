/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Unsafe

let changeIndexTag<'From, 'To> (index: RawIndex<'From>) = index.ChangeTag<'To>()

let tryAddTypeDefRow<'Tag> (builder: CliMetadataBuilder) flags typeName typeNamespace extends parent =
    let row =
        TypeDefRow (
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        )
    match builder.TypeDef.TryAdd &row with
    | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
    | ValueNone -> DuplicateTypeDefError(row).ToResult()

let tryAddMethodDefRow<'Tag> (builder: CliMetadataBuilder) owner (method: inref<_>) =
    match builder.Method.TryAdd(owner, method) with
    | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
    | ValueNone -> DuplicateMethodError(method).ToResult()

let tryCreateMethodDefRow<'Tag> builder owner body implFlags methodFlags name signature paramList =
    let method =
        MethodDefRow (
            body,
            implFlags,
            methodFlags,
            name,
            signature,
            paramList
        )
    tryAddMethodDefRow<'Tag> builder owner &method

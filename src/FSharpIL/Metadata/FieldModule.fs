namespace FSharpIL.Metadata

[<RequireQualifiedAccess>]
module internal Field =
    let inline tryAddRow (builder: CliMetadataBuilder) owner (field: Field<'Flags>) =
        field.Row() |> Unsafe.tryAddFieldRow<Field<'Flags>> builder owner

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticField =
    let fieldIndex (field: RawIndex<StaticField>) = field.ChangeTag<FieldRow>()
    let inline tryAddRow builder (StaticMemberParent owner) (field: inref<StaticField>): Result<RawIndex<StaticField>, _> =
        Field.tryAddRow builder owner field
    let inline addRow builder owner field = tryAddRow builder owner &field |> ValidationError.check

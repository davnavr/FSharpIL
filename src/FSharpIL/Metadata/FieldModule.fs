namespace FSharpIL.Metadata

// TODO: To avoid forcing tryAddRow functions of Field and Methods to use inref, maybe make an public function in the Unsafe module that retrieves the Row objects?

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

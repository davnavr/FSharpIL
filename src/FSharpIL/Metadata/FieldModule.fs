﻿namespace FSharpIL.Metadata

// TODO: To avoid forcing tryAddRow functions of Field and Methods to use inref, maybe make an public function in the Unsafe module that retrieves the Row objects?

[<RequireQualifiedAccess>]
module internal Field =
    let inline tryAddRow (builder: CliMetadataBuilder) owner (field: Field<'Flags, 'Tag>) =
        field.Row() |> Unsafe.tryAddFieldRow<Field<'Flags, 'Tag>> builder owner

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticField =
    let fieldIndex (field: RawIndex<StaticField>) = field.ChangeTag<FieldRow>()
    let tryAddRow builder (StaticMemberOwner owner) (field: inref<StaticField>): Result<RawIndex<StaticField>, _> =
        Field.tryAddRow builder owner field
    let inline addRow builder owner field = tryAddRow builder owner &field |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module InstanceField =
    let fieldIndex (field: RawIndex<InstanceField>) = field.ChangeTag<FieldRow>()
    let tryAddRow builder (InstanceMemberOwner owner) (field: inref<InstanceField>): Result<RawIndex<InstanceField>, _> =
        Field.tryAddRow builder owner field
    let inline addRow builder owner field = tryAddRow builder owner &field |> ValidationError.check

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module LiteralField =
    let fieldIndex (field: RawIndex<LiteralField>) = field.ChangeTag<FieldRow>()
    let tryAddRow builder (StaticMemberOwner owner) (field: inref<LiteralField>) value: Result<RawIndex<LiteralField>, _> =
        match Field.tryAddRow builder owner field with
        | Ok i as ok ->
            match builder.Constant.TryAdd(fieldIndex i, value) with
            | ValueSome _ -> ok
            | ValueNone -> sprintf "Unable to add constant value to field %A" field |> invalidOp
        | err -> err
    let inline addRow builder owner value field = tryAddRow builder owner &field value |> ValidationError.check

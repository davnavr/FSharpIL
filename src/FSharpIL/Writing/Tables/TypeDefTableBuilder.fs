namespace FSharpIL.Writing.Tables

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when an invalid combination of <c>TypeAttributes</c> is used (2b, 2c, 27).</summary>
/// <category>Errors</category>
type InvalidTypeAttributesCombination = InvalidFlagsCombination<TypeDefFlags>

/// <summary>Error used when an interface is marked <c>Abstract</c> (23).</summary>
/// <category>Errors</category>
type InterfaceMustBeAbstract () =
    override _.ToString() = "Interface types must be marked abstract"
    interface IValidationError

/// <summary>Error used when an interface extends another type (13).</summary>
/// <category>Errors</category>
type InterfaceCannotExtendType (extends: TypeDefOrRef) =
    member _.Extends = extends
    override _.ToString() = sprintf "Cannot extend %O, interface types are not allowed to extend other types" extends
    interface IValidationError

[<Sealed>]
type TypeDefTableBuilder internal () =
    let rows = RowList<TypeDefRow>() // TODO: Figure out how to prevent duplicate rows, since nested types make it impossible right now.

    member _.TryAddRow(row: inref<TypeDefRow>) =
        if
            Flags.set (TypeDefFlags.SequentialLayout ||| TypeDefFlags.ExplicitLayout) row.Flags // 2b
            || Flags.set (TypeDefFlags.UnicodeClass ||| TypeDefFlags.AutoClass) row.Flags // 2c
            || Flags.set (TypeDefFlags.Interface ||| TypeDefFlags.Sealed) row.Flags// 27
        then
            ValidationResult.failure(InvalidTypeAttributesCombination row.Flags)
        elif row.IsInterface && not(Flags.set TypeDefFlags.Abstract row.Flags) then
            ValidationResult.failure(InterfaceMustBeAbstract())
        elif row.IsInterface && not row.Extends.IsNull then
            ValidationResult.failure(InterfaceCannotExtendType row.Extends)
        else Ok(rows.Add &row)

    interface ITableBuilder<TypeDefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE(uint32 row.Flags)
            StreamOffset.writeString &wr hsizes row.TypeName
            StreamOffset.writeString &wr hsizes row.TypeNamespace
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Extends
            TableIndex.write &wr tsizes ValidTableFlags.Field row.FieldList
            TableIndex.write &wr tsizes ValidTableFlags.Field row.MethodList

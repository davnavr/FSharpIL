namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when a duplicate row is added to the <c>GenericParamConstraint</c> table (7).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateGenericParamConstraintRow =
    { Row: GenericParamConstraintRow }
    override this.ToString() = sprintf "An event with the same owner %O and constraint %O" this.Row.Owner this.Row.Constraint
    interface IValidationError

[<Struct>]
type private GenericParamConstraintRowValidator =
    interface IRowRangeValidator<GenericParamConstraintRow> with
        member _.Validate _ = None
        member _.Duplicate row = { DuplicateGenericParamConstraintRow.Row = row } :> IValidationError // 7

[<Sealed>]
type GenericParamConstraintTableBuilder internal () =
    let rows = RangedRowList<GenericParamConstraintRow, GenericParamConstraintRowValidator>()
    interface ITableBuilder<GenericParamConstraintRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.GenericParam row.Owner
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Constraint

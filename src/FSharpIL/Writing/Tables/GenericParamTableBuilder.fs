namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when a duplicate row is added to the <c>GenericParam</c> table (11).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateGenericParamRow =
    { Row: GenericParamRow }
    override this.ToString() =
        sprintf
            "An event with the same owner %O and name %O or number %i already exists"
            this.Row.Owner
            this.Row.Name
            this.Row.Number
    interface IValidationError

[<Struct>]
type private GenericParamRowValidator =
    interface IRowRangeValidator<GenericParamRow> with
        member _.Validate _ = None
        member _.Duplicate row = { DuplicateGenericParamRow.Row = row } :> IValidationError // 11

[<Sealed>]
type GenericParamTableBuilder internal () =
    let rows = RangedRowList<GenericParamRow, GenericParamRowValidator>()
    interface ITableBuilder<GenericParamRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE row.Number
            wr.WriteLE(uint16 row.Flags)
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeOrMethodDef row.Owner
            StreamOffset.writeString &wr hsizes row.Name

namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when a duplicate row is added to the <c>Property</c> table (8).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicatePropertyRow =
    { Row: PropertyRow }
    override this.ToString() = sprintf "An event with the same name %O and type %O already exists" this.Row.Name this.Row.Type
    interface IValidationError

[<AutoOpen>]
module private ValidPropertyFlags =
    let [<Literal>] ValidPropertyFlags = PropertyFlags.SpecialName ||| PropertyFlags.RTSpecialName ||| PropertyFlags.HasDefault

/// <summary>Error used when an invalid combination of <c>EventAttributes</c> is used (3).</summary>
/// <category>Errors</category>
type InvalidPropertyAttributesCombination = InvalidFlagsCombination<PropertyFlags>

[<Struct>]
type private PropertyRowValidator =
    interface IRowRangeValidator<PropertyRow> with
        member _.Validate row =
            if row.Flags ||| ValidPropertyFlags <> ValidPropertyFlags
            then Some(InvalidPropertyAttributesCombination row.Flags :> IValidationError) // 3
            else None
        member _.Duplicate row = { DuplicatePropertyRow.Row = row } :> IValidationError // 8

[<Sealed>]
type PropertyTableBuilder internal () =
    let rows = RangedRowList<PropertyRow, PropertyRowValidator>()
    interface ITableBuilder<PropertyRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name.Offset
            StreamOffset.writeBlob &wr hsizes row.Type.PropertySig

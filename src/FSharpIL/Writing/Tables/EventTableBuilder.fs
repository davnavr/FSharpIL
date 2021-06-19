namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when a duplicate row is added to the <c>Event</c> table (11).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateEventRow =
    { Row: EventRow }
    override this.ToString() = sprintf "An event with the same name %O already exists" this.Row.Name
    interface IValidationError

[<AutoOpen>]
module private ValidEventFlags =
    let [<Literal>] ValidEventFlags = EventFlags.SpecialName ||| EventFlags.RTSpecialName

/// <summary>Error used when an invalid combination of <c>EventAttributes</c> is used (3).</summary>
/// <category>Errors</category>
type InvalidEventAttributesCombination = InvalidFlagsCombination<EventFlags>

[<Struct>]
type private EventRowValidator =
    interface IRowRangeValidator<EventRow> with
        member _.Validate row =
            if row.EventFlags ||| ValidEventFlags <> ValidEventFlags
            then Some(InvalidEventAttributesCombination row.EventFlags :> IValidationError) // 3
            else None
        member _.Duplicate row = { DuplicateEventRow.Row = row } :> IValidationError // 11

[<Sealed>]
type EventTableBuilder internal () =
    let rows = RangedRowList<EventRow, EventRowValidator>()
    interface ITableBuilder<EventRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE(uint16 row.EventFlags)
            StreamOffset.writeString &wr hsizes row.Name
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeDefOrRef row.EventType

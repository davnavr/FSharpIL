namespace FSharpIL.Writing.Tables

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<RequireQualifiedAccess>]
module private InvalidFieldFlags =
    let [<Literal>] LiteralInitOnly = FieldFlags.Literal ||| FieldFlags.InitOnly

/// <summary>Error used when an invalid combination of <c>FieldAttributes</c> is used (5, 6).</summary>
/// <category>Errors</category>
type InvalidFieldAttributesCombination = InvalidFlagsCombination<FieldFlags>

/// <summary>Error used when a field is marked <c>Literal</c> but is not marked <c>Static</c> (7).</summary>
/// <category>Errors</category>
type LiteralFieldMustBeStatic () =
    override _.ToString() = "Literal fields must also be static"
    interface IValidationError

/// <summary>Error used when a field is marked <c>RTSpecialName</c> but is not marked <c>SpecialName</c> (8).</summary>
/// <category>Errors</category>
type RTSpecialNameFieldMustBeSpecialName () =
    override _.ToString() = "Fields marked RTSpecialName must also be marked SpecialName"
    interface IValidationError

/// <summary>Error used when a duplicate row is added to the <c>Field</c> table (17).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateFieldRow =
    { Row: FieldRow }
    override this.ToString() =
        sprintf "A field with the same owner, name %O, and signature %O already exists" this.Row.Name this.Row.Signature
    interface IValidationError

[<Struct>]
type internal FieldRowValidator =
    interface IRowRangeValidator<FieldRow> with
        member _.Validate row =
            match row.Flags with
            | _ when row.Flags &&& FieldFlags.FieldAccessMask = FieldFlags.FieldAccessMask -> // 5
                Some(InvalidFieldAttributesCombination FieldFlags.FieldAccessMask :> IValidationError)
            | ValidationResult.CheckFlags InvalidFieldFlags.LiteralInitOnly err -> Some err // 6
            | _ when Flags.set FieldFlags.Literal row.Flags && not(Flags.set FieldFlags.Static row.Flags) -> // 7
                Some(LiteralFieldMustBeStatic() :> IValidationError)
            | _ when Flags.set FieldFlags.RTSpecialName row.Flags && not(Flags.set FieldFlags.SpecialName row.Flags) -> // 8
                Some(RTSpecialNameFieldMustBeSpecialName() :> IValidationError)
            | _ -> None
        member _.Duplicate row = { DuplicateFieldRow.Row = row } :> IValidationError // 17

[<Sealed>]
type FieldTableBuilder internal () =
    // TODO: Make FieldRow implement IComparable, since fields marked CompilerControlled are ignored in duplicate checking (15).
    let rows = RangedRowList<FieldRow, FieldRowValidator>()
    member _.TryAdd fields: ValidationResult<_> = rows.TryAdd fields
    interface ITableBuilder<FieldRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name.Offset
            StreamOffset.writeBlob &wr hsizes row.Signature.FieldSig

namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

/// <summary>Error used when there is a duplicate row in the <c>TypeRef</c> table (6).</summary>
/// <category>Errors</category>
[<RequireQualifiedAccess>]
type DuplicateTypeRef =
    { Duplicate: TypeRefRow }
    interface IValidationError
    override this.ToString() =
        sprintf
            "Cannot add duplicate reference to \"%O\", a type with the same resolution scope, name, and namespace already exists"
            this.Duplicate

/// <summary>Warning used when a row in the <c>TypeRef</c> table uses the <c>Module</c> resolution scope (1d).</summary>
/// <category>Warnings</category>
[<RequireQualifiedAccess>]
type TypeRefUsesModuleResolutionScope =
    { Row: TypeRefRow }
    interface IValidationWarning

[<Sealed>]
type TypeRefTableBuilder internal () =
    let rows = RowSet<TypeRefRow>()

    member _.TryAdd
        (
            row: inref<TypeRefRow>,
            warnings: ValidationWarningsBuilder,
            error: outref<IValidationError>,
            index: outref<TableIndex<TypeRefRow>>
        ) =
        match rows.TryAdd &row with
        | true, index' ->
            if warnings <> null && row.ResolutionScope.Tag = ResolutionScopeTag.Module then
                warnings.Add { TypeRefUsesModuleResolutionScope.Row = row }
            index <- index'
            true
        | false, _ ->
            error <- { DuplicateTypeRef.Duplicate = row }
            false

    interface ITableBuilder<TypeRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            CodedIndex.write &wr tsizes &CodedIndexKinds.ResolutionScope row.ResolutionScope
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.ResolutionScope
            StreamOffset.writeString &wr hsizes row.TypeName.Offset
            StreamOffset.writeString &wr hsizes row.TypeNamespace

namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type AssemblyRefTableBuilder internal () =
    let rows = RowList<AssemblyRefRow>()
    member _.Add(row: inref<_>) = rows.Add &row
    interface ITableBuilder<AssemblyRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE row.MajorVersion
            wr.WriteLE row.MinorVersion
            wr.WriteLE row.BuildNumber
            wr.WriteLE row.RevisionNumber
            wr.WriteLE(uint32 row.Flags)
            StreamOffset.writeBlob &wr hsizes row.PublicKeyOrToken.Token
            StreamOffset.writeString &wr hsizes row.Name.Offset.Offset
            StreamOffset.writeString &wr hsizes row.Culture
            StreamOffset.writeBlob &wr hsizes row.HashValue

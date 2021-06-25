namespace FSharpIL.Writing.Tables

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables

/// <summary>(0x20) Builds the <c>Assembly</c> table, which can only contain one row (II.22.2).</summary>
[<Sealed>]
type AssemblyTableBuilder internal () =
    let mutable exists, row = false, Unchecked.defaultof<AssemblyRow>
    member _.TryGetRow(assembly: outref<_>) =
        if exists then assembly <- row
        exists
    member _.SetRow(assembly: inref<_>) =
        row <- assembly
        exists <- true
        { TableIndex = 1u }: TableIndex<AssemblyRow>
    interface ITableBuilder<AssemblyRow> with
        member _.Count = if exists then 1 else 0
        member _.Item with get index =
            if not exists then invalidOp "The assembly table was empty."
            if index.TableIndex <> 1u then argOutOfRange "index" index "The assembly table only contains one row."
            &row
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint32 row.HashAlgId)
            wr.WriteLE row.MajorVersion
            wr.WriteLE row.MinorVersion
            wr.WriteLE row.BuildNumber
            wr.WriteLE row.RevisionNumber
            wr.WriteLE(uint32 row.Flags)
            StreamOffset.writeBlob &wr hsizes row.PublicKey
            StreamOffset.writeString &wr hsizes row.Name.Offset.Offset
            StreamOffset.writeString &wr hsizes row.Culture

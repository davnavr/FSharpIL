namespace FSharpIL.Writing

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables

type RowBuilder<'Row when 'Row :> ITableRow> = StringsStreamBuilder -> GuidStreamBuilder -> BlobStreamBuilder -> 'Row

/// <summary>Builds the metadata tables stored in the <c>#~</c> metadata stream (II.24.2.6 and II.22).</summary>
[<Sealed>]
type MetadataTablesBuilder (moduleBuilder: RowBuilder<ModuleRow>, strings, guid, blob) as builder =
    let [<Literal>] MaxSmallHeapOffset = 65535u
    let mutable valid, sorted = ValidTableFlags.Module, ValidTableFlags.None

    member _.HeapSizes =
        let mutable flags = HeapSizes.None
        if strings.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.String
        if guid.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Guid
        if blob.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Blob
        flags

    member val Module = moduleBuilder strings guid blob
    member val TypeRef = TypeRefTableBuilder()

    member val IndexSizes =
        { new ITableIndexSizes with
            member _.SizeOf table =
                match table with
                | ValidTableFlags.Module -> 1u
                | ValidTableFlags.TypeRef -> TableBuilder.indexSize builder.TypeRef
                | _ -> 0u }

    member inline private this.SerializeTable(wr: byref<_>, table: #ITableBuilder<_>) =
        TableBuilder.serialize &wr this.HeapSizes this.IndexSizes table

    interface IStreamBuilder with
        member _.StreamName = Magic.StreamNames.metadata
        member _.StreamLength =
            failwith "TODO: Calculate stream length"
        member this.Serialize wr =
            // TODO: Use TablesHeader<_> type?
            wr.WriteLE 0u // Reserved
            wr.Write 2uy // MajorVersion
            wr.Write 0uy // MinorVersion
            wr.Write(uint8 this.HeapSizes)
            wr.Write 0uy // Reserved
            wr.WriteLE(uint64 valid) // Valid
            wr.WriteLE(uint64 sorted) // Sorted

            // Rows
            for i = 0 to 63 do
                let size = this.IndexSizes.SizeOf(LanguagePrimitives.EnumOfValue(1UL <<< i))
                if size > 0u then wr.WriteLE size

            // Module
            wr.WriteLE this.Module.Generation
            TableBuilder.writeStringOffset &wr this.HeapSizes this.Module.Name
            TableBuilder.writeGuidOffset &wr this.HeapSizes this.Module.Mvid
            TableBuilder.writeGuidOffset &wr this.HeapSizes this.Module.EncId
            TableBuilder.writeGuidOffset &wr this.HeapSizes this.Module.EncBaseId

            this.SerializeTable(&wr, this.TypeRef)

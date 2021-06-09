namespace FSharpIL.Writing

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type RowBuilder<'Row when 'Row :> ITableRow> = StringsStreamBuilder -> GuidStreamBuilder -> unit (*BlobStreamBuilder*) -> 'Row

/// <summary>Builds the metadata tables stored in the <c>#~</c> metadata stream (II.24.2.6 and II.22).</summary>
[<Sealed>]
type MetadataTablesBuilder (moduleBuilder: RowBuilder<ModuleRow>, strings, guid) =
    let [<Literal>] MaxSmallHeapOffset = 65535u
    let mutable valid, sorted = ValidTableFlags.Module, ValidTableFlags.None

    member _.HeapSizes =
        let mutable flags = HeapSizes.None
        if strings.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.String
        if guid.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Guid
        //if blobs.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Blob
        flags

    member val Module = moduleBuilder strings guid ()

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
            failwith "TODO: Write row counts"
            failwith "TODO: Serialize table rows"

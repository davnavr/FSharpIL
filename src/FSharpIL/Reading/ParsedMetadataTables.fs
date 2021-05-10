﻿namespace FSharpIL.Reading

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Reading.ByteParser

[<RequireQualifiedAccess>]
module internal OffsetParser =
    let read (buffer: Span<byte>) =
        match buffer.Length with
        | 4 -> Bytes.readU4 0 buffer
        | 2 -> uint32(Bytes.readU2 0 buffer)
        | bad -> invalidArg "buffer" (sprintf "Invalid heap offset size %i" bad)

[<IsReadOnly; Struct>]
type StringParser (sizes: HeapSizes) =
    interface IByteParser<ParsedString> with
        member _.Parse buffer = { ParsedString.StringOffset = OffsetParser.read(buffer.Slice(0, sizes.StringSize)) }
        member _.Length = sizes.StringSize

[<IsReadOnly; Struct>]
type GuidParser (sizes: HeapSizes) =
    interface IByteParser<ParsedGuid> with
        member _.Parse buffer = { ParsedGuid.GuidOffset = OffsetParser.read buffer }
        member _.Length = sizes.GuidSize

// TODO: Allow usage of existing types in FSharpIL.Metadata by using generic parameters and remove "string" and GUID and using some sort of index and builder system just like with Blobs.

[<IsReadOnly; Struct>]
type ParsedModuleRow =
    { Generation: uint16
      Name: ParsedString
      Mvid: ParsedGuid
      EncId: ParsedGuid
      EncBaseId: ParsedGuid }

[<IsReadOnly; Struct>]
type ModuleParser (sizes: HeapSizes) =
    interface IByteParser<ParsedModuleRow> with
        member _.Parse buffer =
            let guid = GuidParser sizes
            let goffset = 2 + sizes.StringSize
            { Generation = Bytes.readU2 0 buffer
              Name = parse 2 buffer (StringParser sizes)
              Mvid = parse goffset buffer guid
              EncId = parse (goffset + sizes.GuidSize) buffer guid
              EncBaseId = parse (goffset + (2 * sizes.GuidSize)) buffer guid }
        member _.Length = 2 + sizes.StringSize + (3 * sizes.GuidSize)

[<IsReadOnly; Struct>]
type internal ParsedCodedIndex =
    { Tag: uint8
      Index: uint32 }

[<RequireQualifiedAccess>]
module CodedIndex =
    [<IsReadOnly; IsByRefLike; Struct>]
    type Parser = struct
        /// Gets a value indicating whether this coded index would occupy four bytes.
        val IsLarge: bool
        val EncodingBits: int32
        internal new (count: uint32, n: int32) = { IsLarge = count > (0xFFFFu >>> n); EncodingBits = n }
        /// The number of bytes that this coded index would occupy.
        member this.Length = if this.IsLarge then 4 else 2
        member internal this.Parse(buffer: Span<byte>) =
            if this.IsLarge then
                let index = Bytes.readU4 0 buffer
                { Tag = uint8(index >>> (32 - this.EncodingBits))
                  Index = index &&& (UInt32.MaxValue >>> this.EncodingBits) }
            else
                let index = Bytes.readU2 0 buffer
                { Tag = uint8(index >>> (16 - this.EncodingBits))
                  Index = uint32(index &&& (UInt16.MaxValue >>> this.EncodingBits)) }
        member inline internal this.Parse(offset, buffer: Span<byte>) = this.Parse(buffer.Slice offset)
    end

    let resolutionScopeParser (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.Module
            + (counts.GetValueOrDefault MetadataTableFlags.ModuleRef)
            + (counts.GetValueOrDefault MetadataTableFlags.AssemblyRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef),
            2
        )

// TODO: Have constants that store the tags for coded indices instead of duplicating them with the writing code.
[<IsReadOnly; Struct>]
type ParsedResolutionScope =
    private { ResolutionScope: ParsedCodedIndex }

// TODO: Use RawIndex<'T> type instead of uint32.

[<RequireQualifiedAccess>]
module ParsedResolutionScope =
    let (|Null|Module|ModuleRef|AssemblyRef|TypeRef|Unknown|) { ResolutionScope = rscope } =
        match rscope with
        | { Index = 0u } -> Null
        | { Tag = 0uy } -> Module rscope.Index
        | { Tag = 1uy } -> ModuleRef rscope.Index
        | { Tag = 2uy } -> AssemblyRef rscope.Index
        | { Tag = 3uy } -> TypeRef rscope.Index
        | { Tag = unknown } -> Unknown unknown

[<IsReadOnly; Struct>]
type ParsedTypeRefRow =
    { ResolutionScope: ParsedResolutionScope
      TypeName: ParsedString
      TypeNamespace: ParsedString }

[<IsReadOnly; Struct>]
type TypeRefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    interface IByteParser<ParsedTypeRefRow> with
        member _.Parse buffer =
            let str = StringParser sizes
            let rscope = CodedIndex.resolutionScopeParser counts
            { ResolutionScope = { ResolutionScope = rscope.Parse buffer }
              TypeName = parse rscope.Length buffer str
              TypeNamespace = parse (rscope.Length + sizes.StringSize) buffer str }
        member _.Length = (CodedIndex.resolutionScopeParser counts).Length + (2 * sizes.StringSize)

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTable<'Parser, 'Row when 'Parser :> IByteParser<'Row>> =
    internal
        { Chunk: ChunkReader
          Table: MetadataTableFlags
          TableOffset: uint64
          TableParser: 'Parser
          TableCount: uint32 }

    member this.RowCount = this.TableCount
    /// The size of this metadata table in bytes.
    member this.Size = uint64 this.TableCount * uint64 this.TableParser.Length

    // TODO: Consider having reader functions for each table instead, for better error handling and reporting that includes offset and other information
    member this.TryGetRow(i: uint32) =
        if i >= this.RowCount then Error(MetadataRowOutOfBounds(this.Table, i, this.TableCount))
        else
            let buffer = Span.stackalloc<Byte> this.TableParser.Length
            if this.Chunk.TryReadBytes(this.TableOffset + (uint64 i * uint64 this.TableParser.Length), buffer)
            then Ok(this.TableParser.Parse buffer)
            else Error(StructureOutsideOfCurrentSection(ParsedStructure.MetadataRow(this.Table, i)))

    /// <exception cref="System.ArgumentOutOfRangeException">
    /// Thrown when the index is negative or the table does not contain enough rows.
    /// </exception>
    member this.Item with get(i: int32) =
        if i < 0 then raise(IndexOutOfRangeException())
        else
            match this.TryGetRow(uint32 i) with
            | Error(MetadataRowOutOfBounds _) -> raise(IndexOutOfRangeException())
            | Error err -> failwithf "Error occured while retrieving row at index %i, %O" i err
            | Ok row -> row

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTables =
    private
        { Chunk: ChunkReader
          TablesHeader: ParsedMetadataTablesHeader
          TablesOffset: uint64
          [<DefaultValue>] mutable TablesSize: uint64
          [<DefaultValue>] mutable ModuleTable: ParsedMetadataTable<ModuleParser, ParsedModuleRow>
          [<DefaultValue>] mutable TypeRefTable: ParsedMetadataTable<TypeRefParser, ParsedTypeRefRow> voption }

    member this.Header = this.TablesHeader
    /// The size of the metadata tables in bytes.
    member this.Size = this.TablesSize
    /// Offset from start of section containing the CLI metadata to the first byte of the first row of the first table.
    member this.Offset = this.TablesOffset

    member this.Module =
        if this.ModuleTable = Unchecked.defaultof<_> then invalidOp "Unable to find module table"
        this.ModuleTable

    member this.TypeRef = this.TypeRefTable

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    let internal create chunk header offset =
        let tables =
            { Chunk = chunk
              TablesHeader = header
              TablesOffset = offset }
        for KeyValue(table, count) in header.Rows do
            let inline createTable parser =
                { Chunk = chunk
                  Table = table
                  TableOffset = tables.TablesSize + offset
                  TableParser = parser
                  TableCount = count }
            match table with
            | MetadataTableFlags.Module ->
                tables.ModuleTable <- createTable(ModuleParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.ModuleTable.Size
            | MetadataTableFlags.TypeRef ->
                tables.TypeRefTable <- TypeRefParser(header.HeapSizes, header.Rows) |> createTable |> ValueSome
                tables.TablesSize <- tables.TablesSize + tables.TypeRefTable.Value.Size
            | _ -> () // Temporary to get printing to work
        tables

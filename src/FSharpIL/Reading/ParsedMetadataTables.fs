namespace FSharpIL.Reading

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
        member _.Parse buffer = ParsedString(OffsetParser.read buffer)
        member _.Length = sizes.StringSize

[<IsReadOnly; Struct>]
type GuidParser (sizes: HeapSizes) =
    interface IByteParser<ParsedGuid> with
        member _.Parse buffer = ParsedGuid(OffsetParser.read buffer)
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
type ParsedTypeRefRow =
    { ResolutionScope: ResolutionScope
      TypeName: ParsedString
      TypeNamespace: ParsedString }

[<IsReadOnly; Struct>]
type internal ParsedCodedIndex =
    { Tag: uint32
      Index: uint32 }

[<RequireQualifiedAccess>]
module internal CodedIndex =
    [<IsReadOnly; IsByRefLike; Struct>]
    type Parser = struct
        val IsLarge: bool
        val EncodingBits: int32
        new (count: uint32, n: int32) = { IsLarge = count > (0xFFFFu >>> n); EncodingBits = n }
        member this.Length = if this.IsLarge then 4 else 2
        member this.Parse(buffer: Span<byte>) =
            if this.IsLarge then
                let index = Bytes.readU4 0 buffer
                { Tag = index >>> (32 - this.EncodingBits)
                  Index = index &&& (UInt32.MaxValue >>> this.EncodingBits) }
            else
                let index = Bytes.readU2 0 buffer
                { Tag = uint32(index >>> (16 - this.EncodingBits))
                  Index = uint32(index &&& (UInt16.MaxValue >>> this.EncodingBits)) }
        member inline this.Parse(offset, buffer: Span<byte>) = this.Parse(buffer.Slice offset)
    end

    let inline resolutionScopeParser (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.Module
            + (counts.GetValueOrDefault MetadataTableFlags.ModuleRef)
            + (counts.GetValueOrDefault MetadataTableFlags.AssemblyRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef),
            2
        )

[<IsReadOnly; Struct>]
type TypeRefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    interface IByteParser<ParsedTypeRefRow> with
        member _.Parse buffer =
            let str = StringParser sizes
            let rscope = CodedIndex.resolutionScopeParser counts
            { ResolutionScope =
                let rscope' = rscope.Parse buffer
                ResolutionScope(LanguagePrimitives.EnumOfValue(uint8 rscope'.Tag), int32 rscope'.Index)
              TypeName = parse rscope.Length buffer str
              TypeNamespace = parse (rscope.Length + sizes.StringSize) buffer str }
        member _.Length = (CodedIndex.resolutionScopeParser counts).Length + (2 * sizes.StringSize)

[<Sealed>]
type ParsedMetadataTable<'Parser, 'Row when 'Parser :> IByteParser<'Row>> internal
    (
        chunk: ChunkReader,
        offset: uint64,
        parser: 'Parser,
        count: uint32
    ) =
    member _.Count = count
    /// <exception cref="System.ArgumentOutOfRangeException">
    /// Thrown when the index is negative or when the table does not contain enough rows.
    /// </exception>
    member _.Item with get(i: int32) =
        if i < 0 || uint32 i >= count then raise(IndexOutOfRangeException())
        let buffer = Span.stackalloc<Byte> parser.Length
        chunk.ReadBytes(offset + (uint64 i * uint64 parser.Length), buffer)
        parser.Parse buffer

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTables =
    private
        { Chunk: ChunkReader
          TablesHeader: ParsedMetadataTablesHeader
          TablesOffset: uint64
          [<DefaultValue>] mutable ModuleTable: ParsedMetadataTable<ModuleParser, ParsedModuleRow>
          [<DefaultValue>] mutable TypeRefTable: ParsedMetadataTable<TypeRefParser, ParsedTypeRefRow> voption }

    member this.Header = this.TablesHeader

    /// Offset from start of section containing the CLI metadata to the first byte of the first row of the first table.
    member this.Offset = this.TablesOffset

    member this.Module =
        if this.ModuleTable = Unchecked.defaultof<_> then invalidOp "Unable to find module table"
        this.ModuleTable

    member this.TypeRef = this.TypeRefTable

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    let internal create header =
        let tables =
            { Chunk = failwith ""
              TablesHeader = header
              TablesOffset = failwith "" }
        for KeyValue(table, count) in header.Rows do
            match table with
            | MetadataTableFlags.Module ->
                tables.ModuleTable <- ParsedMetadataTable(failwith "", failwith "", ModuleParser header.HeapSizes, count)
            | MetadataTableFlags.TypeRef ->
                tables.TypeRefTable <-
                    ValueSome(ParsedMetadataTable(failwith "", failwith "", TypeRefParser(header.HeapSizes, header.Rows), count))
        tables

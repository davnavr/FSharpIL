namespace FSharpIL.Reading

open System
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

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type ParsedModuleRow =
    { Generation: uint16
      Name: ParsedString
      Mvid: ParsedGuid
      EncId: ParsedGuid
      EncBaseId: ParsedGuid }

[<IsReadOnly; Struct>]
type ModuleParser (sizes: HeapSizes) =
    interface IByteParser<ParsedModuleRow> with
        member _.Parse(buffer: Span<byte>) =
            let guid = GuidParser sizes
            let goffset = 2 + sizes.StringSize
            { Generation = Bytes.readU2 0 buffer
              Name = parse 2 buffer (StringParser sizes)
              Mvid = parse goffset buffer guid
              EncId = parse (goffset + sizes.GuidSize) buffer guid
              EncBaseId = parse (goffset + (2 * sizes.GuidSize)) buffer guid }
        member _.Length = 2 + sizes.StringSize + (3 * sizes.GuidSize)

type ParsedMetadataTable<'Parser, 'Row when 'Parser :> IByteParser<'Row>> (parser: 'Parser, count: uint32) =
    member _.Count = count

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTables =
    private
        { Chunk: ChunkReader
          TablesHeader: ParsedMetadataTablesHeader
          /// Offset from start of section containing the CLI metadata to the first byte of the first row of the first table.
          TablesOffset: uint64 }

    member this.Header = this.TablesHeader

    member _.Modules = () // TODO: Retrieve count depending on Valid flags.

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    ()

namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata

[<Obsolete>]
[<NoComparison; StructuralEquality>]
type ParsedModuleRow = // TODO: Allow Generation, EncId, and EncBaseId to be set to allow its usage in FSharpIL.Reading
    { Generation: uint16
      Name: ParsedString
      Mvid: RawIndex<Guid> // ParsedGuid
      EncId: RawIndex<Guid>
      EncBaseId: RawIndex<Guid> }

[<IsReadOnly; Struct>]
type ModuleParser =
    interface IByteParser<ParsedModuleRow> with
        member this.Parse(arg1: Span<byte>): ParsedModuleRow = 
            raise (System.NotImplementedException())
        member _.Length = 0

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

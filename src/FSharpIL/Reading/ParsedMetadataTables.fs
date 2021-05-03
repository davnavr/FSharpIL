namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

[<IsReadOnly; IsByRefLike; Struct>]
type _ParsedModuleRow =
    { A: Span<byte> }

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTables =
    private
        { Chunk: ChunkReader
          TablesHeader: ParsedMetadataTablesHeader
          /// Offset from start of section containing the CLI metadata to the first byte of the first row of the first table.
          TablesOffset: uint64 }

    member _.Modules = () // TODO: Retrieve count depending on Valid flags.

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    ()

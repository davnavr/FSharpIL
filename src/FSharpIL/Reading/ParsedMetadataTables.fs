namespace FSharpIL.Reading

open System.Collections.Generic

open FSharpIL
open FSharpIL.Metadata.Tables

type ParsedTableRowCounts = IReadOnlyDictionary<ValidTableFlags, uint32>

type ParsedTablesHeader = TablesHeader<ParsedTableRowCounts>

[<Sealed>]
type ParsedMetadataTables = class
    val private stream: ChunkedMemory
    //val internal moduleTable
    //val MethodBodies
    val Header: ParsedTablesHeader

    internal new (stream, header) = { stream = stream; Header = header }
end

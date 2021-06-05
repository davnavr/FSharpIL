namespace FSharpIL.Reading

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

// TODO: What to use for EntryPointToken?
type ParsedCliHeader = CliHeader<uint32, CorFlags, uint32, RvaAndSize, RvaAndSize, RvaAndSize, RvaAndSize, RvaAndSize>

/// Collection of functions used to read CLI metadata (II.24).
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadCliHeader: StructureReader<ParsedCliHeader, 'State>
      HandleError: ErrorHandler<'State> }

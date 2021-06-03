namespace FSharpIL.Reading

open FSharpIL.Metadata

/// Collection of functions used to read CLI metadata (II.24).
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadCliHeader: StructureReader<ParsedCliHeader, 'State>
      HandleError: ErrorHandler<'State> }

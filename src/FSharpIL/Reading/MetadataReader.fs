﻿namespace FSharpIL.Reading

open System.Collections.Immutable

/// Collection of functions used to read CLI metadata (II.24).
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadCliHeader: StructureReader<ParsedCliHeader, 'State>
      ReadMetadataRoot: StructureReader<ParsedCliMetadataRoot, 'State>
      ReadStreamHeaders: StructureReader<ImmutableArray<ParsedStreamHeader>, 'State>
      ReadStringsStream: StructureReader<ParsedStringsStream, 'State>
      ReadGuidStream: StructureReader<ParsedGuidStream, 'State>
      ReadUserStringStream: StructureReader<ParsedUserStringStream, 'State>
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module MetadataReader =
    let defaultReader<'State> =
        { ReadCliHeader = ValueNone
          ReadMetadataRoot = ValueNone
          ReadStreamHeaders = ValueNone
          ReadStringsStream = ValueNone
          ReadGuidStream = ValueNone
          ReadUserStringStream = ValueNone
          HandleError = ErrorHandler.throwOnError }
        : MetadataReader<'State>

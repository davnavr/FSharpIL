namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL.PortableExecutable

type ParsedCoffHeader = CoffHeader<uint16, uint16>
/// Represents a PE file optional header that has been parsed (II.25.2.3).
type ParsedOptionalHeader = OptionalHeader<ImageKind, uint32, uint32, uint64, uint32, uint32>
/// Represents the data directories of a PE File that have been parsed (II.25.2.3.3).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedDataDirectories = { DataDirectories: DataDirectories; Additional: ImmutableArray<RvaAndSize> }

/// Collection of functions used to read a Portable Executable file (II.25).
[<NoComparison; NoEquality>]
type PEFileReader<'State> =
    { ReadLfanew: StructureReader<uint32, 'State>
      ReadCoffHeader: StructureReader<ParsedCoffHeader, 'State>
      ReadOptionalHeader: StructureReader<ParsedOptionalHeader, 'State>
      ReadDataDirectories: StructureReader<ParsedDataDirectories, 'State>
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module PEFileReader =
    let defaultReader<'State> =
        { ReadLfanew = ValueNone
          ReadCoffHeader = ValueNone
          ReadOptionalHeader = ValueNone
          ReadDataDirectories = ValueNone
          HandleError = ErrorHandler.throwOnError }
        : PEFileReader<'State>

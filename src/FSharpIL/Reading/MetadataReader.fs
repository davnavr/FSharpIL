namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

// TODO: What to use for EntryPointToken?
type ParsedCliHeader = CliHeader<uint32, CorFlags, uint32, RvaAndSize, RvaAndSize, RvaAndSize, RvaAndSize, RvaAndSize>
type ParsedCliMetadataRoot = CliMetadataRoot<uint32>

// TODO: Move these two types to Metadata namespace instead, and just have writing code use an immutable array of parsed stream headers?
/// Offset from the start of the CLI metadata root, used to specify where a metadata stream begins (II.24.2.2).
[<IsReadOnly>]
type MetadataRootOffset = struct
    val private offset: uint32
    new (offset) = { offset = offset }
    static member op_Implicit(offset: MetadataRootOffset) = offset.offset
end

/// Describes the location, size, and name of a metadata stream (II.24.2.2).
[<IsReadOnly; Struct>]
type ParsedStreamHeader =
    { Offset: MetadataRootOffset
      /// The size of this metadata stream, rounded up to a multiple of four.
      Size: uint32
      /// The name of the stream, including padding null bytes.
      StreamName: ImmutableArray<byte> }

/// Collection of functions used to read CLI metadata (II.24).
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadCliHeader: StructureReader<ParsedCliHeader, 'State>
      ReadMetadataRoot: StructureReader<ParsedCliMetadataRoot, 'State>
      ReadStreamHeaders: StructureReader<ImmutableArray<ParsedStreamHeader>, 'State>
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module MetadataReader =
    let defaultReader<'State> =
        { ReadCliHeader = ValueNone
          ReadMetadataRoot = ValueNone
          ReadStreamHeaders = ValueNone
          HandleError = ErrorHandler.throwOnError }
        : MetadataReader<'State>

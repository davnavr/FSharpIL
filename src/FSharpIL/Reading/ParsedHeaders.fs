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
    override this.ToString() = sprintf "0x%08X" this.offset
end

/// Describes the location, size, and name of a metadata stream (II.24.2.2).
[<IsReadOnly; Struct>]
type ParsedStreamHeader =
    { Offset: MetadataRootOffset
      /// The size of this metadata stream, rounded up to a multiple of four.
      Size: uint32
      /// The name of the stream in ASCII encoding, including padding null bytes.
      StreamName: ImmutableArray<byte> } // TODO: Create new type for stream name.

    member this.PrintedName = System.Text.Encoding.ASCII.GetString(this.StreamName.AsSpan()).TrimEnd '\000'

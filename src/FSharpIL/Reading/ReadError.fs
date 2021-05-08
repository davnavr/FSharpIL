﻿namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Text

open FSharpIL
open FSharpIL.Metadata

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type ParsedStructure =
    | CliHeader
    | CliMetadataRoot
    | StreamHeader of index: int32
    | StringHeap of size: uint64
    | GuidHeap of size: uint64
    | UserStringHeap of size: uint64
    | BlobHeap of size: uint64
    | MetadataSignature
    | MetadataTablesHeader
    | MetadataTableRowCounts
    | MetadataRow of table: MetadataTableFlags * index: uint32

    override this.ToString() =
        match this with
        | CliHeader -> "the CLI metadata header"
        | CliMetadataRoot -> "the CLI metadata root"
        | StreamHeader i -> sprintf "the CLI metadata stream header at index %i" i
        | StringHeap size -> sprintf "the \"#Strings\" metadata heap (%i bytes)" size
        | GuidHeap size -> sprintf "the \"#GUID\" metadata heap (%i bytes)" size
        | UserStringHeap size -> sprintf "the \"#US\" metadata heap (%i bytes)" size
        | BlobHeap size -> sprintf "the \"#Blob\" metadata heap (%i bytes)" size
        | MetadataSignature -> "the CLI metadata signature"
        | MetadataTablesHeader -> "the CLI metadata tables header"
        | MetadataTableRowCounts -> "the CLI metadata table row counts"
        | MetadataRow(table, index) -> sprintf "the %A metadata row at index %i (0x%08x)" table index index

[<NoComparison; NoEquality>]
type ReadError =
    | InvalidMagic of expected: ImmutableArray<byte> * actual: byte[]
    | CannotMoveToPreviousOffset of offset: uint64
    | OptionalHeaderTooSmall of size: uint16
    | TooFewDataDirectories of count: uint32
    | NoCliMetadata
    | RvaNotInTextSection of rva: uint32
    | CliHeaderTooSmall of size: uint32
    | MetadataVersionHasNoNullTerminator of version: byte[]
    | StructureOutsideOfCurrentSection of ParsedStructure
    | MissingNullTerminator of string
    | InvalidStringIndex of offset: uint32 * size: uint64
    | CannotFindMetadataTables
    | MissingModuleTable
    | MetadataRowOutOfBounds of table: MetadataTableFlags * index: uint32 * count: uint32
    | CannotReadDebugTables // TODO: Mark debug tables error as obsolete when debug tables are supported.
    | UnexpectedEndOfFile

    member this.Message =
        match this with
        | InvalidMagic(expected, Bytes.ReadOnlySpan actual) ->
            sprintf
                "expected magic %s, but got %s"
                (Bytes.print(expected.AsSpan()))
                (Bytes.print actual)
        | CannotMoveToPreviousOffset offset -> sprintf "Cannot move to previous offset 0x%016X" offset
        | OptionalHeaderTooSmall size -> sprintf "the specified optional header size (%i) is too small" size
        | TooFewDataDirectories count -> sprintf "the number of data directories (%i) is too small" count
        | NoCliMetadata -> "the Portable Executable file does not contain any CLI metadata"
        | RvaNotInTextSection rva ->
            sprintf "the Relative Virtual Address (0x%08X) does not point into the \".text\" section" rva
        | CliHeaderTooSmall size -> sprintf "the specified CLI header size is too small (%i)" size
        | MetadataVersionHasNoNullTerminator version ->
            sprintf
                "the metadata version in the CLI metadata root \"%s\" does not end in a null terminator"
                (Encoding.UTF8.GetString version)
        | StructureOutsideOfCurrentSection structure ->
            sprintf
                "%O does not fit within the current section, check that the offset to the structure or the size of the section is correct"
                structure
        | MissingNullTerminator name -> sprintf "the string \"%s\" does not end in a null terminator" name
        | InvalidStringIndex(offset, size) ->
            sprintf "Invalid offset into the \"#Strings\" heap (0x%08X), maximum valid offset is (0x%08X)" offset (size - 1UL)
        | CannotFindMetadataTables -> "the stream containing the metadata tables \"#~\" could not be found"
        | MissingModuleTable -> "the Module table (0x00) is missing"
        | MetadataRowOutOfBounds(table, index, count) ->
            sprintf
                "the %A metadata row at index %i (0x%08x) is out of bounds, the table only contains %i (0x%08x) items"
                table
                index
                index
                count
                count
        | CannotReadDebugTables -> "the metadata tables contain debugging metadata, which is currently not supported by FSharpIL"
        | UnexpectedEndOfFile -> "the end of the file was unexpectedly reached"

[<RequireQualifiedAccess>]
module ReadError =
    let message (state: ReadState) (error: ReadError) { FileOffset = offset } =
        sprintf "Error occured while %s at offset 0x%X: %s" state.Description offset error.Message

exception ReadException
    of state: ReadState * error: ReadError * offset: FileOffset
    with override this.Message = ReadError.message this.state this.error this.offset

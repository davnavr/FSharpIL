﻿namespace FSharpIL.Reading

open System
open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata.Tables
open FSharpIL.Metadata.Blobs
open FSharpIL.PortableExecutable

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type ParsedStructure =
    | CliHeader
    | CliMetadataRoot
    | StreamHeader of index: int32
    | StringHeap of size: uint32
    | GuidHeap of size: uint32
    | UserStringHeap of size: uint32
    | BlobHeap of size: uint32
    | MetadataSignature
    | MetadataTablesHeader
    | MetadataTableRowCounts
    | MetadataRow of table: ValidTableFlags * index: uint32

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

// TODO: Move offset: uint32 to BlobError case in ReadError and use offset: ParsedBlob
type BlobError =
    | BlobOutsideOfHeap of offset: uint32 * size: uint32
    | ExpectedEndOfBlob of offset: uint32 * size: uint32 * remaining: uint32
    | CompressedIntegerOutOfBounds of size: uint32
    | InvalidBlobOffset of offset: uint32 * max: uint32
    | InvalidUnsignedCompressedIntegerKind of msb: uint8
    | InvalidFieldSignatureMagic of actual: uint8
    | InvalidElementType of etype: ElementType
    | InvalidGenericInstantiationKind of ElementType voption
    | MissingGenericArguments
    | InvalidMethodSignatureCallingConvention of uint8 voption
    | InvalidPropertyMagic of uint8 voption
    | InvalidCustomAttributeProlog of uint16 voption
    | MissingNamedArgumentCount

    override this.ToString() =
        match this with
        | BlobOutsideOfHeap(offset, size) ->
            sprintf
                "the blob at offset (0x%08X) points to a blob with an invalid size (0x%08X), the blob extends outside of the heap"
                offset
                size
        | ExpectedEndOfBlob(offset, size, remaining) ->
            sprintf
                "the blob at offset 0x%08X with size 0x%08X was expected to end at offset 0x%08X but %i bytes were remaining"
                offset
                size
                (offset + size - remaining - 1u)
                remaining
        | CompressedIntegerOutOfBounds size ->
            sprintf "the %i-byte wide compressed integer extended outside the end of the blob" size
        | InvalidBlobOffset(offset, max) -> sprintf "invalid blob offset 0x%08X, maximum valid offset is 0x%08X" offset max
        | InvalidUnsignedCompressedIntegerKind msb ->
            sprintf
                "the first byte of the unsigned compressed integer (0b%s) is invalid, only 1-byte integers (0b0???), 2-byte integers (0b10??), or 4-byte integers are valid (0b110?)"
                (Convert.ToString(msb, 2))
        | InvalidFieldSignatureMagic actual -> sprintf "expected field signature to begin with the byte 0x06, but got 0x%02X" actual
        | InvalidElementType etype -> sprintf "the element type %A (0x%02X) is invalid" etype (uint8 etype)
        | InvalidGenericInstantiationKind etype ->
            match etype with
            | ValueNone -> "end of blob"
            | ValueSome etype -> sprintf "%A (%02X)" etype (uint8 etype)
            |> sprintf
                "invalid generic instantiation, expected CLASS (0x%02X) or VALUETYPE (0x%02X) but got %s"
                (uint8 ElementType.Class)
                (uint8 ElementType.ValueType)
        | MissingGenericArguments -> "Expected at least 1 generic argument but got 0"
        | InvalidMethodSignatureCallingConvention(ValueSome cconv) ->
            sprintf
                "the calling conventions of the method signature %A (0x%02X) are invalid"
                (LanguagePrimitives.EnumOfValue<_, CallingConvention> cconv)
                cconv
        | InvalidMethodSignatureCallingConvention ValueNone -> "expected method signature calling conventions but got empty blob"
        | InvalidPropertyMagic actual ->
            match actual with
            | ValueNone -> "end of blob"
            | ValueSome magic -> sprintf "0x%02X" magic
            |> sprintf "expected property signature to begin with PROPERTY (0x8) or PROPERTY HASTHIS (0x28), but got %s"
        | InvalidCustomAttributeProlog actual ->
            match actual with
            | ValueSome prolog -> sprintf "got 0x%04X" prolog
            | ValueNone -> "the blob did not contain enough bytes"
            |> sprintf "expected custom attribute PROLOG 0x0001, but %s"
        | MissingNamedArgumentCount -> "missing custom attribute named argument count NumNamed"

[<NoComparison; NoEquality>]
type ReadError =
    | UnexpectedEndOfFile
    | InvalidMagic of expected: ImmutableArray<byte> * actual: ImmutableArray<byte>
    | CannotMoveToPreviousOffset of offset: FileOffset
    | OptionalHeaderTooSmall of size: uint16
    | UnsupportedOptionalHeaderSize of size: uint16
    | TooFewDataDirectories of count: uint32
    | StructureOutsideOfCurrentSection of ParsedStructure
    | NoCliMetadata
    | RvaNotInCliSection of Rva
    | InvalidCliHeaderLocation of Rva
    | CliHeaderTooSmall of size: uint32

    override this.ToString() =
        match this with
        | UnexpectedEndOfFile -> "the end of the file was unexpectedly reached"
        | InvalidMagic(expected, actual) ->
            sprintf
                "expected magic %s, but got %s"
                (Bytes.print(expected.AsSpan()))
                (Bytes.print(actual.AsSpan()))
        | CannotMoveToPreviousOffset offset -> sprintf "Cannot move to previous offset 0x%O" offset
        | OptionalHeaderTooSmall size -> sprintf "the specified optional header size (%i) is too small" size
        | UnsupportedOptionalHeaderSize size -> sprintf "the parser does not support an optional header size of %i bytes" size
        | TooFewDataDirectories count -> sprintf "the number of data directories (%i) is too small" count
        | StructureOutsideOfCurrentSection structure ->
            sprintf
                "%O does not fit within the current section, check that the offset to the %O or that the size of the section is correct"
                structure
                structure
        | NoCliMetadata -> "the Portable Executable file does not contain any CLI metadata"
        | RvaNotInCliSection rva ->
            sprintf "the Relative Virtual Address (%O) does not point into the same section containing the CLI header" rva
        | InvalidCliHeaderLocation rva ->
            sprintf "the CLI header at the Relative Virtual Address (0x%O), is not contained within any section" rva
        | CliHeaderTooSmall size -> sprintf "the specified CLI header size is too small (%i)" size

[<RequireQualifiedAccess>]
module ReadError =
    let message (state: IReadState) (error: ReadError) { FileOffset = offset } =
        sprintf "Error occured while %O at offset 0x%X: %O" state offset error

exception ReadException
    of state: IReadState * error: ReadError * offset: FileOffset
    with override this.Message = ReadError.message this.state this.error this.offset

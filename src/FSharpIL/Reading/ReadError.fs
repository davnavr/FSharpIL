namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Text

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Metadata.Blobs
open FSharpIL.PortableExecutable

/// Represents a structure or file header used in CLI metadata (II.24).
[<RequireQualifiedAccess>]
type ParsedMetadataStructure =
    | CliMetadataRoot
    | MetadataSignature
    | StreamHeader of index: int32
    | MetadataTablesHeader
    | MetadataTable of table: ValidTableFlags

    override this.ToString() =
        match this with
        | CliMetadataRoot -> "the CLI metadata root"
        | MetadataSignature -> "the CLI metadata signature"
        | StreamHeader i -> sprintf "the stream header (index %i)" i
        | MetadataTablesHeader -> "the metadata tables header"
        | MetadataTable table -> sprintf "the %A metadata table" table

// TODO: Move offset: uint32 to BlobError case in ReadError and use offset: ParsedBlob
type BlobError =
    | BlobOutsideOfHeap of offset: uint32 * size: uint32 // TODO: Rename to BlobOutsideOfStream
    | ExpectedEndOfBlob of offset: uint32 * size: uint32 * remaining: uint32
    | CompressedIntegerOutOfBounds of size: uint32
    | InvalidBlobOffset of offset: uint32 * max: uint32
    | InvalidUnsignedCompressedIntegerKind of msb: uint8
    | InvalidFieldSignatureMagic of actual: uint8
    | InvalidElementType of etype: ElementType
    | MissingElementType
    | UnexpectedTypeSpec of TableIndex<TypeSpecRow>
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
                "the blob at offset (0x%08X) points to a blob with an invalid size (0x%08X), the blob extends outside of the stream"
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
        | InvalidFieldSignatureMagic actual ->
            sprintf "expected field signature to begin with the byte 0x06, but got 0x%02X" actual
        | InvalidElementType etype -> sprintf "the element type %A (0x%02X) is invalid" etype (uint8 etype)
        | MissingElementType -> "expected ELEMENT_TYPE byte but got the end of the blob instead"
        | UnexpectedTypeSpec { TableIndex = i } ->
            sprintf "expected TypeDef or TypeRef but got an index (0x%08X) into the TypeSpec table instead" i
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
                (LanguagePrimitives.EnumOfValue<_, CallConvFlags> cconv)
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

type ReadError =
    | UnexpectedEndOfFile
    | InvalidMagic of expected: ImmutableArray<byte> * actual: ImmutableArray<byte>
    | CannotMoveToPreviousOffset of offset: FileOffset
    | OptionalHeaderTooSmall of size: uint16
    | UnsupportedOptionalHeaderSize of size: uint16
    | InvalidAlignment of salignment: uint32 * falignment: uint32
    | TooFewDataDirectories of count: uint32
    | StructureOutOfBounds of ParsedMetadataStructure
    | NoCliMetadata
    | RvaNotInCliSection of Rva
    | InvalidCliHeaderLocation of Rva
    | CliHeaderOutOfSection of Rva
    | CliHeaderTooSmall of size: uint32
    | InvalidEntryPointKind of MetadataTokenType
    | InvalidMetadataVersionLength of length: uint32
    | MissingNullTerminator of string
    | StreamOutOfBounds of index: int32 * ParsedStreamHeader
    | InvalidStringOffset of StringOffset * max: StringOffset // TODO: This case should be a BlobError case instead.
    | MissingStringStreamTerminator
    | InvalidGuidIndex of GuidIndex * max: GuidIndex
    | InvalidBlob of offset: uint32 * BlobError
    | TableIsEmpty of ValidTableFlags
    | TableHasMoreThanOneRow of ValidTableFlags * count: uint32

    override this.ToString() =
        match this with
        | UnexpectedEndOfFile -> "the end of the file was unexpectedly reached"
        | InvalidMagic(expected, actual) ->
            let inline print () (bytes: ImmutableArray<byte>) =
                let sb = StringBuilder(bytes.Length * 5 - 1)
                for i = 0 to bytes.Length - 1 do
                    if i > 0 then sb.Append ' ' |> ignore
                    Printf.bprintf sb "0x%02X" bytes.[i]
                sb.ToString()
            sprintf "expected magic %a, but got %a" print expected print actual
        | CannotMoveToPreviousOffset offset -> sprintf "Cannot move to previous offset %O" offset
        | OptionalHeaderTooSmall size -> sprintf "the specified optional header size (%i) is too small" size
        | UnsupportedOptionalHeaderSize size -> sprintf "the parser does not support an optional header size of %i bytes" size
        | InvalidAlignment(salignment, falignment) ->
            sprintf "the section alignment (0x%08X) and file alignment (0x%08X) are invalid" salignment falignment
        | TooFewDataDirectories count -> sprintf "the number of data directories (%i) is too small" count
        | StructureOutOfBounds structure -> sprintf "%O was out of bounds" structure
        | NoCliMetadata -> "the Portable Executable file does not contain any CLI metadata"
        | RvaNotInCliSection rva ->
            sprintf "the Relative Virtual Address (%O) does not point into the same section containing the CLI header" rva
        | InvalidCliHeaderLocation rva ->
            sprintf "the CLI header at the Relative Virtual Address (%O) is not contained within any section" rva
        | CliHeaderOutOfSection rva ->
            sprintf "the CLI header at the Relative Virtual Address (%O) extends past the end of the section" rva
        | CliHeaderTooSmall size -> sprintf "the specified CLI header size is too small (%i)" size
        | InvalidEntryPointKind kind ->
            sprintf "the entry point token was expected to be null, MethodDef, or File, but got %A" kind
        | InvalidMetadataVersionLength length ->
            if length > 255u
            then "cannot exceed 255 bytes"
            else "was expected to be a multiple of 4"
            |> sprintf "the length of the Version field of the CLI metadata root (%i bytes) %s" length
        | StreamOutOfBounds(_, header) ->
            sprintf
                "the \"%s\" stream at offset %O from the start of the CLI metadata root with size %i bytes was out of bounds"
                header.PrintedName
                header.Offset
                header.Size
        | InvalidStringOffset(offset, max) ->
            sprintf "Invalid offset into the \"#Strings\" stream (%O), maximum valid offset is (%O)" offset max
        | MissingStringStreamTerminator -> "the last byte of the \"#Strings\" stream must end in a null byte"
        | MissingNullTerminator s -> sprintf "The string \"%s\" is missing a null terminator" s
        | InvalidGuidIndex(offset, max) ->
            sprintf "Invalid index into the \"#GUID\" stream (%O), maximum valid index is (%O)" offset max
        | InvalidBlob(offset, err) -> sprintf "the blob at offset 0x%08X is invalid, %O" offset err
        | TableIsEmpty table -> sprintf "The %A metadata table was unexpectedly empty" table
        | TableHasMoreThanOneRow(table, count) ->
            sprintf "The %A metadata table was expected to have one row, but actual has %i rows" table count

[<RequireQualifiedAccess>]
module ReadError =
    let message (state: IReadState) (error: ReadError) { FileOffset = offset } =
        sprintf "Error occured while %O at offset 0x%X: %O" state offset error

exception ReadException
    of state: IReadState * error: ReadError * offset: FileOffset
    with override this.Message = ReadError.message this.state this.error this.offset

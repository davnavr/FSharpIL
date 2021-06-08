﻿[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.PortableExecutable

type [<Struct>] HeaderDataPointer<'Data> = { Offset: SectionOffset; [<DefaultValue(false)>] mutable Data: 'Data }

type ParsedMetadataStream<'Stream> = struct
    val Index: int32 voption
    [<DefaultValue(false)>] val mutable Stream: 'Stream
    new (index) = { Index = ValueSome index }
end

[<NoComparison; NoEquality>]
type CliInfo =
    { /// Offset from the start of the section to the first byte of the CLI header (II.25.3.3).
      CliHeaderOffset: SectionOffset
      SectionRva: Rva
      SectionOffset: FileOffset
      [<DefaultValue>] mutable CliHeader: ParsedCliHeader
      [<DefaultValue>] mutable CliMetadata: HeaderDataPointer<ChunkedMemory>
      [<DefaultValue>] mutable MetadataRoot: ParsedCliMetadataRoot
      [<DefaultValue>] mutable StreamHeaders: HeaderDataPointer<ImmutableArray<ParsedStreamHeader>>
      [<DefaultValue>] mutable StringsStream: ParsedMetadataStream<ParsedStringsStream>
      [<DefaultValue>] mutable GuidStream: ParsedMetadataStream<ParsedGuidStream>
      [<DefaultValue>] mutable UserStringStream: ParsedMetadataStream<ParsedUserStringStream>
      [<DefaultValue>] mutable BlobStream: ParsedMetadataStream<ParsedBlobStream>
      [<DefaultValue>] mutable TablesStream: ParsedMetadataStream<ChunkedMemory>
      [<DefaultValue>] mutable Tables: ParsedMetadataTables
      //[<DefaultValue>] mutable MyField: MyType
      }

let inline calculateFileOffset { CliInfo.SectionOffset = start } { SectionOffset.SectionOffset = soffset } = start + soffset

let readRvaAndSize offset (chunk: inref<_>) =
    { Rva = Rva(ChunkedMemory.readU4 offset &chunk)
      Size = ChunkedMemory.readU4 (offset + 4u) &chunk }

let readCliHeader (section: inref<ChunkedMemory>) info reader ustate =
    let inline failure err = Failure(info.CliHeaderOffset, err)
    let offset = info.CliHeaderOffset
    match ChunkedMemory.tryReadU4 (uint32 offset) &section with
    | ValueSome cb when cb >= Magic.cliHeaderSize ->
        match section.TrySlice(uint32 offset + 4u) with
        | true, fields ->
            info.CliHeader <-
                { Cb = cb
                  MajorRuntimeVersion = ChunkedMemory.readU2 0u &fields
                  MinorRuntimeVersion = ChunkedMemory.readU2 2u &fields
                  Metadata = readRvaAndSize 4u &fields
                  Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 12u &fields)
                  EntryPointToken = ChunkedMemory.readU4 16u &fields
                  Resources = readRvaAndSize 20u &fields
                  StrongNameSignature = readRvaAndSize 28u &fields
                  CodeManagerTable = readRvaAndSize 36u &fields
                  VTableFixups = readRvaAndSize 44u &fields
                  ExportAddressTableJumps = readRvaAndSize 52u &fields
                  ManagedNativeHeader = readRvaAndSize 60u &fields }
            StructureReader.read reader.ReadCliHeader info.CliHeader (calculateFileOffset info offset) ustate FindMetadataRoot
        | false, _ -> CliHeaderOutOfSection(info.SectionRva + info.CliHeaderOffset + 4u) |> failure
    | ValueSome cb -> failure(CliHeaderTooSmall cb)
    | ValueNone -> CliHeaderOutOfSection(info.SectionRva + info.CliHeaderOffset) |> failure

let readHeaderDataPointer (section: inref<ChunkedMemory>) sectionRva { Rva = rva; Size = size } (data: outref<_>) =
    if rva >= sectionRva then
        data <- { Offset = { SectionOffset = uint32(sectionRva - rva) } }
        if section.TrySlice(uint32 data.Offset, size, &data.Data)
        then None
        else Some(StructureOutOfBounds ParsedMetadataStructure.CliMetadataRoot)
    else Some(RvaNotInCliSection rva)

let findMetadataRoot (section: inref<ChunkedMemory>) info ustate =
    match readHeaderDataPointer &section info.SectionRva info.CliHeader.Metadata &info.CliMetadata with
    | None -> Success(ustate, ReadMetadataRoot)
    | Some err -> Failure(info.CliHeaderOffset + 8u, err) // Offset to MetaData field in CliHeader

let readMetadataSignature info =
    let magic = Span.stackalloc<byte> 4
    if info.CliMetadata.Data.TryCopyTo(0u, magic) then
        if Span.readOnlyEqual (Span.asReadOnly magic) (Magic.metadataRootSignature.AsSpan())
        then Ok(Bytes.toU4 0 magic)
        else Error(InvalidMagic(Magic.metadataRootSignature, Span.toBlock magic))
    else Error(StructureOutOfBounds ParsedMetadataStructure.MetadataSignature)

let inline missingNullTerminator (encoding: Encoding) (str: ReadOnlySpan<byte>) = MissingNullTerminator(encoding.GetString str)

let readMetadataVersion length offset (root: inref<ChunkedMemory>) =
    let version = root.Slice(offset, length).ToImmutableArray()
    match version.[version.Length - 1] with
    | 0uy -> Ok { RoundedLength = Checked.uint8 length; MetadataVersion = version }
    | _ -> Error(missingNullTerminator Encoding.UTF8 (version.AsSpan()))

/// Parses the CLI metadata root (II.24.2.1).
let readMetadataRoot info reader ustate =
    let inline failure err = Failure(info.CliMetadata.Offset, err)
    let root: inref<_> = &info.CliMetadata.Data
    match readMetadataSignature info with
    | Ok signature ->
        match ChunkedMemory.tryReadU4 12u &root with
        | ValueSome length when length > 255u || length % 4u <> 0u -> failure(InvalidMetadataVersionLength length)
        | ValueSome length when root.HasFreeBytes(0u, 20u + length) ->
            match readMetadataVersion length 16u &root with
            | Ok version ->
                info.MetadataRoot <-
                    { Signature = signature
                      MajorVersion = ChunkedMemory.readU2 4u &root
                      MinorVersion = ChunkedMemory.readU2 6u &root
                      Reserved = ChunkedMemory.readU4 8u &root
                      Version = version
                      Flags = ChunkedMemory.readU2 (16u + length) &root
                      Streams = ChunkedMemory.readU2 (18u + length) &root }
                info.StreamHeaders <- { Offset = { SectionOffset = 20u + length } }
                StructureReader.read
                    reader.ReadMetadataRoot
                    info.MetadataRoot
                    (calculateFileOffset info info.CliMetadata.Offset)
                    ustate
                    ReadStreamHeaders
            | Error err -> failure err
        | ValueSome _
        | ValueNone -> failure(StructureOutOfBounds ParsedMetadataStructure.CliMetadataRoot)
    | Error err -> failure err

let rec readStreamNameSegment (data: inref<ChunkedMemory>) (offset: SectionOffset) headeri i (buffer: Span<byte>) =
    if i < buffer.Length then
        if data.TryCopyTo(uint32 offset, buffer.Slice(i, 4)) then
            let i' = i + 4
            if buffer.[i + 3] = 0uy then
                buffer.Slice(0, i').ToArray()
                |> Convert.unsafeTo<_, ImmutableArray<byte>>
                |> Ok
            else readStreamNameSegment &data (offset + 4u) headeri i' buffer
        else Error(offset, StructureOutOfBounds(ParsedMetadataStructure.StreamHeader headeri))
    else Error(offset, missingNullTerminator Encoding.ASCII (Span.asReadOnly buffer))

let readStreamName (data: inref<ChunkedMemory>) (offset: SectionOffset) headeri =
    // According to (II.24.2.2), the name of the stream is limited to 32 characters.
    readStreamNameSegment &data offset headeri 0 (Span.stackalloc<byte> 32)

let rec readStreamHeadersLoop (section: inref<ChunkedMemory>) info (offset: SectionOffset) (headers: ParsedStreamHeader[]) i =
    if i <= headers.Length then
        let offset' = uint32 offset
        if section.HasFreeBytes(offset', 8u) then
            match readStreamName &section (offset + 8u) i with
            | Ok name ->
                headers.[i] <-
                    { Offset = MetadataRootOffset(ChunkedMemory.readU4 offset' &section)
                      Size = ChunkedMemory.readU4 (offset' + 4u) &section
                      StreamName = name }

                if name = Magic.StreamNames.strings then info.StringsStream <- ParsedMetadataStream i
                if name = Magic.StreamNames.guid then info.GuidStream <- ParsedMetadataStream i

                readStreamHeadersLoop &section info (offset + 8u + uint32 name.Length) headers (i + 1)
            | Error err -> Some err
        else Some(offset, StructureOutOfBounds(ParsedMetadataStructure.StreamHeader i))
    else None

/// Parses the stream headers of the CLI metadata root (II.24.2.2).
let readStreamHeaders (section: inref<ChunkedMemory>) info reader ustate =
    let mutable headers = Unsafe.As &info.StreamHeaders
    headers <- Array.zeroCreate(int32 info.MetadataRoot.Streams)
    match readStreamHeadersLoop &section info info.StreamHeaders.Offset headers 0 with
    | None ->
        StructureReader.read
            reader.ReadStreamHeaders
            info.StreamHeaders.Data
            (calculateFileOffset info info.StreamHeaders.Offset)
            ustate
            ReadStringsStream
    | Some err -> Failure err

/// Turns an offset from the start of the CLI metadata root to an offset from the start of the section.
let inline offsetFromRoot info (offset: MetadataRootOffset) = info.CliMetadata.Offset + uint32 offset

let inline createMetadataStream stream = Result<_, ReadError>.Ok(^Stream : (new : ChunkedMemory -> ^Stream) stream)

let validateMetadataStream (section: inref<ChunkedMemory>) info streami ctor =
    match streami with
    | ValueSome i ->
        let header = &info.StreamHeaders.Data.ItemRef i
        let offset = offsetFromRoot info header.Offset
        match section.TrySlice(uint32 offset, header.Size) with
        | true, data ->
            match ctor data with
            | Ok stream -> Ok(struct(calculateFileOffset info offset, stream))
            | Error err -> Error(offset, err)
        | false, _ -> Error(offset, StreamOutOfBounds(i, header))
    | ValueNone -> Ok(struct(FileOffset.Zero, new 'Stream()))

let readMetadataStream (section: inref<_>) info (stream: byref<ParsedMetadataStream<_>>) ctor reader ustate next =
    match validateMetadataStream &section info stream.Index ctor with
    | Ok(offset, stream') ->
        stream.Stream <- stream'
        StructureReader.read reader stream' offset ustate next
    | Error err -> Failure err

type CliInfo with
    member inline this.TablesOffset = offsetFromRoot this (this.StreamHeaders.Data.ItemRef(this.TablesStream.Index.Value).Offset)

/// Size of the first 7 fields of the metadata tables header, in bytes (II.24.2.6).
let tablesHeaderFieldsSize = 24u

let rec createTableRowCounts (lookup: 'Lookup) (valid: ValidTableFlags) (counts: uint32[]) counti validi =
    match valid with
    | ValidTableFlags.None -> System.Collections.ObjectModel.ReadOnlyDictionary lookup
    | _ ->
        let counti' =
            if valid.HasFlag ValidTableFlags.Module then
                lookup.[ValidTableFlags.Module <<< validi] <- counts.[counti]
                counti + 1
            else counti
        createTableRowCounts lookup (valid >>> 1) counts counti' (validi + 1)

let readMetadataTables (info: CliInfo) =
    let inline outOfBounds offset =
        Some(info.TablesOffset + offset, StructureOutOfBounds ParsedMetadataStructure.MetadataTablesHeader)
    let stream: inref<_> = &info.TablesStream.Stream
    if stream.HasFreeBytes(0u, tablesHeaderFieldsSize) then
        let valid = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU8 8u &stream)
        let numRowCounts =
            let rec inline inner valid count =
                match valid with
                | ValidTableFlags.None -> count
                | _ ->
                    let count' =
                        if valid.HasFlag ValidTableFlags.Module
                        then count + 1
                        else count
                    inner (valid >>> 1) count'
            inner valid 0
        let tableRowsOffset = tablesHeaderFieldsSize + (uint32 numRowCounts * 4u)
        match stream.TrySlice tableRowsOffset with
        | true, rows ->
            let rcounts = Array.zeroCreate<uint32> numRowCounts
            for rowi = 0 to numRowCounts - 1 do
                rcounts.[rowi] <- ChunkedMemory.readU4 (tablesHeaderFieldsSize + (uint32 rowi * 4u)) &rows

            let header =
                { Reserved1 = ChunkedMemory.readU4 0u &stream
                  MajorVersion = stream.[4u]
                  MinorVersion = stream.[5u]
                  HeapSizes = LanguagePrimitives.EnumOfValue stream.[6u]
                  Reserved2 = stream.[7u]
                  Valid = valid
                  Sorted = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU8 16u &stream)
                  Rows = createTableRowCounts (Dictionary numRowCounts) valid rcounts 0 0 :> IReadOnlyDictionary<_, _> }

            match ParsedMetadataTables.tryCreate &rows header with
            | Ok tables ->
                info.Tables <- tables
                None
            | Error(offset, err) -> Some(info.TablesOffset + tableRowsOffset + offset, err)
        | false, _ -> outOfBounds tablesHeaderFieldsSize
    else outOfBounds 0u

let rec readTableRowsLoop (roffset: FileOffset) (table: MetadataTableParser<_, _>) referenced i reader (ustate: byref<_>) =
    if i <= table.RowCount then
        match reader (struct(referenced, table.[{ TableIndex = i }])) (roffset + table.TableOffset) ustate with
        | ValueSome ustate' ->
            ustate <- ustate'
            readTableRowsLoop roffset table referenced (i + 1u) reader &ustate
        | ValueNone -> ()

let readTablesSequential info reader ustate =
    let mutable ustate' = ustate

    let referenced =
        { ReferencedMetadataStreams.Tables = info.Tables
          Strings = info.StringsStream.Stream
          Guid = info.GuidStream.Stream
          UserString = info.UserStringStream.Stream
          Blob = info.BlobStream.Stream }
    let roffset =
        calculateFileOffset info (info.TablesOffset + tablesHeaderFieldsSize + (uint32 info.Tables.Header.Rows.Count * 4u))

    let inline readTableRows table (reader': TableRowReader<_, _>) =
        match reader' with
        | ValueSome reader' -> readTableRowsLoop roffset table referenced 1u reader' &ustate'
        | ValueNone -> ()

    readTableRows info.Tables.moduleTable reader.ReadModule
    readTableRows info.Tables.typeRefTable reader.ReadTypeRef
    readTableRows info.Tables.typeDefTable reader.ReadTypeDef
    readTableRows info.Tables.fieldTable reader.ReadField
    readTableRows info.Tables.methodDefTable reader.ReadMethodDef
    readTableRows info.Tables.paramTable reader.ReadParam
    readTableRows info.Tables.interfaceImplTable reader.ReadInterfaceImpl
    readTableRows info.Tables.memberRefTable reader.ReadMemberRef
    readTableRows info.Tables.constantTable reader.ReadConstant
    readTableRows info.Tables.customAttributeTable reader.ReadCustomAttribute

    readTableRows info.Tables.classLayoutTable reader.ReadClassLayout

    readTableRows info.Tables.standAloneSigTable reader.ReadStandaloneSig

    readTableRows info.Tables.propertyMapTable reader.ReadPropertyMap
    readTableRows info.Tables.propertyTable reader.ReadProperty
    readTableRows info.Tables.methodSemanticsTable reader.ReadMethodSemantics
    readTableRows info.Tables.methodImplTable reader.ReadMethodImpl

    readTableRows info.Tables.typeSpecTable reader.ReadTypeSpec

    readTableRows info.Tables.fieldRvaTable reader.ReadFieldRva
    readTableRows info.Tables.assemblyTable reader.ReadAssembly
    readTableRows info.Tables.assemblyRefTable reader.ReadAssemblyRef

    readTableRows info.Tables.manifestResourceTable reader.ReadManifestResource
    readTableRows info.Tables.nestedClassTable reader.ReadNestedClass
    readTableRows info.Tables.genericParamTable reader.ReadGenericParam
    readTableRows info.Tables.methodSpecTable reader.ReadMethodSpec
    readTableRows info.Tables.genericParamConstraintTable reader.ReadGenericParamConstraint

    Success(ustate', MetadataReadFinished)

let readMetadata (section: inref<ChunkedMemory>) info reader ustate rstate =
    match rstate with
    | ReadCliHeader -> readCliHeader &section info reader ustate
    | FindMetadataRoot -> findMetadataRoot &section info ustate
    | ReadMetadataRoot -> readMetadataRoot info reader ustate
    | ReadStreamHeaders ->
        match reader, info.MetadataRoot.Streams with
        // TODO: Consider returning an error if other reading functions depend on contents of stream or stream headers, but there are no streams defined.
        | _, 0us -> End
        | _, _ -> readStreamHeaders &section info reader ustate
    | ReadStringsStream ->
        readMetadataStream
            &section
            info
            &info.StringsStream
            ParsedStringsStream.tryCreate
            reader.ReadStringsStream
            ustate
            ReadGuidStream
    | ReadGuidStream ->
        readMetadataStream
            &section
            info
            &info.GuidStream
            createMetadataStream
            reader.ReadGuidStream
            ustate
            ReadUserStringStream
    | ReadUserStringStream ->
        readMetadataStream
            &section
            info
            &info.UserStringStream
            createMetadataStream
            reader.ReadUserStringStream
            ustate
            ReadBlobStream
    | ReadBlobStream ->
        readMetadataStream
            &section
            info
            &info.BlobStream
            createMetadataStream
            reader.ReadBlobStream
            ustate
            ReadTablesStream
    | ReadTablesStream ->
        match validateMetadataStream &section info info.TablesStream.Index Ok with
        | Ok(_, stream') ->
            info.TablesStream.Stream <- stream'

            match reader with
            | { ReadTables = ValueNone } -> End
            | { ReadTables = ValueSome _ } when stream'.IsEmpty -> noImpl "error for when metadata stream does not exist."
            | _ -> Success(ustate, ReadMetadataTables)
        | Error err -> Failure err
    | ReadMetadataTables ->
        match readMetadataTables info with
        | None -> Success(ustate, ReadTableRows)
        | Some err -> Failure err
    | ReadTableRows ->
        match reader.ReadTables with
        | ValueSome(SequentialTableReader treader) -> readTablesSequential info treader ustate
        | ValueNone -> End
    | MetadataReadFinished -> End

let rec readMetadataLoop (section: inref<_>) info reader ustate rstate =
    match readMetadata &section info reader ustate rstate with
    | Success(ustate', rstate') -> readMetadataLoop &section info reader ustate' rstate'
    | Failure(soffset, err) ->
        ErrorHandler.handle rstate err (calculateFileOffset info soffset) ustate reader.HandleError
    | End -> ustate

let fromChunkedMemory (section: inref<_>) sectionRva sectionOffset cliHeaderOffset state reader =
    readMetadataLoop
        &section
        { CliHeaderOffset = cliHeaderOffset
          SectionRva = sectionRva
          SectionOffset = sectionOffset }
        reader
        state
        ReadCliHeader

/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.PortableExecutable
open FSharpIL.Reading

[<IsReadOnly; Struct>]
type ReadResult<'State, 'T, 'Error when 'State : struct> =
    | Success of 'T * 'State
    | Failure of 'Error
    | End

[<Sealed>]
type Reader (src: Stream) =
    let mutable pos = 0u
    do if not src.CanRead then invalidArg "src" "The stream must support reading"

    member _.Offset = { FileOffset = pos }

    member _.ReadByte() =
        pos <- pos + 1u
        src.ReadByte()

    member _.ReadBytes(buffer: Span<byte>) =
        let read = src.Read buffer
        pos <- pos + uint32 read
        read

    member _.SkipBytes(count: uint32) =
        let buf = Span.stackalloc<byte> 1
        let mutable cont, skipped = true, 0u
        while cont && skipped < count do
            match src.Read buf with
            | 0 -> cont <- false
            | _ -> skipped <- skipped + 1u
        pos <- pos + skipped
        skipped

    member this.MoveTo { FileOffset = offset } =
        if offset < pos then
            Error(CannotMoveToPreviousOffset this.Offset)
        elif offset = pos then
            Ok()
        else
            let diff = offset - pos
            if this.SkipBytes diff = diff
            then Ok()
            else Error UnexpectedEndOfFile

    /// Reads an unsigned, little-endian, 4-byte integer.
    member inline this.ReadU4(value: outref<uint32>) =
        let bytes = Span.stackalloc<byte> 4
        match this.ReadBytes bytes with
        | 4 ->
            value <- Bytes.toU4 0 bytes
            true
        | _ -> false

[<NoComparison; NoEquality>]
type MutableFile =
    { mutable Lfanew: uint32
      [<DefaultValue>] mutable CoffHeader: ParsedCoffHeader
      [<DefaultValue>] mutable StandardFields: ParsedStandardFields
      [<DefaultValue>] mutable NTSpecificFields: ParsedNTSpecificFields
      [<DefaultValue>] mutable DataDirectories: RvaAndSize[]
      [<DefaultValue>] mutable SectionHeaders: SectionHeader[]
      [<DefaultValue>] mutable TextSectionIndex: int32
      [<DefaultValue>] mutable TextSectionData: ChunkedMemory
      /// Offset from start of section to the CLI header (II.25.3.3).
      [<DefaultValue>] mutable CliHeaderOffset: uint32
      [<DefaultValue>] mutable CliHeader: ParsedCliHeader
      /// Offset from start of section to the CLI metadata root (II.24.2.1).
      [<DefaultValue>] mutable MetadataRootOffset: uint32
      [<DefaultValue>] mutable MetadataRoot: ParsedMetadataRoot
      /// Offset from start of section to the first byte of the CLI metadata stream headers (II.24.2.2).
      [<DefaultValue>] mutable StreamHeadersOffset: uint32
      [<DefaultValue>] mutable StreamHeaders: ParsedStreamHeader[]
      [<DefaultValue>] mutable StringsStreamIndex: int32 voption
      [<DefaultValue>] mutable StringsStream: ParsedStringsStream
      [<DefaultValue>] mutable GuidStreamIndex: int32 voption
      [<DefaultValue>] mutable GuidStream: ParsedGuidStream
      [<DefaultValue>] mutable UserStringStreamIndex: int32 voption
      [<DefaultValue>] mutable UserStringStream: ParsedUserStringStream
      [<DefaultValue>] mutable BlobStreamIndex: int32 voption
      [<DefaultValue>] mutable BlobStream: ParsedBlobStream
      [<DefaultValue>] mutable MetadataTablesIndex: int32 voption
      [<DefaultValue>] mutable MetadataTablesHeader: ParsedMetadataTablesHeader
      /// Offset from start of section to the first byte of the physical representation of the metadata tables (II.22.1).
      [<DefaultValue>] mutable MetadataTablesOffset: uint32
      [<DefaultValue>] mutable MetadataTables: ParsedMetadataTables }

    /// <summary>File offset to the first byte of the <c>.text</c> section.</summary>
    member this.TextSectionOffset = { FileOffset = this.SectionHeaders.[this.TextSectionIndex].RawDataPointer }

let inline rvaAndSize i buffer = { Rva = Rva(Bytes.toU4 i buffer); Size = Bytes.toU4 (i + 4) buffer }

// TODO: Fix, offset will always point after the end of the structure.
let readCoffHeader (src: Reader) (headers: outref<_>) reader ustate =
    let buffer = Span.stackalloc<byte>(int32 Magic.coffHeaderSize)
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        headers <-
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.toU2 0 buffer)
              NumberOfSections = Bytes.toU2 2 buffer
              TimeDateStamp = Bytes.toU4 4 buffer
              SymbolTablePointer = Bytes.toU4 8 buffer
              SymbolCount = Bytes.toU4 12 buffer
              OptionalHeaderSize = Bytes.toU2 16 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.toU2 18 buffer) }
        Success(MetadataReader.read reader.ReadCoffHeader headers offset ustate, ReadStandardFields)
    else Failure UnexpectedEndOfFile

let readStandardFields (src: Reader) (fields: outref<_>) reader ustate =
    let buffer = Span.stackalloc<byte> 28
    let offset = src.Offset
    if src.ReadBytes(buffer.Slice(0, 2)) = 2 then
        let magic = LanguagePrimitives.EnumOfValue(Bytes.toU2 0 buffer)
        let length = if magic = ImageKind.PE32 then 26 else 22
        let fields' = buffer.Slice(2, length)
        if src.ReadBytes fields' = length then
            fields <-
                { Magic = magic
                  LMajor = fields'.[0]
                  LMinor = fields'.[1]
                  CodeSize = Bytes.toU4 2 fields'
                  InitializedDataSize = Bytes.toU4 6 fields'
                  UninitializedDataSize = Bytes.toU4 10 fields'
                  EntryPointRva = Bytes.toU4 14 fields'
                  BaseOfCode = Bytes.toU4 18 fields'
                  BaseOfData =
                    match magic with
                    | ImageKind.PE32 -> ValueSome(Bytes.toU4 22 fields')
                    | _ -> ValueNone }
            Success(MetadataReader.read reader.ReadStandardFields fields offset ustate, ReadNTSpecificFields)
        else Failure UnexpectedEndOfFile
    else Failure UnexpectedEndOfFile

let readNTSpecificFields (src: Reader) magic (fields: outref<_>) reader ustate =
    let length =
        match magic with
        | ImageKind.PE32Plus -> 88
        | _ -> 68
    let buffer = Span.heapalloc<byte> length
    let offset = src.Offset
    if src.ReadBytes buffer = length then
        let numdirs = Bytes.toU4 64 buffer
        if numdirs >= 15u then
            fields <-
                match magic with
                | ImageKind.PE32Plus -> invalidOp "PE32+ not yet supported"
                | _ ->
                    { ImageBase = uint64(Bytes.toU4 0 buffer)
                      // TODO: Validate alignment values, maybe use Alignment type?
                      Alignment = Bytes.toU4 4 buffer, Bytes.toU4 8 buffer
                      OSMajor = Bytes.toU2 12 buffer
                      OSMinor = Bytes.toU2 14 buffer
                      UserMajor = Bytes.toU2 16 buffer
                      UserMinor = Bytes.toU2 18 buffer
                      SubSysMajor = Bytes.toU2 20 buffer
                      SubSysMinor = Bytes.toU2 22 buffer
                      Win32VersionValue = Bytes.toU4 24 buffer
                      ImageSize = Bytes.toU4 28 buffer
                      HeadersSize = Bytes.toU4 32 buffer
                      FileChecksum = Bytes.toU4 36 buffer
                      Subsystem = LanguagePrimitives.EnumOfValue(Bytes.toU2 40 buffer)
                      DllFlags = LanguagePrimitives.EnumOfValue(Bytes.toU2 42 buffer)
                      StackReserveSize = uint64(Bytes.toU4 44 buffer)
                      StackCommitSize = uint64(Bytes.toU4 48 buffer)
                      HeapReserveSize = uint64(Bytes.toU4 52 buffer)
                      HeapCommitSize = uint64(Bytes.toU4 56 buffer)
                      LoaderFlags = Bytes.toU4 60 buffer
                      NumberOfDataDirectories = Bytes.toU4 64 buffer }
            Success(MetadataReader.read reader.ReadNTSpecificFields fields offset ustate, ReadDataDirectories)
        else Failure(TooFewDataDirectories numdirs)
    else Failure UnexpectedEndOfFile

let readDataDirectories (src: Reader) (count: uint32) (directories: outref<_>) reader ustate =
    let count' = int32 count
    let buffer = Span.heapalloc<byte>(count' * 8)
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        directories <- Array.zeroCreate count'
        for i = 0 to count' - 1 do
            let i' = i * 8
            directories.[i] <- rvaAndSize i' buffer
        Success(MetadataReader.read reader.ReadDataDirectories (Unsafe.As &directories) offset ustate, ReadSectionHeaders)
    else Failure UnexpectedEndOfFile

// TODO: Read section headers one at a time instead of looping through them twice.
let readSectionHeaders (src: Reader) (count: uint16) (file: MutableFile) reader ustate =
    let count' = int32 count
    let buffer = Span.heapalloc<byte>(count' * int32 Magic.sectionHeaderSize)
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        file.SectionHeaders <- Array.zeroCreate<_> count'
        for i = 0 to count' - 1 do
            let i' = i * (int32 Magic.sectionHeaderSize)
            let name = SectionName(buffer.Slice(i', 8).ToArray())
            if name = SectionName.text then file.TextSectionIndex <- i
            file.SectionHeaders.[i] <-
                { SectionName = name
                  VirtualSize = Bytes.toU4 (i' + 8) buffer
                  VirtualAddress = Rva(Bytes.toU4 (i' + 12) buffer)
                  RawDataSize = Bytes.toU4 (i' + 16) buffer
                  RawDataPointer = Bytes.toU4 (i' + 20) buffer
                  PointerToRelocations = Bytes.toU4 (i' + 24) buffer
                  PointerToLineNumbers = Bytes.toU4 (i' + 28) buffer
                  NumberOfRelocations = Bytes.toU2 (i' + 32) buffer
                  NumberOfLineNumbers = Bytes.toU2 (i' + 34) buffer
                  Characteristics = LanguagePrimitives.EnumOfValue(Bytes.toU4 (i' + 36) buffer) }
        Success(MetadataReader.read reader.ReadSectionHeaders (Unsafe.As &file.SectionHeaders) offset ustate, MoveToTextSectionData)
    else Failure UnexpectedEndOfFile

let rec readSectionData (data: byref<byte[][]>) (src: Reader) file i size =
    match size with
    | 0u ->
        file.TextSectionData <- ChunkedMemory.ofArrayUnsafe &data
        End
    | _ ->
        let len = min (snd file.NTSpecificFields.Alignment) size
        let len' = int32 len
        let chunk = Array.zeroCreate len'
        if src.ReadBytes(Span chunk) = len' then
            data.[i] <- chunk
            readSectionData &data src file (i + 1) (size - len)
        else Failure UnexpectedEndOfFile

let readTextSection (src: Reader) (file: MutableFile) =
    let vsize = file.SectionHeaders.[file.TextSectionIndex].VirtualSize
    let mutable data =
        let falignment = snd file.NTSpecificFields.Alignment
        let length = vsize / falignment
        if falignment * length < vsize
        then int32 length + 1
        else int32 length
        |> Array.zeroCreate
    readSectionData &data src file 0 vsize

let readPE (src: Reader) file reader ustate rstate =
    let inline moveto target state' =
        match src.MoveTo { FileOffset = target } with
        | Ok() -> Success(ustate, state')
        | Error err -> Failure err
    let inline magic (expected: ImmutableArray<byte>) next =
        let actual = Span.stackalloc<byte> expected.Length
        match src.ReadBytes actual with
        | 0 -> Failure UnexpectedEndOfFile
        | len when len = expected.Length && expected.AsSpan() = Span.asReadOnly actual -> Success(ustate, next)
        | len -> InvalidMagic(expected, Span.toBlock(actual.Slice(0, len))) |> Failure
    match rstate with
    | ReadPEMagic -> magic Magic.dosHeaderSignature MoveToLfanew
    | MoveToLfanew -> moveto 0x3Cu ReadLfanew
    | ReadLfanew ->
        let offset = src.Offset
        if src.ReadU4(&file.Lfanew) then
            Success(MetadataReader.read reader.ReadLfanew file.Lfanew offset ustate, MoveToPESignature)
        else Failure UnexpectedEndOfFile
    | MoveToPESignature -> moveto file.Lfanew ReadPESignature
    | ReadPESignature -> magic Magic.portableExecutableSignature ReadCoffHeader
    | ReadCoffHeader ->
        // TODO: Check for PE32 or PE32+ files.
        match readCoffHeader src &file.CoffHeader reader ustate with
        | Success _ when file.CoffHeader.OptionalHeaderSize < Magic.optionalHeaderSize -> // TODO: How to stop reading optional fields according to size of optional header?
            Failure(OptionalHeaderTooSmall file.CoffHeader.OptionalHeaderSize)
        | result -> result
    | ReadStandardFields ->
        readStandardFields src &file.StandardFields reader ustate
    | ReadNTSpecificFields ->
        readNTSpecificFields src file.StandardFields.Magic &file.NTSpecificFields reader ustate
    | ReadDataDirectories ->
        readDataDirectories src file.NTSpecificFields.NumberOfDataDirectories &file.DataDirectories reader ustate
    | ReadSectionHeaders ->
        readSectionHeaders src file.CoffHeader.NumberOfSections file reader ustate
    | MoveToTextSectionData -> moveto file.SectionHeaders.[file.TextSectionIndex].RawDataPointer ReadTextSectionData
    | ReadTextSectionData -> readTextSection src file

/// <summary>Calculates a file offset from an RVA pointing to a location within the <c>.text</c> section.</summary>
let findTextOffset (text: SectionHeader) { RvaAndSize.Rva = rva } (offset: byref<uint32>) rstate ustate =
    if SectionHeader.contains rva text then
        offset <- uint32(rva - text.VirtualAddress)
        Success(ustate, rstate)
    else Failure({ FileOffset = text.RawDataPointer }, RvaNotInTextSection rva)

let inline rvaAndSize' i (chunk: inref<ChunkedMemory>) =
    { Rva = ChunkedMemory.readU4 i &chunk; Size = ChunkedMemory.readU4 (i + 4u) &chunk }

let readCliHeader (chunk: inref<ChunkedMemory>) file reader ustate =
    let foffset = file.CliHeaderOffset + file.TextSectionOffset
    match ByteParser.tempParseChunkStruct<ParseU4, _> file.CliHeaderOffset &chunk with
    | ValueSome size when size < Magic.cliHeaderSize -> Failure(foffset, CliHeaderTooSmall size)
    | ValueSome size ->
        let offset = file.CliHeaderOffset + 4u
        match chunk.TrySlice(offset, size) with
        | true, fields ->
            file.CliHeader <-
                { Size = size
                  MajorRuntimeVersion = ChunkedMemory.readU2 0u &fields
                  MinorRuntimeVersion = ChunkedMemory.readU2 2u &fields
                  MetaData = rvaAndSize' 4u &fields
                  Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 12u &fields)
                  EntryPointToken = ChunkedMemory.readU4 16u &fields
                  Resources = rvaAndSize' 20u &fields
                  StrongNameSignature = ChunkedMemory.readU8 28u &fields
                  CodeManagerTable = ChunkedMemory.readU8 36u &fields
                  VTableFixups = rvaAndSize' 44u &fields
                  ExportAddressTableJumps = ChunkedMemory.readU8 52u &fields
                  ManagedNativeHeader = ChunkedMemory.readU8 60u &fields }
            Success (
                MetadataReader.read reader.ReadCliHeader file.CliHeader foffset ustate,
                FindMetadataRoot
            )
        | false, _ -> Failure(file.TextSectionOffset + offset, StructureOutsideOfCurrentSection ParsedStructure.CliHeader)
    | _ -> Failure(foffset, StructureOutsideOfCurrentSection ParsedStructure.CliHeader)

let readMetadataVersion (version: inref<ChunkedMemory>) =
    let mutable cont, i = true, version.Length - 1u
    while cont && i >= 0u do
        if version.[i] > 0uy
        then cont <- false
        else i <- i - 1u
    if cont
    then ValueNone
    else
        let version' = Array.zeroCreate<byte>(int32 i + 1)
        let mutable i' = 0u
        while i' < i do
            version'.[int32 i'] <- version.[i']
            i' <- i' + 1u
        ValueSome(MetadataVersion(uint8 i + 1uy, version'))

let readMetadataSignature (chunk: inref<ChunkedMemory>) file ustate =
    let foffset = file.MetadataRootOffset + file.TextSectionOffset
    match chunk.TrySlice(file.MetadataRootOffset, 4u) with
    | true, signature when signature.Equals Magic.CliSignature -> Success(ustate, ReadMetadataRoot)
    | true, signature -> Failure(foffset, InvalidMagic(Magic.CliSignature, signature.ToImmutableArray()))
    | false, _ -> Failure(foffset, StructureOutsideOfCurrentSection ParsedStructure.MetadataSignature)

/// Parses the CLI metadata root (II.24.2.1).
let readMetadataRoot (chunk: inref<ChunkedMemory>) file reader ustate =
    let offset = file.MetadataRootOffset + 4u
    let foffset = file.TextSectionOffset + offset
    match chunk.TrySlice(offset, 12u) with
    | true, buffer -> // First 12 bytes are for MajorVersion, MinorVersion, Reserved and Length
        let length = ChunkedMemory.readU4 8u &buffer
        let offset' = offset + 12u
        let foffset' = file.TextSectionOffset + offset'
        match chunk.TrySlice(offset', length) with
        | true, version ->
            match readMetadataVersion &version, chunk.TrySlice(offset' + length, 4u) with
            | ValueSome version', (true, fields) -> // Last 4 bytes are for Flags and Streams
                let nstreams = ChunkedMemory.readU2 2u &fields
                file.StreamHeadersOffset <- offset' + length + 4u
                file.StreamHeaders <- Array.zeroCreate(int32 nstreams)
                file.MetadataRoot <-
                    { MajorVersion = ChunkedMemory.readU2 0u &buffer
                      MinorVersion = ChunkedMemory.readU2 2u &buffer
                      Reserved = ChunkedMemory.readU4 4u &buffer
                      Version = version'
                      Flags = ChunkedMemory.readU2 0u &fields
                      Streams = nstreams }
                Success (
                    MetadataReader.read reader.ReadMetadataRoot file.MetadataRoot foffset ustate,
                    ReadStreamHeaders
                )
            | ValueNone, _ -> Failure(foffset', MetadataVersionHasNoNullTerminator(version.ToImmutableArray()))
            | _, (false, _) -> Failure(foffset' + length, UnexpectedEndOfFile)
        | false, _ -> Failure(foffset, UnexpectedEndOfFile)
    | false, _ -> Failure(foffset, UnexpectedEndOfFile)

let rec streamNameSegment offset i (chunk: inref<ChunkedMemory>) (buffer: Span<byte>) =
    let offset' = offset + uint32 i
    if i < buffer.Length then
        if chunk.TryCopyTo(offset', buffer.Slice(i, 4)) then
            let i' = i + 4
            if buffer.[i + 3] = 0uy then
                let mutable name = buffer.Slice(0, i').ToArray()
                Ok(Unsafe.As &name)
            else streamNameSegment offset i' &chunk buffer
        else Error UnexpectedEndOfFile
    else Error(MissingNullTerminator(Encoding.ASCII.GetString(Span.asReadOnly buffer)))

/// Reads the name of a metadata stream (II.24.2.2).
let readStreamName (chunk: inref<ChunkedMemory>) offset =
    // According to (II.24.2.2), the name of the stream is limited to 32 characters.
    let buffer = Span.stackalloc<byte> 32
    streamNameSegment offset 0 &chunk buffer

let readStreamHeaders (chunk: inref<ChunkedMemory>) file reader ustate =
    let fields = Span.stackalloc<byte> 8
    let mutable i, offset, ustate', err = 0, file.StreamHeadersOffset, ustate, None
    // Workaround InvalidProgramException by explicitly using a while loop instead of a recursive function.
    while i < file.StreamHeaders.Length && err.IsNone do
        if chunk.TryCopyTo(offset, fields) then
            offset <- offset + 8u
            match readStreamName &chunk offset with
            | Ok name ->
                let header =
                    { Offset = Bytes.toU4 0 fields
                      Size = Bytes.toU4 4 fields
                      Name = name }
                ustate' <- MetadataReader.readStreamHeader reader header i (file.TextSectionOffset + offset) ustate'

                if name = Magic.MetadataStream then file.MetadataTablesIndex <- ValueSome i
                if name = Magic.StringStream then file.StringsStreamIndex <- ValueSome i
                if name = Magic.GuidStream then file.GuidStreamIndex <- ValueSome i
                if name = Magic.UserStringStream then file.UserStringStreamIndex <- ValueSome i
                if name = Magic.BlobStream then file.BlobStreamIndex <- ValueSome i

                file.StreamHeaders.[i] <- header
                offset <- offset + uint32 name.Length
                i <- i + 1
            | Error err' -> err <- Some err'
        else err <- Some(StructureOutsideOfCurrentSection(ParsedStructure.StreamHeader i))
    match err with
    | None -> Success(ustate', ReadStringsStream)
    | Some err' -> Failure(file.TextSectionOffset + offset, err')

let readMetadataHeap i (chunk: inref<ChunkedMemory>) file (heap: outref<_>) =
    let header: inref<_> = &file.StreamHeaders.[i]
    let offset = file.TextSectionOffset + file.MetadataRootOffset + uint64 header.Offset
    let success = ParsedMetadataStream.ofHeader file.MetadataRootOffset &chunk header &heap
    struct(success, offset, header.Size)

let readStringsHeap (chunk: inref<ChunkedMemory>) file reader ustate =
    match file.StringsStreamIndex with
    | ValueSome i ->
        let mutable heap = Unchecked.defaultof<_>
        match readMetadataHeap i &chunk file &heap with
        | true, offset, _ ->
            file.StringsStream <- ParsedStringsStream heap
            Success (
                MetadataReader.read
                    reader.ReadStringsStream
                    file.StringsStream
                    offset
                    ustate,
                ReadGuidStream
            )
        | false, offset, size -> Failure(offset, StructureOutsideOfCurrentSection(ParsedStructure.StringHeap size))
    | ValueNone -> Success(ustate, ReadGuidStream)

let readGuidHeap (chunk: inref<ChunkedMemory>) file reader ustate =
    match file.GuidStreamIndex with
    | ValueSome i ->
        let header: inref<_> = &file.StreamHeaders.[i]
        let offset = file.MetadataRootOffset + header.Offset
        let foffset = file.TextSectionOffset + offset
        match chunk.TrySlice(offset, header.Size) with
        | true, guids ->
            file.GuidStream <- ParsedGuidStream guids
            Success(MetadataReader.read reader.ReadGuidStream file.GuidStream foffset ustate, ReadUserStringStream)
        | false, _ -> Failure(foffset, StructureOutsideOfCurrentSection(ParsedStructure.GuidHeap header.Size))
    | ValueNone -> Success(ustate, ReadUserStringStream)

let readUserStringHeap (chunk: inref<ChunkedMemory>) file reader ustate =
    match file.StringsStreamIndex with
    | ValueSome i ->
        let mutable heap = Unchecked.defaultof<_>
        match readMetadataHeap i &chunk file &heap with
        | true, offset, _ ->
            file.UserStringStream <- ParsedUserStringStream heap
            Success (
                MetadataReader.read
                    reader.ReadUserStringStream
                    file.UserStringStream
                    offset
                    ustate,
                ReadBlobStream
            )
        | false, offset, size -> Failure(offset, StructureOutsideOfCurrentSection(ParsedStructure.UserStringHeap size))
    | ValueNone -> Success(ustate, ReadBlobStream)

let readBlobHeap (chunk: inref<ChunkedMemory>) file reader ustate =
    match file.BlobStreamIndex with
    | ValueSome i ->
        // TODO: Reduce code duplication with readGuidHeap and readMetadataHeap
        let header: inref<_> = &file.StreamHeaders.[i]
        let offset = file.MetadataRootOffset + header.Offset
        let foffset = file.TextSectionOffset + offset
        match chunk.TrySlice(offset, header.Size) with
        | true, blobs ->
            file.BlobStream <- ParsedBlobStream blobs
            Success (
                MetadataReader.read
                    reader.ReadBlobStream
                    file.BlobStream
                    foffset
                    ustate,
                ReadMetadataTablesHeader
            )
        | false, _ -> Failure(foffset, StructureOutsideOfCurrentSection(ParsedStructure.BlobHeap header.Size))
    | ValueNone -> Success(ustate, ReadMetadataTablesHeader)

/// Calculates the number of row counts for the valid metadata tables.
let rec tableRowsCount (valid: Metadata.Tables.ValidTableFlags) count =
    match valid with
    | ValidTableFlags.None -> count
    | _ ->
        let count' =
            if valid.HasFlag ValidTableFlags.Module
            then count + 1
            else count
        tableRowsCount (valid >>> 1) count'

let rec readTableCounts (lookup: 'Lookup) (valid: ValidTableFlags) (counts: uint32[]) counti validi =
    match valid with
    | ValidTableFlags.None -> System.Collections.ObjectModel.ReadOnlyDictionary lookup
    | _ ->
        let counti' =
            if valid.HasFlag ValidTableFlags.Module then
                lookup.[ValidTableFlags.Module <<< validi] <- counts.[counti]
                counti + 1
            else counti
        readTableCounts lookup (valid >>> 1) counts counti' (validi + 1)

let readTablesHeader (chunk: inref<ChunkedMemory>) (file: MutableFile) offset =
    let offset' = offset + file.TextSectionOffset
    match chunk.TrySlice(offset, 24u) with
    | true, fields ->
        let valid = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU8 8u &fields)
        let tablen = tableRowsCount valid 0
        match chunk.TrySlice(offset + fields.Length, uint32 tablen * 4u) with
        | true, rows ->
            let mutable rows' = Array.zeroCreate tablen
            for rowi = 0 to tablen - 1 do
                rows'.[rowi] <- ChunkedMemory.readU4 (uint32 rowi * 4u) &rows

            file.MetadataTablesHeader <-
                { Reserved1 = ChunkedMemory.readU4 0u &fields
                  MajorVersion = fields.[4u]
                  MinorVersion = fields.[5u]
                  HeapSizes = LanguagePrimitives.EnumOfValue fields.[6u]
                  Reserved2 = fields.[7u]
                  Valid = valid
                  Sorted = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU8 16u &fields)
                  Rows = readTableCounts (Dictionary tablen) valid rows' 0 0 }

            file.MetadataTablesOffset <- offset + fields.Length + rows.Length

            Ok ReadMetadataTables
        | false, _ -> Error(offset', StructureOutsideOfCurrentSection ParsedStructure.MetadataTableRowCounts)
    | false, _ ->
        Error(offset', StructureOutsideOfCurrentSection ParsedStructure.MetadataTablesHeader)

let readMetadataTables file reader ustate =
    match reader.ReadMetadataTables with
    | ValueSome reader' ->
        let inline stream index stream =
            match index with
            | ValueSome _ -> ValueSome stream
            | ValueNone -> ValueNone
        reader'
            (stream file.StringsStreamIndex file.StringsStream)
            (stream file.GuidStreamIndex file.GuidStream)
            (stream file.BlobStreamIndex file.BlobStream)
            file.MetadataTables
            //(file.TextSectionOffset + file.MetadataTablesOffset) // This points to the first byte of the tables.
            (file.TextSectionOffset + file.MetadataRootOffset + file.StreamHeaders.[file.MetadataTablesIndex.Value].Offset) // NOTE: This points to the tables header.
            ustate
    | ValueNone -> ustate

let readMetadata (chunk: inref<ChunkedMemory>) file reader ustate rstate =
    let inline text() = file.SectionHeaders.[file.TextSectionIndex]
    match rstate with
    | FindCliHeader ->
        findTextOffset (text()) file.DataDirectories.[Magic.CliHeaderIndex] &file.CliHeaderOffset ReadCliHeader ustate
    | ReadCliHeader -> readCliHeader &chunk file reader ustate
    | FindMetadataRoot ->
        findTextOffset (text()) file.CliHeader.MetaData &file.MetadataRootOffset ReadMetadataSignature ustate
    | ReadMetadataSignature -> readMetadataSignature &chunk file ustate
    | ReadMetadataRoot -> readMetadataRoot &chunk file reader ustate
    | ReadStreamHeaders ->
        match file.MetadataRoot.Streams with
        | 0us -> End
        | _ -> readStreamHeaders &chunk file reader ustate
    | ReadStringsStream -> readStringsHeap &chunk file reader ustate
    | ReadGuidStream -> readGuidHeap &chunk file reader ustate
    | ReadUserStringStream -> readUserStringHeap &chunk file reader ustate
    | ReadBlobStream -> readBlobHeap &chunk file reader ustate
    | ReadMetadataTablesHeader ->
        match file.MetadataTablesIndex with
        | ValueSome i ->
            match readTablesHeader &chunk file (file.MetadataRootOffset + file.StreamHeaders.[i].Offset) with
            | Ok rstate' -> Success(ustate, rstate')
            | Error err -> Failure err
        | ValueNone -> Failure(file.TextSectionOffset + file.StreamHeadersOffset, CannotFindMetadataTables)
    | ReadMetadataTables ->
        file.MetadataTables <-
            ParsedMetadataTables.create
                chunk
                file.MetadataTablesHeader
                { Chunk = file.TextSectionData
                  SectionRva = file.SectionHeaders.[file.TextSectionIndex].Data.VirtualAddress }
                file.MetadataTablesOffset
        Success(readMetadataTables file reader ustate, MoveToEnd)
    | MoveToEnd -> End

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader =
    let src = Reader stream
    let file = { Lfanew = Unchecked.defaultof<uint32> }
    let rec metadata chunk ustate rstate =
        match readMetadata &chunk file reader ustate rstate with
        | Success(ustate', rstate') -> metadata chunk ustate' rstate'
        | Failure(offset, err) -> reader.HandleError (ReadState.Metadata rstate) err (file.TextSectionOffset + offset) ustate
        | End -> ustate
    let rec pe ustate rstate =
        match readPE src file reader ustate rstate with
        | Success(ustate', rstate') -> pe ustate' rstate'
        | Failure err -> reader.HandleError (ReadState.File rstate) err src.Offset ustate
        | End ->
            // Can skip reading metadata if the reader does not need to
            match reader with
            | { ReadCliHeader = ValueNone
                ReadMetadataRoot = ValueNone
                ReadStreamHeader = ValueNone
                ReadMetadataTables = ValueNone } -> ustate
            | _ -> metadata file.TextSectionData ustate FindCliHeader
    pe state ReadPEMagic

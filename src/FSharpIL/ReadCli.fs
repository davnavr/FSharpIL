/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.ReadCli

open System
open System.IO
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.PortableExecutable
open FSharpIL.Reading

[<IsReadOnly; Struct>]
type ReadResult<'State, 'T, 'Error when 'State : struct> =
    | Success of 'T * 'State
    | Failure of 'Error
    | End

[<Sealed>]
type Reader (src: Stream) =
    let mutable pos = 0UL
    do if not src.CanRead then invalidArg "src" "The stream must support reading"

    member _.Offset = pos

    member _.ReadByte() =
        pos <- pos + 1UL
        src.ReadByte()

    member _.ReadBytes(buffer: Span<byte>) =
        let read = src.Read buffer
        pos <- pos + uint64 read
        read

    member _.SkipBytes(count: uint64) =
        let buf = Span.stackalloc<byte> 1
        let mutable cont, skipped = true, 0UL
        while cont && skipped < count do
            match src.Read buf with
            | 0 -> cont <- false
            | _ -> skipped <- skipped + 1UL
        pos <- pos + skipped
        skipped

    member this.MoveTo(offset: uint64) =
        if offset < pos then
            invalidArg "offset" "Cannot move to a previous location"
        elif offset = pos then
            true
        else
            let diff = offset - pos
            this.SkipBytes diff = diff

    /// Reads an unsigned, little-endian, 4-byte integer.
    member inline this.ReadU4(value: outref<uint32>) =
        let bytes = Span.stackalloc<byte> 4
        match this.ReadBytes bytes with
        | 4 ->
            value <- Bytes.readU4 0 bytes
            true
        | _ -> false

[<NoComparison; NoEquality>]
type MutableFile =
    { mutable Lfanew: uint32
      [<DefaultValue>] mutable CoffHeader: ParsedCoffHeader
      [<DefaultValue>] mutable StandardFields: ParsedStandardFields
      [<DefaultValue>] mutable NTSpecificFields: ParsedNTSpecificFields
      [<DefaultValue>] mutable DataDirectories: RvaAndSize[]
      [<DefaultValue>] mutable SectionHeaders: SectionHeader<SectionLocation>[]
      [<DefaultValue>] mutable TextSection: int32
      [<DefaultValue>] mutable TextSectionData: ChunkReader
      /// File offset from start of section to the CLI header (II.25.3.3).
      [<DefaultValue>] mutable CliHeaderOffset: uint64
      [<DefaultValue>] mutable CliHeader: ParsedCliHeader
      /// File offset from start of section to the CLI metadata root (II.24.2.1).
      [<DefaultValue>] mutable MetadataRootOffset: uint64
      [<DefaultValue>] mutable MetadataRoot: ParsedMetadataRoot
      [<DefaultValue>] mutable StreamHeadersOffset: uint64
      [<DefaultValue>] mutable StreamHeaders: ParsedStreamHeader[] }

    /// <summary>File offset to the first byte of the <c>.text</c> section.</summary>
    member this.TextSectionOffset = uint64 this.SectionHeaders.[this.TextSection].Data.RawDataPointer

let inline rvaAndSize i buffer = { Rva = Bytes.readU4 i buffer; Size = Bytes.readU4 (i + 4) buffer }

// TODO: Fix, offset will always point after the end of the structure.
let readCoffHeader (src: Reader) (headers: byref<_>) reader ustate =
    let buffer = Span.stackalloc<byte> Size.CoffHeader
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        headers <-
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
              NumberOfSections = Bytes.readU2 2 buffer
              TimeDateStamp = Bytes.readU4 4 buffer
              SymbolTablePointer = Bytes.readU4 8 buffer
              SymbolCount = Bytes.readU4 12 buffer
              OptionalHeaderSize = Bytes.readU2 16 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.readU2 18 buffer) }
        Success(MetadataReader.readCoffHeader reader headers offset ustate, ReadStandardFields)
    else Failure UnexpectedEndOfFile

let readStandardFields (src: Reader) (fields: byref<_>) reader ustate =
    let buffer = Span.stackalloc<byte> 28
    let offset = src.Offset
    if src.ReadBytes(buffer.Slice(0, 2)) = 2 then
        let magic = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
        let length = if magic = PEImageKind.PE32 then 26 else 22
        let fields' = buffer.Slice(2, length)
        if src.ReadBytes fields' = length then
            fields <-
                { Magic = magic
                  LMajor = fields'.[0]
                  LMinor = fields'.[1]
                  CodeSize = Bytes.readU4 2 fields'
                  InitializedDataSize = Bytes.readU4 6 fields'
                  UninitializedDataSize = Bytes.readU4 10 fields'
                  EntryPointRva = Bytes.readU4 14 fields'
                  BaseOfCode = Bytes.readU4 18 fields'
                  BaseOfData =
                    match magic with
                    | PEImageKind.PE32 -> ValueSome(Bytes.readU4 22 fields')
                    | _ -> ValueNone }
            Success(MetadataReader.readStandardFields reader fields offset ustate, ReadNTSpecificFields)
        else Failure UnexpectedEndOfFile
    else Failure UnexpectedEndOfFile

let readNTSpecificFields (src: Reader) magic (fields: byref<_>) reader ustate =
    let length =
        match magic with
        | PEImageKind.PE32Plus -> 88
        | _ -> 68
    let buffer = Span.heapalloc<byte> length
    let offset = src.Offset
    if src.ReadBytes buffer = length then
        let numdirs = Bytes.readU4 64 buffer
        if numdirs >= 15u then
            fields <-
                match magic with
                | PEImageKind.PE32Plus -> invalidOp "PE32+ not yet supported"
                | _ ->
                    { ImageBase = uint64(Bytes.readU4 0 buffer)
                      // TODO: Validate alignment values, maybe use Alignment type?
                      Alignment = Bytes.readU4 4 buffer, Bytes.readU4 8 buffer
                      OSMajor = Bytes.readU2 12 buffer
                      OSMinor = Bytes.readU2 14 buffer
                      UserMajor = Bytes.readU2 16 buffer
                      UserMinor = Bytes.readU2 18 buffer
                      SubSysMajor = Bytes.readU2 20 buffer
                      SubSysMinor = Bytes.readU2 22 buffer
                      Win32VersionValue = Bytes.readU4 24 buffer
                      ImageSize = Bytes.readU4 28 buffer
                      HeadersSize = Bytes.readU4 32 buffer
                      FileChecksum = Bytes.readU4 36 buffer
                      Subsystem = LanguagePrimitives.EnumOfValue(Bytes.readU2 40 buffer)
                      DllFlags = LanguagePrimitives.EnumOfValue(Bytes.readU2 42 buffer)
                      StackReserveSize = uint64(Bytes.readU4 44 buffer)
                      StackCommitSize = uint64(Bytes.readU4 48 buffer)
                      HeapReserveSize = uint64(Bytes.readU4 52 buffer)
                      HeapCommitSize = uint64(Bytes.readU4 56 buffer)
                      LoaderFlags = Bytes.readU4 60 buffer
                      NumberOfDataDirectories = Bytes.readU4 64 buffer }
            Success(MetadataReader.readNTSpecificFields reader fields offset ustate, ReadDataDirectories)
        else Failure(TooFewDataDirectories numdirs)
    else Failure UnexpectedEndOfFile

let readDataDirectories (src: Reader) (count: uint32) (directories: byref<_>) reader ustate =
    let count' = int32 count
    let buffer = Span.heapalloc<byte>(count' * 8)
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        directories <- Array.zeroCreate count'
        for i = 0 to count' - 1 do
            let i' = i * 8
            directories.[i] <- rvaAndSize i' buffer
        Success(MetadataReader.readDataDirectories reader (Unsafe.As &directories) offset ustate, ReadSectionHeaders)
    else Failure UnexpectedEndOfFile

let readSectionHeaders (src: Reader) (count: uint16) (file: MutableFile) reader ustate =
    let count' = int32 count
    let buffer = Span.heapalloc<byte>(count' * Size.SectionHeader)
    let offset = src.Offset
    if src.ReadBytes buffer = buffer.Length then
        file.SectionHeaders <- Array.zeroCreate<_> count'
        for i = 0 to count' - 1 do
            let i' = i * Size.SectionHeader
            let name = SectionName(buffer.Slice(i', 8).ToArray())
            if name = SectionName.text then file.TextSection <- i
            file.SectionHeaders.[i] <-
                { SectionName = name
                  Data =
                    { VirtualSize = Bytes.readU4 (i' + 8) buffer
                      VirtualAddress = Bytes.readU4 (i' + 12) buffer
                      RawDataSize = Bytes.readU4 (i' + 16) buffer
                      RawDataPointer = Bytes.readU4 (i' + 20) buffer }
                  PointerToRelocations = Bytes.readU4 (i' + 24) buffer
                  // PointerToLineNumbers
                  NumberOfRelocations = Bytes.readU2 (i' + 32) buffer
                  // NumberOfLineNumbers
                  Characteristics = LanguagePrimitives.EnumOfValue(Bytes.readU4 (i' + 36) buffer) }
        Success(MetadataReader.readSectionHeaders reader (Unsafe.As &file.SectionHeaders) offset ustate, MoveToTextSectionData)
    else Failure UnexpectedEndOfFile

let readTextSection (src: Reader) (file: MutableFile) =
    let falignment = snd file.NTSpecificFields.Alignment
    let vsize = file.SectionHeaders.[file.TextSection].Data.VirtualSize
    let len =
        let length = vsize / falignment
        if falignment * length < vsize
        then int32 length + 1
        else int32 length
    let data = Array.zeroCreate len
    file.TextSectionData <- ChunkReader(int32 falignment, data)

    let rec inner i size =
        match size with
        | 0u -> End
        | _ ->
            let len = min falignment size
            let len' = int32 len
            let chunk = Array.zeroCreate len'
            data.[i] <- chunk
            if src.ReadBytes(Span chunk) = len'
            then inner (i + 1) (size - len)
            else Failure UnexpectedEndOfFile

    inner 0 vsize

let readPE (src: Reader) file reader ustate rstate =
    let inline moveto target state' =
        if src.MoveTo target
        then Success(ustate, state')
        else Failure UnexpectedEndOfFile
    match rstate with
    | ReadPEMagic ->
        let magic = Span.stackalloc<byte> 2
        match src.ReadBytes magic with
        | 0 -> Failure UnexpectedEndOfFile
        | 2 when Magic.matches Magic.MZ magic -> Success(ustate, MoveToLfanew)
        | len -> InvalidMagic(Magic.MZ, Bytes.ofSpan len magic) |> Failure
    | MoveToLfanew -> moveto 0x3CUL ReadLfanew
    | ReadLfanew ->
        let offset = src.Offset
        if src.ReadU4(&file.Lfanew) then
            Success(MetadataReader.readLfanew reader file.Lfanew offset ustate, MoveToPESignature)
        else Failure UnexpectedEndOfFile
    | MoveToPESignature -> moveto (uint64 file.Lfanew) ReadPESignature
    | ReadPESignature ->
        let signature = Span.stackalloc<byte> 4
        match src.ReadBytes signature with
        | 0 -> Failure UnexpectedEndOfFile
        | 4 when Magic.matches Magic.PESignature signature -> Success(ustate, ReadCoffHeader)
        | len -> InvalidMagic(Magic.PESignature, Bytes.ofSpan len signature) |> Failure
    | ReadCoffHeader ->
        match readCoffHeader src &file.CoffHeader reader ustate with
        | Success _ when file.CoffHeader.OptionalHeaderSize < Size.OptionalHeader -> // TODO: How to stop reading optional fields according to size of optional header?
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
    | MoveToTextSectionData -> moveto (uint64 file.SectionHeaders.[file.TextSection].Data.RawDataPointer) ReadTextSectionData
    | ReadTextSectionData -> readTextSection src file

let findTextOffset (text: inref<SectionLocation>) { Rva = rva } (offset: byref<uint64>) rstate ustate =
    if text.ContainsRva rva then
        offset <- uint64 rva - uint64 text.VirtualAddress
        Success(ustate, rstate)
    else Failure(uint64 rva, RvaNotInTextSection rva)

// TODO: For functions that read for ChunkReader, don't use UnexpectedEndOfFile, make new error called UnexpectedEndOfSection or something.
let readCliHeader (chunk: ChunkReader) (file: MutableFile) reader ustate =
    match chunk.Parse<ByteParser.U4, _> file.CliHeaderOffset with
    | ValueSome size when size < Size.CliHeader -> Failure(file.CliHeaderOffset, CliHeaderTooSmall size)
    | ValueSome size ->
        let fields = Span.heapalloc<byte>(int32 size)
        let offset = file.CliHeaderOffset + 4UL
        if chunk.ReadBytes(offset, fields) then
            file.CliHeader <-
                { Size = size
                  MajorRuntimeVersion = Bytes.readU2 0 fields
                  MinorRuntimeVersion = Bytes.readU2 2 fields
                  MetaData = rvaAndSize 4 fields
                  Flags = LanguagePrimitives.EnumOfValue(Bytes.readU4 12 fields)
                  EntryPointToken = Bytes.readU4 16 fields
                  Resources = rvaAndSize 20 fields
                  StrongNameSignature = Bytes.readU8 28 fields
                  CodeManagerTable = Bytes.readU8 36 fields
                  VTableFixups = rvaAndSize 44 fields
                  ExportAddressTableJumps = Bytes.readU8 52 fields
                  ManagedNativeHeader = Bytes.readU8 60 fields }
            let ustate' =
                MetadataReader.readCliHeader reader file.CliHeader (file.TextSectionOffset + file.CliHeaderOffset) ustate
            Success(ustate', FindMetadataRoot)
        else Failure(offset, UnexpectedEndOfFile)
    | _ -> Failure(file.CliHeaderOffset, UnexpectedEndOfFile)

let readMetadataVersion (version: byte[]) =
    let mutable cont, i = true, version.Length - 1
    while cont && i >= 0 do
        if version.[i] > 0uy
        then cont <- false
        else i <- i - 1
    if cont
    then ValueNone
    else ValueSome(MetadataVersion(uint8 i + 1uy, version.[..i]))

// TODO: Optimize reading of metadata root and streams, by using the Size part of the MetaData RVA and Size.

let readMetadataRoot (chunk: ChunkReader) (file: MutableFile) reader ustate =
    // First 12 bytes are for MajorVersion, MinorVersion, Reserved and Length
    // Last 4 bytes are for Flags and Streams
    let buffer = Span.stackalloc<byte> 16
    let offset = file.MetadataRootOffset + 4UL
    if chunk.ReadBytes(offset, buffer.Slice(0, 12)) then
        let length = Bytes.readU4 8 buffer
        let version = Array.zeroCreate(int32 length)
        let offset' = offset + 12UL
        if chunk.ReadBytes(offset', Span version) then
            match readMetadataVersion version with
            | ValueSome version' when chunk.ReadBytes(offset' + uint64 length, buffer.Slice 12) ->
                let scount = Bytes.readU2 14 buffer
                file.StreamHeadersOffset <- offset' + uint64 length + 4UL
                file.StreamHeaders <- Array.zeroCreate(int32 scount)
                file.MetadataRoot <-
                    { MajorVersion = Bytes.readU2 0 buffer
                      MinorVersion = Bytes.readU2 2 buffer
                      Reserved = Bytes.readU4 4 buffer
                      Version = version'
                      Flags = Bytes.readU2 12 buffer
                      Streams = scount }
                let ustate' = MetadataReader.readMetadataRoot reader file.MetadataRoot (file.TextSectionOffset + offset) ustate
                Success(ustate', ReadStreamHeaders)
            | ValueSome _ -> Failure(offset' + uint64 length, UnexpectedEndOfFile)
            | ValueNone -> Failure(offset', MetadataVersionHasNoNullTerminator version)
        else Failure(offset', UnexpectedEndOfFile)
    else Failure(offset, UnexpectedEndOfFile)

let readStreamName (chunk: ChunkReader) offset = // TODO: Make this recursive instead.
    let name = Span.stackalloc<byte> 32 // According to (II.24.2.2), the name of the stream is limited to 32 characters.
    let mutable cont, i = true, 0
    while cont && i < name.Length do
        let name' = name.Slice(i, 4)
        if chunk.ReadBytes(offset + uint64 i, name') then
            i <- i + 4
            if name'.[3] = 0uy then cont <- false
        else cont <- false
    if cont
    then Error(name.ToArray())
    else Ok(name.Slice(0, Round.upTo 4 i).ToArray())

let rec readStreamHeaders (chunk: ChunkReader) (file: MutableFile) (fields: Span<byte>) i offset reader ustate =
    if chunk.ReadBytes(offset, fields) then
        let offset' = offset + 8UL
        match readStreamName chunk offset' with
        | Ok name ->
            let mutable name' = name
            let header =
                { Offset = Bytes.readU4 0 fields
                  Size = Bytes.readU4 4 fields
                  Name = Unsafe.As &name' }
            let ustate' = MetadataReader.readStreamHeader reader header i (file.TextSectionOffset + offset) ustate
            
            file.StreamHeaders.[i] <- header
            
            if i >= file.StreamHeaders.Length - 1
            then Success(ustate', FindMetadataTables)
            else readStreamHeaders chunk file fields (i + 1) (offset' + uint64 name.Length) reader ustate'
        | Error err -> Failure(offset, StreamNameHasNoNullTerminator err)
    else Failure(offset, StreamHeaderOutOfBounds i)

let readMetadata (chunk: ChunkReader) (file: MutableFile) reader ustate rstate =
    let text = &file.SectionHeaders.[file.TextSection].Data
    match rstate with
    | FindCliHeader ->
        findTextOffset &text file.DataDirectories.[Magic.CliHeaderIndex] &file.CliHeaderOffset ReadCliHeader ustate
    | ReadCliHeader -> readCliHeader chunk file reader ustate
    | FindMetadataRoot ->
        findTextOffset &text file.CliHeader.MetaData &file.MetadataRootOffset ReadMetadataSignature ustate
    | ReadMetadataSignature ->
        let magic = Span.stackalloc<byte> 4
        if chunk.ReadBytes(file.MetadataRootOffset, magic) then
            if Magic.matches Magic.CliSignature magic
            then Success(ustate, ReadMetadataRoot)
            else Failure(file.MetadataRootOffset, InvalidMagic(Magic.CliSignature, Bytes.ofSpan 4 magic))
        else Failure(file.MetadataRootOffset, UnexpectedEndOfFile)
    | ReadMetadataRoot -> readMetadataRoot chunk file reader ustate
    | ReadStreamHeaders ->
        match file.MetadataRoot.Streams with
        | 0us -> End
        | _ ->
            let fields = Span.stackalloc<byte> 8
            readStreamHeaders chunk file fields 0 file.StreamHeadersOffset reader ustate

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader =
    let src = Reader stream
    let file = { Lfanew = Unchecked.defaultof<uint32> }
    let rec metadata chunk ustate rstate =
        match readMetadata chunk file reader ustate rstate with
        | Success(ustate', rstate') -> metadata chunk ustate' rstate'
        | Failure(offset, err) -> reader.HandleError (ReadState.Metadata rstate) err (file.TextSectionOffset + offset) ustate
        | End -> ustate
    let rec pe ustate rstate =
        match readPE src file reader ustate rstate with
        | Success(ustate', rstate') -> pe ustate' rstate'
        | Failure err -> reader.HandleError (ReadState.File rstate) err src.Offset ustate
        | End ->
            // NOTE: Don't read CLI metadata if provided reader doesn't provide a function to read any metadata.
            match reader with
            | { ReadCliHeader = ValueNone
                ReadMetadataRoot = ValueNone
                ReadStreamHeader = ValueNone } -> ustate
            | _ -> metadata file.TextSectionData ustate FindCliHeader
    pe state ReadPEMagic

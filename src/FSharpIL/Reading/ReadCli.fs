[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

// TODO: Make type of Data be generic and rename this type, so it can be used with StreamHeaders as well.
[<Struct>]
type HeaderDataPointer = { Offset: SectionOffset; [<DefaultValue>] mutable Data: ChunkedMemory }

[<NoComparison; NoEquality>]
type CliInfo =
    { /// Offset from the start of the section to the first byte of the CLI header (II.25.3.3).
      CliHeaderOffset: SectionOffset
      SectionRva: Rva
      SectionOffset: FileOffset
      [<DefaultValue>] mutable CliHeader: ParsedCliHeader
      [<DefaultValue>] mutable CliMetadata: HeaderDataPointer
      [<DefaultValue>] mutable MetadataRoot: ParsedCliMetadataRoot
      [<DefaultValue>] mutable StreamHeadersOffset: SectionOffset
      [<DefaultValue>] mutable StreamHeaders: ImmutableArray<ParsedStreamHeader> }

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
        else Some(StructureOutsideOfCurrentSection ParsedStructure.CliMetadataRoot)
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
    else Error(StructureOutsideOfCurrentSection ParsedStructure.MetadataSignature)

let readMetadataVersion length offset (root: inref<ChunkedMemory>) =
    let version = root.Slice(offset, length).ToImmutableArray()
    match version.[version.Length - 1] with
    | 0uy -> Ok { RoundedLength = Checked.uint8 length; MetadataVersion = version }
    | _ -> Error(MetadataVersionNotTerminated version)

/// Parses the CLI metadata root (II.24.2.1).
let readMetadataRoot info reader ustate =
    let inline failure err = Failure(info.CliMetadata.Offset, err)
    let root: inref<_> = &info.CliMetadata.Data
    match readMetadataSignature info with
    | Ok signature ->
        match ChunkedMemory.tryReadU4 12u &root with
        | ValueSome length when length > 255u || length &&& 4u <> 0u -> failure(InvalidMetadataVersionLength length)
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
                info.StreamHeadersOffset <- { SectionOffset = 20u + length }
                StructureReader.read
                    reader.ReadMetadataRoot
                    info.MetadataRoot
                    (calculateFileOffset info info.CliMetadata.Offset)
                    ustate
                    ReadStreamHeaders
            | Error err -> failure err
        | ValueSome _
        | ValueNone -> failure(noImpl "// TODO: Use different error for structure out of MetaData, since it isn't out of the section but exceeds the length in the Size part of RvaAndSize.")
    | Error err -> failure err

let rec readStreamNameSegment (data: inref<ChunkedMemory>) (offset: SectionOffset) i (buffer: Span<byte>) =
    if i < buffer.Length then
        if data.TryCopyTo(uint32 offset, buffer.Slice(i, 4)) then
            let i' = i + 4
            if buffer.[i + 3] = 0uy then
                buffer.Slice(0, i').ToArray()
                |> Convert.unsafeTo<_, ImmutableArray<byte>>
                |> Ok
            else readStreamNameSegment &data (offset + 4u) i' buffer
        else Error(offset, noImpl "stream name out of bounds")
    else Error(offset, MissingNullTerminator)

let readStreamName (data: inref<ChunkedMemory>) (offset: SectionOffset) =
    // According to (II.24.2.2), the name of the stream is limited to 32 characters.
    readStreamNameSegment &data offset 0 (Span.stackalloc<byte> 32)

let rec readStreamHeadersLoop (section: inref<ChunkedMemory>) (offset: SectionOffset) (headers: ParsedStreamHeader[]) i =
    if i <= headers.Length then
        let offset' = uint32 offset
        if section.HasFreeBytes(offset', 8u) then
            match readStreamName &section (offset + 8u) with
            | Ok name ->
                headers.[i] <-
                    { Offset = MetadataRootOffset(ChunkedMemory.readU4 offset' &section)
                      Size = ChunkedMemory.readU4 (offset' + 4u) &section
                      StreamName = name }
                readStreamHeadersLoop &section (offset + 8u + uint32 name.Length) headers (i + 1)
            | Error err -> Some err
        else Some(offset, noImpl "stream header out of bounds")
    else None

/// Parses the stream headers of the CLI metadata root (II.24.2.2).
let readStreamHeaders (section: inref<ChunkedMemory>) info reader ustate =
    let mutable headers = Unsafe.As &info.StreamHeaders
    headers <- Array.zeroCreate(int32 info.MetadataRoot.Streams)
    match readStreamHeadersLoop &section info.StreamHeadersOffset headers 0 with
    | None ->
        StructureReader.read
            reader.ReadStreamHeaders
            info.StreamHeaders
            (calculateFileOffset info info.StreamHeadersOffset)
            ustate
            ReadStringsStream
    | Some err -> Failure err

let readMetadata (section: inref<ChunkedMemory>) info reader ustate rstate =
    match rstate with
    | ReadCliHeader -> readCliHeader &section info reader ustate
    | FindMetadataRoot -> findMetadataRoot &section info ustate
    | ReadMetadataRoot -> readMetadataRoot info reader ustate
    | ReadStreamHeaders ->
        match reader, info.MetadataRoot.Streams with
        //| { }, 0us -> noImpl "TODO: If further metadata reading functions are defined that depend on stream contents, return error here"
        | _, 0us -> End
        | _, _ -> readStreamHeaders &section info reader ustate

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

[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadPE

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.PortableExecutable

[<IsReadOnly; Struct>]
type ReadResult<'State, 'T, 'Error when 'State : struct> =
    | Success of 'T * 'State
    | Failure of 'Error
    | End

type IHeaderReader = abstract Read: Span<byte> -> int32

[<Sealed>]
type HeaderReader<'Reader when 'Reader :> IHeaderReader> (reader: 'Reader) =
    let mutable pos = { FileOffset = 0u }
    member _.Offset = pos
    member _.Read buffer = reader.Read buffer = buffer.Length

    member this.SkipBytes(count: uint32) = // TODO: Make skipping method work better.
        let buffer = Span.stackalloc<byte> 1
        let mutable cont, skipped = true, 0u
        while cont && skipped < count do
            if this.Read buffer
            then skipped <- skipped + 1u
            else cont <- false
        pos <- pos + skipped
        skipped

    member this.MoveTo (offset: FileOffset) =
        if offset < pos then
            Some(CannotMoveToPreviousOffset this.Offset)
        elif offset = pos then
            None
        else
            let diff = uint32(offset - pos)
            if this.SkipBytes diff = diff
            then None
            else Some UnexpectedEndOfFile

[<NoComparison; NoEquality>]
type PEInfo =
    { mutable Lfanew: FileOffset
      [<DefaultValue>] mutable CoffHeader: ParsedCoffHeader
      [<DefaultValue>] mutable OptionalHeader: ParsedOptionalHeader
      [<DefaultValue>] mutable DataDirectories: ParsedDataDirectories
      [<DefaultValue>] mutable SectionHeaders: ParsedSectionHeaders
      }

let offsetToLfanew = { FileOffset = 0x3Cu }

let inline readStructure reader arg offset ustate next =
    match StructureReader.read reader arg offset ustate with
    | ValueSome ustate' -> Success(ustate', next)
    | ValueNone -> End

let readMagic (src: HeaderReader<_>) (expected: ImmutableArray<byte>) ustate next =
    let actual = Span.stackalloc<byte> expected.Length
    if src.Read actual then
        if expected.AsSpan() = Span.asReadOnly actual
        then Success(ustate, next)
        else InvalidMagic(expected, Span.toBlock actual) |> Failure
    else Failure UnexpectedEndOfFile

let readLfanew (src: HeaderReader<_>) (lfanew: byref<FileOffset>) reader ustate =
    let buffer, offset = Span.stackalloc<byte> 4, src.Offset
    if src.Read buffer then
        lfanew <- { FileOffset = Bytes.toU4 0 buffer }
        readStructure reader.ReadLfanew lfanew offset ustate MoveToPESignature
    else Failure UnexpectedEndOfFile

let readCoffHeader (src: HeaderReader<_>) (coff: outref<_>) reader ustate =
    let buffer = Span.stackalloc<byte>(int32 Magic.coffHeaderSize)
    let offset = src.Offset
    if src.Read buffer then
        coff <-
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.toU2 0 buffer)
              NumberOfSections = Bytes.toU2 2 buffer
              TimeDateStamp = Bytes.toU4 4 buffer
              SymbolTablePointer = Bytes.toU4 8 buffer
              SymbolCount = Bytes.toU4 12 buffer
              OptionalHeaderSize = Bytes.toU2 16 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.toU2 18 buffer) }
        readStructure reader.ReadCoffHeader coff offset ustate ReadOptionalHeader
    else Failure UnexpectedEndOfFile

/// Contains functions for reading the contents of the PE file optional header (II.25.2.3).
[<RequireQualifiedAccess>]
module OptionalHeader =
    let createStandardFields magic (fields: Span<byte>) baseOfData =
        { Magic = magic
          LMajor = fields.[0]
          LMinor = fields.[1]
          CodeSize = Bytes.toU4 2 fields
          InitializedDataSize = Bytes.toU4 6 fields
          UninitializedDataSize = Bytes.toU4 10 fields
          EntryPointRva = Bytes.toU4 14 fields
          BaseOfCode = Bytes.toU4 18 fields
          BaseOfData = baseOfData }

    let readStandardFields (src: HeaderReader<_>) =
        let buffer = Span.stackalloc<byte> 28 // Size of standard fields for PE32, which is larger than size for PE32+
        if src.Read(buffer.Slice(0, 2)) then
            let magic = LanguagePrimitives.EnumOfValue<_, ImageKind>(Bytes.toU2 0 buffer)
            let fields = buffer.Slice 2
            if magic <> ImageKind.PE32Plus
            then Choice1Of2(createStandardFields magic fields (Bytes.toU4 22 fields))
            else Choice2Of2(createStandardFields magic fields Omitted)
            |> Ok
        else Error UnexpectedEndOfFile

    let readAlignment = noImpl "TODO: Read alignment"

    let readNTSpecificFields (src: HeaderReader<_>) standardFields =
        let buffer =
            let size =
                match standardFields with
                | Choice1Of2 _ -> 68
                | Choice2Of2 _ -> 88
            Span.stackalloc<byte> size
        match src.Read buffer, standardFields with
        | true, Choice1Of2 standardFields -> // PE32
            let fields =
                { ImageBase = Bytes.toU4 0 buffer
                  Alignment = readAlignment
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
                  StackReserveSize = Bytes.toU4 44 buffer
                  StackCommitSize = Bytes.toU4 48 buffer
                  HeapReserveSize = Bytes.toU4 52 buffer
                  HeapCommitSize = Bytes.toU4 56 buffer
                  LoaderFlags = Bytes.toU4 60 buffer
                  NumberOfDataDirectories = Bytes.toU4 64 buffer }
            Ok(ParsedOptionalHeader.PE32(standardFields, fields))
        | true, Choice2Of2 standardFields -> // PE32+
            let fields =
                { ImageBase = Bytes.toU8 0 buffer
                  Alignment = readAlignment
                  OSMajor = Bytes.toU2 16 buffer
                  OSMinor = Bytes.toU2 18 buffer
                  UserMajor = Bytes.toU2 20 buffer
                  UserMinor = Bytes.toU2 22 buffer
                  SubSysMajor = Bytes.toU2 24 buffer
                  SubSysMinor = Bytes.toU2 26 buffer
                  Win32VersionValue = Bytes.toU4 28 buffer
                  ImageSize = Bytes.toU4 32 buffer
                  HeadersSize = Bytes.toU4 36 buffer
                  FileChecksum = Bytes.toU4 40 buffer
                  Subsystem = LanguagePrimitives.EnumOfValue(Bytes.toU2 44 buffer)
                  DllFlags = LanguagePrimitives.EnumOfValue(Bytes.toU2 46 buffer)
                  StackReserveSize = Bytes.toU8 48 buffer
                  StackCommitSize = Bytes.toU8 56 buffer
                  HeapReserveSize = Bytes.toU8 64 buffer
                  HeapCommitSize = Bytes.toU8 72 buffer
                  LoaderFlags = Bytes.toU4 80 buffer
                  NumberOfDataDirectories = Bytes.toU4 84 buffer }
            Ok(ParsedOptionalHeader.PE32Plus(standardFields, fields))
        | false, _ -> Error UnexpectedEndOfFile

    let read (src: HeaderReader<_>) (header: outref<ParsedOptionalHeader>) reader ustate =
        let offset = src.Offset
        match readStandardFields src with
        | Ok standardFields ->
            match readNTSpecificFields src standardFields with
            | Ok header' ->
                header <- header'
                readStructure reader.ReadOptionalHeader header' offset ustate ReadDataDirectories
            | Error err -> Failure err
        | Error err -> Failure err

let rec readDataDirectory (src: HeaderReader<_>) (buffer: Span<byte>) (directories: RvaAndSize[]) i =
    if i < directories.Length then
        if src.Read buffer then
            directories.[i] <- { Rva = Rva(Bytes.toU4 0 buffer); Size = Bytes.toU4 4 buffer }
            readDataDirectory src buffer directories (i + 1)
        else Some UnexpectedEndOfFile
    else None

let readDataDirectories
    (src: HeaderReader<_>)
    (Convert.I4 count: uint32)
    (directories: outref<ParsedDataDirectories>)
    reader
    ustate
    =
    let offset = src.Offset
    let mutable directories' = Array.zeroCreate count
    match readDataDirectory src (Span.heapalloc<byte> 8) directories' 0 with
    | None ->
        let named =
            match reader.ReadCliMetadata with
            | ValueSome _ when count < 15 -> Error(TooFewDataDirectories(uint32 count))
            | ValueSome _ ->
                DataDirectories (
                    directories'.[0],
                    directories'.[1],
                    directories'.[2],
                    directories'.[3],
                    directories'.[4],
                    directories'.[5],
                    directories'.[6],
                    directories'.[7],
                    directories'.[8],
                    directories'.[9],
                    directories'.[10],
                    directories'.[11],
                    directories'.[12],
                    directories'.[13],
                    { CliHeader = directories'.[14] },
                    if count > 15 then directories'.[15] else RvaAndSize.Zero
                )
                |> ValueSome
                |> Ok
            | ValueNone -> Ok ValueNone
        match named with
        | Ok named ->
            directories <- named, Unsafe.As &directories'
            readStructure reader.ReadDataDirectories directories offset ustate ReadSectionHeaders
        | Error err -> Failure err
    | Some err -> Failure err

let rec readSectionHeader (src: HeaderReader<_>) (buffer: Span<byte>) (headers: SectionHeader[]) i =
    if i < headers.Length then
        if src.Read buffer then
            headers.[i] <-
                { SectionName = { SectionName.SectionName = buffer.Slice(0, 8).ToArray() }
                  VirtualSize = Bytes.toU4 8 buffer
                  VirtualAddress = Rva(Bytes.toU4 12 buffer)
                  RawDataSize = Bytes.toU4 16 buffer
                  RawDataPointer = Bytes.toU4 20 buffer
                  PointerToRelocations = Bytes.toU4 24 buffer
                  PointerToLineNumbers = Bytes.toU4 28 buffer
                  NumberOfRelocations = Bytes.toU2 32 buffer
                  NumberOfLineNumbers = Bytes.toU2 34 buffer
                  Characteristics = LanguagePrimitives.EnumOfValue(Bytes.toU4 36 buffer) }
            readSectionHeader src buffer headers (i + 1)
        else Some UnexpectedEndOfFile
    else None

let readSectionHeaders (src: HeaderReader<_>) (Convert.I4 count: uint16) (headers: outref<ParsedSectionHeaders>) reader ustate =
    let offset = src.Offset
    let headers': byref<SectionHeader[]> = &Unsafe.As &headers
    headers' <- Array.zeroCreate count
    match readSectionHeader src (Span.stackalloc<byte>(int32 Magic.sectionHeaderSize)) headers' 0 with
    | None -> readStructure reader.ReadSectionHeaders headers offset ustate (noImpl "TODO: Read section containing CliMetadata?")
    | Some err -> Failure err

let readPE (src: HeaderReader<_>) file reader ustate rstate =
    let inline moveToOffset target state' =
        match src.MoveTo target with
        | None -> Success(ustate, state')
        | Some err -> Failure err
    let inline magic expected next = readMagic src expected ustate next
    match rstate with
    | ReadPEMagic -> magic Magic.dosHeaderSignature MoveToLfanew
    | MoveToLfanew -> moveToOffset offsetToLfanew ReadLfanew
    | ReadLfanew -> readLfanew src &file.Lfanew reader ustate
    | MoveToPESignature -> moveToOffset file.Lfanew ReadPESignature
    | ReadPESignature -> magic Magic.portableExecutableSignature ReadCoffHeader
    | ReadCoffHeader ->
        match readCoffHeader src &file.CoffHeader reader ustate with
        | Success _ when file.CoffHeader.OptionalHeaderSize < Magic.optionalHeaderSize -> // TODO: How to stop reading optional fields according to size of optional header?
            Failure(OptionalHeaderTooSmall file.CoffHeader.OptionalHeaderSize)
        | result -> result
    | ReadOptionalHeader -> OptionalHeader.read src &file.OptionalHeader reader ustate
    | ReadDataDirectories ->
        readDataDirectories src file.OptionalHeader.NumberOfDataDirectories &file.DataDirectories reader ustate
    | ReadSectionHeaders -> readSectionHeaders src file.CoffHeader.NumberOfSections &file.SectionHeaders reader ustate

let fromReader (source: #IHeaderReader) state reader =
    let src, file = HeaderReader source, { Lfanew = { FileOffset = 0u } }
    //let rec metadata
    let rec pe ustate rstate =
        match readPE src file reader ustate rstate with
        | Success(ustate', rstate') -> pe ustate' rstate'
        | Failure err -> reader.HandleError (ReadState.File rstate) err src.Offset ustate
        | End ->
            // Can skip reading metadata if the reader does not need to
            match reader.ReadCliMetadata with
            | ValueNone -> ustate
            | ValueSome reader' -> failwith "TODO: read metadata"
    pe state ReadPEMagic

[<IsReadOnly; Struct>]
type StreamHeaderReader<'Stream when 'Stream :> Stream> (stream: 'Stream) =
    interface IHeaderReader with member _.Read buffer = stream.Read buffer

let fromStream (stream: #Stream) state reader = fromReader (StreamHeaderReader stream) state reader

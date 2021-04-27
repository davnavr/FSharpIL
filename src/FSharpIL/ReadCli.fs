/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.ReadCli

open System
open System.IO
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked
open Microsoft.FSharp.NativeInterop

open FSharpIL.PortableExecutable
open FSharpIL.Reading

#nowarn "9"

[<IsReadOnly; Struct>]
type ReadResult<'T> =
    | Success of 'T * ReadState
    | Failure of ReadError
    | End

/// <summary>Creates a <see cref="System.Span`1"/> from a region of memory allocated on the stack.</summary>
[<RequiresExplicitTypeArguments>]
let inline allocspan<'T when 'T : unmanaged> length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

[<RequiresExplicitTypeArguments>]
let inline arrspan<'T> length = Span<'T>(Array.zeroCreate<'T> length)

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
        let buf = allocspan<byte> 1
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
        let bytes = allocspan<byte> 4
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
      [<DefaultValue>] mutable DataDirectories: struct(uint32 * uint32)[]
      [<DefaultValue>] mutable SectionHeaders: SectionHeader<SectionLocation>[]
      [<DefaultValue>] mutable TextSection: int32
      [<DefaultValue>] mutable TextSectionData: byte[][] }

let readCoffHeader (src: Reader) (headers: byref<_>) reader ustate =
    let buffer = allocspan<byte> Size.CoffHeader
    if src.ReadBytes buffer = buffer.Length then
        headers <-
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
              NumberOfSections = Bytes.readU2 2 buffer
              TimeDateStamp = Bytes.readU4 4 buffer
              SymbolTablePointer = Bytes.readU4 8 buffer
              SymbolCount = Bytes.readU4 12 buffer
              OptionalHeaderSize = Bytes.readU2 16 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.readU2 18 buffer) }
        Success(MetadataReader.readCoffHeader reader headers src.Offset ustate, ReadStandardFields)
    else Failure UnexpectedEndOfFile

let readStandardFields (src: Reader) (fields: byref<_>) reader ustate =
    let buffer = allocspan<byte> 28
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
            Success(MetadataReader.readStandardFields reader fields src.Offset ustate, ReadNTSpecificFields)
        else Failure UnexpectedEndOfFile
    else Failure UnexpectedEndOfFile

let readNTSpecificFields (src: Reader) magic (fields: byref<_>) reader ustate =
    let length =
        match magic with
        | PEImageKind.PE32Plus -> 88
        | _ -> 68
    let buffer = arrspan<byte> length
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
            Success(MetadataReader.readNTSpecificFields reader fields src.Offset ustate, ReadDataDirectories)
        else Failure(TooFewDataDirectories numdirs)
    else Failure UnexpectedEndOfFile

let readDataDirectories (src: Reader) (count: uint32) (directories: byref<_>) reader ustate =
    let count' = int32 count
    let buffer = arrspan<byte>(count' * 8)
    if src.ReadBytes buffer = buffer.Length then
        directories <- Array.zeroCreate count'
        for i = 0 to count' - 1 do
            let i' = i * 8
            directories.[i] <- struct(Bytes.readU4 i' buffer, Bytes.readU4 (i' + 4) buffer)
        Success(MetadataReader.readDataDirectories reader (Unsafe.As &directories) src.Offset ustate, ReadSectionHeaders)
    else Failure UnexpectedEndOfFile

let readSectionHeaders (src: Reader) (count: uint16) (file: MutableFile) reader ustate =
    let count' = int32 count
    let buffer = arrspan<byte>(count' * Size.SectionHeader)
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
        Success(MetadataReader.readSectionHeaders reader (Unsafe.As &file.SectionHeaders) src.Offset ustate, MoveToTextSectionData)
    else Failure UnexpectedEndOfFile

let readTextSection (src: Reader) (file: MutableFile) ustate =
    let falignment = snd file.NTSpecificFields.Alignment
    let vsize = file.SectionHeaders.[file.TextSection].Data.VirtualSize
    let len = vsize / falignment
    file.TextSectionData <- Array.zeroCreate(if falignment * len < vsize then int32 len + 1 else int32 len)

    let rec inner i size =
        match size with
        | 0u -> Success(ustate, MoveToCliHeader)
        | _ ->
            let len = min falignment size
            let len' = int32 len
            let chunk = Array.zeroCreate len'
            file.TextSectionData.[i] <- chunk
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
        let magic = allocspan<byte> 2
        match src.ReadBytes magic with
        | 0 -> Failure UnexpectedEndOfFile
        | 2 when Magic.matches Magic.MZ magic -> Success(ustate, MoveToLfanew)
        | len -> InvalidMagic(Magic.MZ, Bytes.ofSpan len magic) |> Failure
    | MoveToLfanew -> moveto 0x3CUL ReadLfanew
    | ReadLfanew when src.ReadU4(&file.Lfanew) ->
        Success(MetadataReader.readLfanew reader file.Lfanew src.Offset ustate, MoveToPESignature)
    | MoveToPESignature -> moveto (uint64 file.Lfanew) ReadPESignature
    | ReadPESignature ->
        let signature = allocspan<byte> 4
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
    | ReadTextSectionData -> readTextSection src file ustate
    | MoveToCliHeader ->
        let struct(rva, _) = file.DataDirectories.[Magic.CliHeaderIndex]
        // NOTE: src.Offset will point past the end of the .text section, meaning these error messages will be inaccurate.
        if file.SectionHeaders.[file.TextSection].Data.ContainsRva rva
        then Success(ustate, ReadCliHeader)
        else Failure(CliHeaderNotInTextSection rva)
    | ReadCliHeader ->
        invalidOp "TODO: Read Size"
    | ReadLfanew -> Failure UnexpectedEndOfFile

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader =
    let src = Reader stream
    let file = { Lfanew = Unchecked.defaultof<uint32> }
    let rec inner ustate rstate =
        match readPE src file reader ustate rstate with
        | Success(ustate', rstate') -> inner ustate' rstate'
        | Failure err -> reader.HandleError rstate err src.Offset ustate
        | End -> ustate // TODO: Call here
    // TODO: Have other recursive loop for reading CLI metadata in .text section
    inner state ReadPEMagic

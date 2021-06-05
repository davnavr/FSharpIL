[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<NoComparison; NoEquality>]
type CliInfo =
    { SectionRva: Rva
      SectionOffset: FileOffset
      [<DefaultValue>] mutable CliHeader: ParsedCliHeader }

let inline calculateFileOffset { CliInfo.SectionOffset = start } { SectionOffset.SectionOffset = soffset } = start + soffset

let readRvaAndSize offset (chunk: inref<_>) =
    { Rva = Rva(ChunkedMemory.readU4 offset &chunk)
      Size = ChunkedMemory.readU4 (offset + 4u) &chunk }

let readCliHeader (section: inref<ChunkedMemory>) info offset reader ustate =
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
        | false, _ -> Failure(offset, StructureOutsideOfCurrentSection ParsedStructure.CliHeader)
    | ValueSome cb -> Failure(offset, CliHeaderTooSmall cb)
    | ValueNone -> Failure(offset, StructureOutsideOfCurrentSection ParsedStructure.CliHeader)

let readMetadata (section: inref<ChunkedMemory>) info (cliHeaderOffset: SectionOffset) reader ustate rstate =
    match rstate with
    | ReadCliHeader -> readCliHeader &section info cliHeaderOffset reader ustate

let rec readMetadataLoop (section: inref<_>) info cliHeaderOffset reader ustate rstate =
    match readMetadata &section info cliHeaderOffset reader ustate rstate with
    | Success(ustate', rstate') -> readMetadataLoop &section info cliHeaderOffset reader ustate' rstate'
    | Failure(soffset, err) ->
        ErrorHandler.handle rstate err (calculateFileOffset info soffset) ustate reader.HandleError
    | End -> ustate

let fromChunkedMemory (section: inref<_>) sectionRva sectionOffset cliHeaderOffset state reader =
    readMetadataLoop
        &section
        { SectionRva = sectionRva; SectionOffset = sectionOffset }
        cliHeaderOffset
        reader
        state
        ReadCliHeader

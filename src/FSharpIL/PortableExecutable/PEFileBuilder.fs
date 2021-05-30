namespace FSharpIL.PortableExecutable

open System.Collections.Generic

open FSharpIL

/// Represents a Portable Executable file.
[<Sealed>]
type PEFileBuilder (fileHeader: CoffHeader<Omitted, Omitted>, optionalHeader: OptionalHeader, sectionsCapacity: int32) =
    let sections = List<SectionBuilder> sectionsCapacity
    member _.FileHeader = fileHeader
    member _.OptionalHeader = optionalHeader

    member _.GetSection i = sections.[i]

    // TODO: Provide functions for modifying instead of methods? Or both?
    /// Creates a section with the specified name and flags.
    member _.CreateSection(name, characteristics) =
        let builder = SectionBuilder(optionalHeader.Alignment, name, characteristics)
        sections.Add builder
        builder

// Comments below are important.

// TODO: Figure out how to make section data and set data directories.
// let addDataTo section (data: ChunkedMemory): RvaAndSize = ?

// type CliMetadataDirectory = { CliMetadata: RvaAndSize }
// let addCliMetadata section (data: CliMetadata): CliMetadataDirectory = ?

namespace FSharpIL.PortableExecutable

open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL

/// Represents a Portable Executable file.
[<Sealed>]
type PEFileBuilder (fileHeader: CoffHeader<Omitted, Omitted>, optionalHeader: OptionalHeader, sectionsCapacity: int32) =
    let sections = List<SectionBuilder> sectionsCapacity
    member _.FileHeader = fileHeader
    member _.OptionalHeader = optionalHeader
    member val DataDirectories =
        { ImportTable = RvaAndSize.Zero
          ImportAddressTable = RvaAndSize.Zero
          CliHeader = Unchecked.defaultof<_> }

    member _.GetSection i = sections.[i]

    //member _.AddSection(builder: SectionBuilder)

    // TODO: Provide functions for modifying instead of methods? Or both?
    [<System.Obsolete>]
    /// Creates a section with the specified name and flags.
    member _.CreateSection(name, characteristics) =
        let builder = SectionBuilder(optionalHeader.Alignment, name, characteristics)
        sections.Add builder
        builder

    member this.ToImmutable() =
        let mutable sections' = Array.zeroCreate sections.Count
        for i = 0 to sections.Count - 1 do
            sections'.[i] <- sections.[i].ToImmutable()
        PEFile(fileHeader, optionalHeader, DataDirectories.ofBuilder this.DataDirectories, Unsafe.As &sections')

// Comments below are important.

// TODO: Figure out how to make section data and set data directories.
// let addDataTo section (data: ChunkedMemory): RvaAndSize = ?

// type CliMetadataDirectory = { CliMetadata: RvaAndSize }
// let addCliMetadata section (data: CliMetadata): CliMetadataDirectory = ?

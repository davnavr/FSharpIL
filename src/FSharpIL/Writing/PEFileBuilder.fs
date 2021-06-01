namespace FSharpIL.PortableExecutable

open System
open System.Collections.Generic

open FSharpIL

[<Sealed>]
type internal PEFileBuilderSectionsEnumerator = class
    val mutable private enumerator: List<SectionBuilder>.Enumerator
    new (enumerator) = { enumerator = enumerator }
    member this.Current = this.enumerator.Current.ToImmutable()
    interface IEnumerator<Section> with
        member this.Current = this.Current
        member this.Current = box this.Current
        member this.MoveNext() = this.enumerator.MoveNext()
        member this.Dispose() = this.enumerator.Dispose()
        member this.Reset() = (this.enumerator :> IEnumerator<_>).Reset()
end

[<Sealed>]
type internal PEFileBuilderSections internal (sections: List<SectionBuilder>) =
    member _.GetEnumerator() = new PEFileBuilderSectionsEnumerator(sections.GetEnumerator())
    interface IReadOnlyCollection<Section> with
        member _.Count = sections.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

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

    // TODO: Provide functions for modifying instead of methods? Or both?
    /// Creates a section with the specified name and flags.
    member _.CreateSection(name, characteristics) =
        let builder = SectionBuilder(optionalHeader.Alignment, name, characteristics)
        sections.Add builder
        builder

    interface IPortableExecutable with
        member this.CoffHeader = this.FileHeader
        member this.OptionalHeader = this.OptionalHeader
        member this.DataDirectories = DataDirectories.ofBuilder this.DataDirectories
        member _.Sections = PEFileBuilderSections sections :> IReadOnlyCollection<_>

// Comments below are important.

// TODO: Figure out how to make section data and set data directories.
// let addDataTo section (data: ChunkedMemory): RvaAndSize = ?

// type CliMetadataDirectory = { CliMetadata: RvaAndSize }
// let addCliMetadata section (data: CliMetadata): CliMetadataDirectory = ?

namespace FSharpIL.PortableExecutable

open System.Collections.Immutable

open FSharpIL
open FSharpIL.Utilities

[<System.Runtime.CompilerServices.IsReadOnly>]
type Section = struct
    val Header: SectionHeader
    val Data: ChunkedMemory
    internal new (header, data) = { Header = header; Data = data }
end

[<Sealed>]
type PEFile internal
    (
        fileHeader: CoffHeader<Omitted, Omitted>,
        optionalHeader: OptionalHeader,
        dataDirectories: DataDirectories,
        sections: ImmutableArray<Section>,
        sizeOfHeaders: uint32
    )
    =
    member _.FileHeader = fileHeader
    member _.OptionalHeader = optionalHeader
    member _.DataDirectories = dataDirectories
    member _.Sections = sections
    member _.SizeOfHeaders = sizeOfHeaders

[<RequireQualifiedAccess>]
module PEFile =
    /// <summary>
    /// Calculates the value of the <c>SizeOfHeaders</c> field in the optional header, rounded up to a multiple of
    /// <c>FileAlignment</c> (II.25.2.3.2).
    /// </summary>
    let internal calculateHeadersSize optionalHeader numOfSections falignment =
        let optionalHeaderSize =
            match optionalHeader with
            | PE32 _ -> Magic.OptionalHeaderSize
            | PE32Plus _ -> Magic.OptionalHeaderPlusSize
        uint32 Magic.msDosStub.Length
        + uint32 Magic.portableExecutableSignature.Length
        + uint32 optionalHeaderSize
        + (Magic.SectionHeaderSize * uint32 numOfSections)
        |> Round.upTo falignment

/// Contains functions for calculating the number of bytes for data.
[<RequireQualifiedAccess>]
module internal FSharpIL.Length

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let peHeader = dosStub.Length + peSignature.Length |> uint32

[<Literal>]
let CoffHeader = 20u

[<Literal>]
/// The size of the standard fields (PE32).
let StandardFields = 28u

[<Literal>]
/// The size of the NT specific fields (PE32).
let NTSpecificFields = 96u

[<Literal>]
/// The size of the ten data directories.
let DataDirectories = 80u

/// The length of a single section header.
[<Literal>]
let SectionHeader = 40u

/// The length of the CLI header.
[<Literal>]
let CliHeader = 0x48u

type Cache =
    { SectionsLength: Lazy<uint32[]> }

    member this.TotalLength() =
        let sectionsLength = this.SectionsLength.Value
        peHeader
        + CoffHeader
        + StandardFields
        + NTSpecificFields
        + DataDirectories
        + (uint32 sectionsLength.Length * SectionHeader) // Section headers
        // TODO: How to add the padding here?
        

/// Calculates the combined length of the CLI header, the strong name hash, the
/// method bodies, and the CLI metadata.
let ofCliData (data: CliHeader) =
    CliHeader
    // + Strong Name Hash
    // + Method Bodies
    // + Metadata

let ofSection (section: Section) =
    let mutable len = 0u
    for data in section.Data do
        let len' =
            match data with
            | SectionData.CliHeader cli -> ofCliData cli
            | ClrLoaderStub -> 8u
            | RawData data -> data() |> Array.length |> uint32
        len <- len + len'
    len

let ofPEFile (file: PEFile) =
    let sections = file.SectionTable
    { SectionsLength =
        lazy (Array.init sections.Length (fun i -> sections.Item i |> ofSection)) }

namespace FSharpIL.PortableExecutable

open System.Collections.Immutable

open FSharpIL
open FSharpIL.Metadata

type PEFile =
    { FileHeader: CoffHeader<Omitted, Omitted>
      StandardFields: StandardFields<Omitted, Omitted, Omitted>
      NTSpecificFields: NTSpecificFields<ImageBase, Alignment, Omitted, uint32, Omitted>
      Sections: PESections }

    member inline this.DataDirectories = this.Sections.DataDirectories
    member inline this.SectionTable = this.Sections.SectionTable
    member this.CliHeader = this.DataDirectories.CliHeader |> Option.map (fun header -> header.Header)

    static member Default =
        { FileHeader = CoffHeader.defaultFields
          StandardFields = StandardFields.defaultFields
          NTSpecificFields = NTSpecificFields.defaultFields
          Sections = PESections.Empty }

[<RequireQualifiedAccess>]
module PEFile =
    let ofMetadata characteristics (metadata: CliMetadata) =
        let text =
            { Kind = TextSection
              Data = [| ClrLoaderStub; CliHeader metadata |].ToImmutableArray() }
        { PEFile.Default with
            FileHeader = { CoffHeader.defaultFields with Characteristics = characteristics }
            Sections = ImmutableArray.Create<Section> text |> PESections }

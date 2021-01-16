﻿namespace FSharpIL.PortableExecutable

open System.Collections.Immutable

open FSharpIL.Metadata

#nowarn "25"

type PEFile =
    { FileHeader: CoffHeader
      StandardFields: StandardFields
      NTSpecificFields: NTSpecificFields
      Sections: PESections }

    member inline this.DataDirectories = this.Sections.DataDirectories
    member inline this.SectionTable = this.Sections.SectionTable
    member inline this.CliHeader = this.DataDirectories.CliHeader |> Option.map (fun header -> header.Header)

    static member Default =
        { FileHeader = CoffHeader.Default
          StandardFields = StandardFields.Default
          NTSpecificFields = NTSpecificFields.Default
          Sections = PESections.Default }

[<RequireQualifiedAccess>]
module PEFile =
    let ofMetadata fileType (metadata: CliHeader) =
        let text :: tail = List.ofSeq PESections.Default.SectionTable
        let text' = { text with Data = [| ClrLoaderStub; CliHeader metadata |].ToImmutableArray() }
        { PEFile.Default with
            FileHeader = { CoffHeader.Default with Characteristics = FileType fileType }
            Sections = (text' :: tail).ToImmutableArray() |> PESections }


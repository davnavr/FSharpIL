[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

/// Writes the CLI header (II.25.3.3).
let header info (wr: ChunkedMemoryBuilder) =
    let header = invalidOp "TODO: Get the CLI header": CliHeader<_, _, _, _, _, _, _, _>
    wr.WriteLE Magic.cliHeaderSize // Cb
    wr.WriteLE header.MajorRuntimeVersion
    wr.WriteLE header.MinorRuntimeVersion

    // MetaData
    noImpl ""

    // Flags
    noImpl ""

    // EntryPointToken
    noImpl ""

    // Resources
    noImpl ""

    // StrongNameSignature
    noImpl ""

    wr.WriteLE 0UL // CodeManagerTable
    
    // VTableFixups
    noImpl ""

    wr.WriteLE 0UL // ExportAddressTableJumps
    wr.WriteLE 0UL // ManagedNativeHeader

let metadata cli rva wr =
    noImpl ""

[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open FSharpIL

/// Writes the CLI header (II.25.3.3).
let header info (wr: ChunkedMemoryBuilder) =
    wr.WriteLE Magic.cliHeaderSize // Cb

let metadata cli rva wr =
    ()

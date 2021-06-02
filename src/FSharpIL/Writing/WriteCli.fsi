[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open FSharpIL
open FSharpIL.Metadata

val metadata: CliMetadata -> cliHeaderRva: Rva -> ChunkedMemoryBuilder -> unit

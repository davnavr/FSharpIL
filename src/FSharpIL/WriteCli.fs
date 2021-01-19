module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System.Text

open FSharpIL.Bytes
open FSharpIL.Magic
open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header.
    [<Literal>]
    let CliHeader = 0x48UL

[<Sealed>]
type CliInfo internal (metadata: CliMetadata, headerRva: uint64) =
    member _.TotalSize: uint64 = invalidOp "TODO: Calculate size."

let data (info: CliInfo)= invalidOp "TODO: Write CLI data"

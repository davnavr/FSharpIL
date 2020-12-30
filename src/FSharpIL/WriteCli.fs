module internal FSharpIL.WriteCli

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header.
    [<Literal>]
    let CliHeader = 0x48UL

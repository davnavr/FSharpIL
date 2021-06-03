/// Contains various magic numbers used throughout the CLI metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Magic

/// <summary>The size of the CLI header, in bytes, stored in the <c>Cb</c> field of the CLI header (II.25.3.3).</summary>
let cliHeaderSize = 0x48u

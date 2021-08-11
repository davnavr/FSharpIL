/// Contains various magic numbers used throughout the CLI metadata.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Magic

open System.Collections.Immutable

open FSharpIL.Utilities

/// <summary>The size of the CLI header, in bytes, stored in the <c>Cb</c> field of the CLI header (II.25.3.3).</summary>
let CliHeaderSize = 0x48u

/// <summary>The value of the <c>Signature</c> field of the CLI metadata root (II.24.2.1).</summary>
let metadataRootSignature = Convert.unsafeTo<_, ImmutableArray<byte>> [| 0x42uy; 0x53uy; 0x4Auy; 0x42uy; |]

[<RequireQualifiedAccess>]
module StreamNames =
    let metadata = Convert.unsafeTo<_, ImmutableArray<byte>> "#~\000\000"B
    let strings = Convert.unsafeTo<_, ImmutableArray<byte>> "#Strings\000\000\000\000"B
    let us = Convert.unsafeTo<_, ImmutableArray<byte>> "#US\000"B
    let guid = Convert.unsafeTo<_, ImmutableArray<byte>> "#GUID\000\000\000"B
    let blob = Convert.unsafeTo<_, ImmutableArray<byte>> "#Blob\000\000\000"B

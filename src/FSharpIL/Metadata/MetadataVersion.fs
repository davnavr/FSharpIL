namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

open FSharpIL.Utilities

open FSharpIL

/// Specifies what version of the CLI that the file is intended to execute on (II.24.2.1).
[<IsReadOnly; Struct>]
type MetadataVersion = // TODO: Rename to RuntimeVersion
    internal { RoundedLength: uint8; MetadataVersion: ImmutableArray<byte> }
    override this.ToString() = Encoding.UTF8.GetString(this.MetadataVersion.AsSpan()).TrimEnd '\000'
    /// The length of the version string including the null terminator, rounded up to a multiple of 4.
    member this.Length = uint32 this.RoundedLength

[<RequireQualifiedAccess>]
module MetadataVersion =
    let tryOfStr (str: string) =
        if str.Length > 255 || str.Contains '\000'
        then ValueNone
        else
            ValueSome
                { RoundedLength = Round.upTo 4uy (uint8 str.Length)
                  MetadataVersion = Convert.unsafeTo(Encoding.UTF8.GetBytes str) }

    let ofStr (str: string) =
        match tryOfStr str with
        | ValueSome ver -> ver
        | ValueNone ->
            invalidArg "str" "The version string cannot contain a null character or have a length greater than 255 bytes"

    let asSpan { MetadataVersion = bytes } = bytes.AsSpan()
    let toBlock { MetadataVersion = bytes } = bytes
    let toArray { MetadataVersion = bytes } = bytes.AsSpan().ToArray()

    /// Latest runtime version for files intended to be executed on Microsoft-specific implementations of the CLI such as .NET
    /// Framework, .NET Core, or .NET.
    let defaultLatest = ofStr "v4.0.30319"
    /// Latest runtime version for files "intended to be executed on any conforming implementation of the CLI" (II.24.2.1).
    let standardLatest = ofStr "Standard CLI 2005"

    let write (builder: byref<ChunkedMemoryBuilder>) { RoundedLength = Convert.I4 len; MetadataVersion = version } =
        let padding = Span.stackalloc<byte>(len - version.Length)
        builder.Write version
        builder.Write padding

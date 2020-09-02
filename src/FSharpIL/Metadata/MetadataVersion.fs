namespace FSharpIL.Metadata

/// NOTE: Length of string is rounded up to a multiple of 4
type MetadataVersion =
    internal
    | MetadataVersion of string

    override this.ToString() =
        let (MetadataVersion version) = this in version

    member this.Length =
        let length = this.ToString().Length |> uint32
        (length - 1u) - ((length - 1u) % 4u) + 4u

namespace FSharpIL.Metadata

[<System.Flags>]
type ManifestResourceFlags =
    | Public = 1u
    | Private = 2u
    | VisibilityMask = 7u

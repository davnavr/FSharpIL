namespace ILInfo

[<System.Flags>]
type IncludedHeaders =
    | NoHeaders = 0uy
    | CoffHeader = 1uy
    | StandardFields = 2uy
    | NTSpecificFields = 4uy
    | DataDirectories = 8uy
    | SectionHeaders = 0x10uy
    | CliHeader = 0x20uy
    | MetadataRoot = 0x40uy

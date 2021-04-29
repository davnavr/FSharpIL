namespace ILInfo

[<System.Flags>]
type IncludedHeaders =
    | None = 0us
    | CoffHeader = 1us
    | StandardFields = 2us
    | NTSpecificFields = 4us
    | DataDirectories = 8us
    | SectionHeaders = 0x10us
    | CliHeader = 0x20us
    | MetadataRoot = 0x40us
    | StreamHeaders = 0x80us
    | MetadataTables = 0x100us
    | All = 0xFFFFus

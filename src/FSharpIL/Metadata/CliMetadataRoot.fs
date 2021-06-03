namespace FSharpIL.Metadata

/// The CLI metadata root (II.24.2.1).
type CliMetadataRoot<'Signature> =
    { MajorVersion: uint16
      MinorVersion: uint16
      Reserved: uint32
      Version: MetadataVersion
      Flags: uint16 }

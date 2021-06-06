namespace FSharpIL.Metadata

/// The CLI metadata root (II.24.2.1).
type CliMetadataRoot<'Signature> =
    { Signature: 'Signature
      MajorVersion: uint16
      MinorVersion: uint16
      Reserved: uint32
      Version: MetadataVersion
      Flags: uint16
      /// Specifies the number of stream headers after the CLI metadata root (II.24.2.1).
      Streams: uint16 }

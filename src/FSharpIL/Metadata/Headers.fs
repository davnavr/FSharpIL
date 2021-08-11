namespace FSharpIL.Metadata

open FSharpIL

/// The CLI metadata root (II.24.2.1).
type CliMetadataRoot<'Signature, 'Streams> =
    { Signature: 'Signature
      MajorVersion: uint16
      MinorVersion: uint16
      Reserved: uint32
      Version: MetadataVersion
      /// Reserved value set to 0 on writing.
      Flags: uint16
      /// Specifies the number of stream headers after the CLI metadata root (II.24.2.1).
      Streams: 'Streams }

[<RequireQualifiedAccess>]
module CliMetadataRoot =
    let defaultFields =
        { Signature = Omitted
          MajorVersion = 1us
          MinorVersion = 1us
          Reserved = 0u
          Version = MetadataVersion.defaultLatest
          Flags = 0us
          Streams = Omitted }

/// Flags that describe the runtime image (II.25.3.3.1).
[<System.Flags>]
type CorFlags =
    | None = 0u
    | ILOnly = 1u
    | Requires32Bit = 2u
    /// The image has a strong name signature.
    | StrongNameSigned = 0x8u
    | NativeEntryPoint = 0x10u
    | TrackDebugData = 0x10000u

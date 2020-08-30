namespace FSharpIL.Cil

open System

type MetadataVersion =
    internal
    | MetadataVersion of string

    override this.ToString() =
        let (MetadataVersion version) = this in version

// II.24.2.2
type MetadataStream =
    | MetadataStream
    | StringStream
    | UserStringStream
    | GUIDStream
    | BlobStream

// NOTE: II.24.2.2 says that each type of stream can only occur 1 time at most.
type MetadataStreams =
    { MetadataStream: unit }

    static member Default =
        { MetadataStream = () }

// II.24.2.1
type MetadataRoot =
    { // Signature
      MajorVersion: uint16
      MinorVersion: uint16
      // Reserved
      Version: MetadataVersion
      // Flags
      Streams: MetadataStreams }

    static member Default =
        { MajorVersion = 1us
          MinorVersion = 1us
          Version = MetadataVersion "v4.0.30319"
          Streams = MetadataStreams.Default }

// II.25.3.3.1
[<Flags>]
type CorFlags =
    | ILOnly = 0x1u
    | Requires32Bit = 0x2u
    | StrongNameSigned = 0x8u
    | NativeEntryPoint = 0x10u
    | TrackDebugData = 0x10000u

// II.25.3.3
type CliHeader =
    { // Cb
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Metadata: MetadataRoot
      Flags: CorFlags // TODO: Create default value for flags.
      EntryPointToken: unit
      Resources: unit
      StrongNameSignature: unit
      CodeManagerTable: uint64
      VTableFixups: unit
      // ExportAddressTableJumps
      // ManagedNativeHeader
      }

    static member Default =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 0us
          Metadata = MetadataRoot.Default
          Flags = invalidOp "default here"
          EntryPointToken = ()
          Resources = ()
          StrongNameSignature = ()
          CodeManagerTable = 0UL
          VTableFixups = () }

namespace FSharpIL.Metadata

open FSharpIL

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

/// <summary>
/// Describes the location of the CLI metadata root, resources, the strong name signature, and other miscellaneous information
/// (II.25.3.3).
/// </summary>
type CliHeader<'Cb, 'EntryPointToken> = // TODO: What to use for EntryPointToken? Remove 'EntryPointToken generic parameter
    { /// The size of the CLI header, in bytes
      Cb: 'Cb
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Metadata: RvaAndSize
      Flags: CorFlags
      EntryPointToken: 'EntryPointToken
      Resources: RvaAndSize
      StrongNameSignature: RvaAndSize
      CodeManagerTable: RvaAndSize
      VTableFixups: RvaAndSize
      ExportAddressTableJumps: RvaAndSize
      ManagedNativeHeader: RvaAndSize }

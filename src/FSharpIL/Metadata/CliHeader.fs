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
[<NoComparison; StructuralEquality>]
type CliHeader<'Cb, 'Flags, 'EntryPointToken, 'Reserved, 'Metadata, 'Resources, 'StrongNameSignature, 'VTableFixups> = // TODO: Don't make this a generic type.
    { /// The size of the CLI header, in bytes
      Cb: 'Cb
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Metadata: 'Metadata
      Flags: 'Flags
      EntryPointToken: 'EntryPointToken
      Resources: 'Resources
      StrongNameSignature: 'StrongNameSignature
      CodeManagerTable: 'Reserved
      VTableFixups: 'VTableFixups
      ExportAddressTableJumps: 'Reserved
      ManagedNativeHeader: 'Reserved }

type CliHeader = CliHeader<Omitted, Omitted, Omitted, Omitted, Omitted, Omitted, Omitted, Omitted>

[<RequireQualifiedAccess>]
module CliHeader =
    let defaultFields =
        { Cb = Omitted
          MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Metadata = Omitted
          Flags = Omitted
          EntryPointToken = Omitted
          Resources = Omitted
          StrongNameSignature = Omitted
          CodeManagerTable = Omitted
          VTableFixups = Omitted
          ExportAddressTableJumps = Omitted
          ManagedNativeHeader = Omitted }

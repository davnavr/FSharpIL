namespace FSharpIL.Metadata

open System
open System.Collections.Immutable

// TODO: Move these types near to where they are used.

// II.25.3.3.1
[<Flags>]
type CorFlags =
    | None = 0u
    | ILOnly = 1u
    | Requires32Bit = 2u
    | StrongNameSigned = 0x8u
    | NativeEntryPoint = 0x10u
    | TrackDebugData = 0x10000u

// II.25.3.3
type CliHeaderFields =
    { // HeaderSize = 0x48u
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      // Metadata
      // Flags
      // EntryPointToken
      Resources: unit
      StrongNameSignature: ImmutableArray<byte>
      CodeManagerTable: uint64 // TODO: Figure out if this field should exist.
      VTableFixups: unit
      // ExportAddressTableJumps
      // ManagedNativeHeader
      }

    static member Default =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Resources = ()
          StrongNameSignature = ImmutableArray.Empty
          CodeManagerTable = 0UL
          VTableFixups = () }

type HashAlgorithmId =
    | None = 0u
    | MD5 = 0x8003u
    | SHA1 = 0x8004u



/// Represents an element type used in a signature (II.23.1.16).
type internal ElementType =
    | End = 0uy
    | Void = 0x1uy

    | String = 0xEuy

    | Array = 0x14uy

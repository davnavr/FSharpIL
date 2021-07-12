namespace FSharpIL.Writing

open FSharpIL
open FSharpIL.Metadata

// TODO: Come up with better name that shows how this type is only used when writing.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type CliHeader =
    { MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Requires32Bit: bool }

[<RequireQualifiedAccess>]
module CliHeader =
    let latestDefault =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Requires32Bit = false }

// TODO: Make this class internal, or make this a public record type.

/// <summary>Builds the CLI metadata stored in the <c>.text</c> section of a PE file (II.24).</summary>
[<Sealed>]
type CliMetadataBuilder internal
    (
        header: CliHeader,
        root: CliMetadataRoot<Omitted, Omitted>,
        methodBodies: FSharpIL.Writing.Cil.MethodBodyList,
        tables: MetadataTablesBuilder,
        strings: StringsStreamBuilder,
        us: UserStringStreamBuilder,
        guid: GuidStreamBuilder,
        blob: BlobStreamBuilder //,
        //resources,
        //strongNameSignature,
        //vTableFixups
    )
    =

    member _.Header = header
    member _.Root = root
    member _.MethodBodies = methodBodies
    member _.Tables = tables
    member _.Strings = strings
    member _.UserString = us
    member _.Guid = guid
    member _.Blob = blob

    member val EntryPointToken: EntryPointToken = EntryPointToken.Null with get, set

    member _.HeaderFlags =
        let mutable flags = CorFlags.ILOnly
        if header.Requires32Bit then flags <- flags ||| CorFlags.Requires32Bit
        //if (noImpl "is signed/has strong name signature") then flags <- flags ||| CorFlags.StrongNameSigned
        flags

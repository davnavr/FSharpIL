namespace FSharpIL.Writing

open FSharpIL
open FSharpIL.Metadata

// TODO: Come up with better name that shows how this type is only used when writing.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CliHeader =
    { MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Requires32Bit: bool }

[<RequireQualifiedAccess>]
module CliHeader =
    let defaultFields =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Requires32Bit = false }

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
    ) =
    new (moduleRow, header, root, stringsCapacity, guidCapacity, usCapacity, blobCapacity) =
        let strings = StringsStreamBuilder stringsCapacity
        let guids = GuidStreamBuilder guidCapacity
        let blobs = BlobStreamBuilder blobCapacity
        CliMetadataBuilder (
            header,
            root,
            FSharpIL.Writing.Cil.MethodBodyList(),
            MetadataTablesBuilder(moduleRow, strings, guids, blobs),
            strings,
            UserStringStreamBuilder usCapacity,
            guids,
            blobs
        )
    new (moduleRow, header, root) = CliMetadataBuilder(moduleRow, header, root, 1024, 1, 1, 512)
    new (moduleRow) = CliMetadataBuilder(moduleRow, CliHeader.defaultFields, CliMetadataRoot.defaultFields)

    member _.Header = header
    member _.Root = root
    member _.MethodBodies = methodBodies
    member _.Tables = tables
    member _.Strings = strings
    member _.UserString = us
    member _.Guid = guid
    member _.Blob = blob

    member val EntryPointToken: EntryPointToken voption = ValueNone with get, set

    member _.HeaderFlags =
        let mutable flags = CorFlags.ILOnly
        if header.Requires32Bit then flags <- flags ||| CorFlags.Requires32Bit
        //if (noImpl "is signed/has strong name signature") then flags <- flags ||| CorFlags.StrongNameSigned
        flags

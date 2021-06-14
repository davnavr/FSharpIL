namespace FSharpIL.Writing

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

/// <summary>Builds the CLI metadata stored in the <c>.text</c> section of a PE file (II.24).</summary>
[<Sealed>]
type CliMetadataBuilder internal
    (
        header: CliHeaderBuilder,
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
    new (moduleRow) = CliMetadataBuilder(moduleRow, CliHeaderBuilder.defaultFields, CliMetadataRoot.defaultFields)

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

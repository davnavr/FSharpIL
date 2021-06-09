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
        // methodBodies,
        tables,
        strings: StringsStreamBuilder,
        us,
        guid: GuidStreamBuilder,
        blob //,
        //resources,
        //strongNameSignature,
        //vTableFixups
    ) =
    new (header, root, stringsCapacity, guidCapacity, usCapacity, blobCapacity) =
        CliMetadataBuilder (
            header,
            root,
            noImpl "tables",
            StringsStreamBuilder stringsCapacity,
            noImpl "us",
            GuidStreamBuilder guidCapacity,
            noImpl "blob"
        )
    new (header, root) =
        CliMetadataBuilder (
            header,
            root,
            noImpl "tables",
            StringsStreamBuilder(),
            noImpl "us",
            GuidStreamBuilder(),
            noImpl "blob"
        )
    new () = CliMetadataBuilder(CliHeaderBuilder.defaultFields, CliMetadataRoot.defaultFields)

    member _.Header = header
    member _.Root = root
    member _.Tables = tables
    member _.Strings = strings
    //member _.UserString = us
    member _.Guid = guid
    //member _.Blob = blob

    member _.HeaderFlags =
        let mutable flags = CorFlags.ILOnly
        if header.Requires32Bit then flags <- flags ||| CorFlags.Requires32Bit
        if (noImpl "is signed") then flags <- flags ||| CorFlags.StrongNameSigned
        flags

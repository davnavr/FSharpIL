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
        strings: StringsStreamBuilder,
        guid: GuidStreamBuilder,
        us,
        blob,
        tables //,
        //resources,
        //strongNameSignature,
        //vTableFixups
    ) =
    new (header, root, stringsCapacity, guidCapacity, usCapacity, blobCapacity) =
        CliMetadataBuilder (
            header,
            root,
            StringsStreamBuilder stringsCapacity,
            GuidStreamBuilder guidCapacity,
            noImpl "us",
            noImpl "blob", 
            noImpl "tables"
        )
    new (header, root) =
        CliMetadataBuilder (
            header,
            root,
            StringsStreamBuilder(),
            GuidStreamBuilder(),
            noImpl "us",
            noImpl "blob", 
            noImpl "tables"
        )
    new () = CliMetadataBuilder(CliHeaderBuilder.defaultFields, CliMetadataRoot.defaultFields)

    member _.Header = header
    member _.Root = root
    member _.Strings = strings
    member _.Guid = guid
    //member _.UserString = us
    //member _.Blob = blob
    member _.Tables = tables

    member _.HeaderFlags =
        let mutable flags = CorFlags.ILOnly
        if header.Requires32Bit then flags <- flags ||| CorFlags.Requires32Bit
        if (noImpl "is signed") then flags <- flags ||| CorFlags.StrongNameSigned
        flags

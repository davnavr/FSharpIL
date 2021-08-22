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

// TODO: Make this class internal, or remove it entirely.

/// <summary>Builds the CLI metadata stored in the <c>.text</c> section of a PE file (II.24).</summary>
[<Sealed>]
type CliMetadataBuilder internal
    (
        header: CliHeader,
        root: CliMetadataRoot<Omitted, Omitted>,
        methodBodies: Lazy<ChunkedMemory>,
        moduleRowBuilder,
        strings: StringsStreamBuilder,
        us: UserStringStreamBuilder,
        guid: GuidStreamBuilder,
        blob: BlobStreamBuilder //,
        //resources,
        //strongNameSignature,
        //vTableFixups
    )
    =
    let mutable nextEmbeddedData = 0u
    member val internal EmbeddedData = System.Collections.Generic.List<System.ReadOnlyMemory<byte>>()
    member val Tables = MetadataTablesBuilder(moduleRowBuilder, strings, guid, blob)
    member _.Header = header
    member _.Root = root
    member _.MethodBodies = methodBodies.Value
    member _.Strings = strings
    member _.UserString = us
    member _.Guid = guid
    member _.Blob = blob

    member val EntryPointToken: EntryPointToken = EntryPointToken.Null with get, set

    member this.AddEmbeddedData data =
        let location = FSharpIL.Metadata.Tables.FieldValueLocation nextEmbeddedData
        this.EmbeddedData.Add data
        nextEmbeddedData <- Checked.(+) nextEmbeddedData (Checked.uint32 data.Length)
        location

    member _.HeaderFlags =
        let mutable flags = CorFlags.ILOnly
        if header.Requires32Bit then flags <- flags ||| CorFlags.Requires32Bit
        //if (noImpl "is signed/has strong name signature") then flags <- flags ||| CorFlags.StrongNameSigned
        flags

module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Collections.Generic
open System.Text

open FSharpIL.Metadata
open FSharpIL.Metadata.Heaps
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48u

[<Sealed>]
type CodedIndex<'T> internal (count: int32, n: int32, indexer: 'T -> uint32 * uint32) =
    let large = count > (65535 <<< n)

    member val IndexSize = if large then 4 else 2

    member _.IndexOf (item: 'T) =
        let index, tag = indexer item
        (index <<< n) ||| tag
    
    member this.WriteIndex(item, writer: ChunkWriter) =
        let index = this.IndexOf item
        if large then writer.WriteU4 index else writer.WriteU2 index

let codedIndex count n indexer = CodedIndex<_>(count, n, indexer)

[<ReferenceEquality; NoComparison>]
type CliInfo =
    { HeaderRva: uint32
      Cli: CliMetadata
      /// Specifies the RVA and size of the CLI metadata (II.25.3.3).
      mutable Metadata: ChunkWriter
      /// Specifies the RVA and size of the "hash data for this PE file" (II.25.3.3).
      mutable StrongNameSignature: ChunkWriter
      StringsStream: Heap<string>
      // US
      GuidStream: Heap<Guid>
      BlobStream: BlobHeap }

/// Writes the CLI header (II.25.3.3).
let header info (writer: ChunkWriter) =
    let header = info.Cli.Header
    writer.WriteU4 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    // MetaData
    info.Metadata <- writer.CreateWriter()
    writer.SkipBytes 8u

    writer.WriteU4 info.Cli.HeaderFlags // Flags
    writer.WriteU4 0u // EntryPointToken // TODO: Figure out what this token value should be. Is an index into the MethodDef table allowed?

    // Resources
    writer.WriteU4 0u
    writer.WriteU4 0u

    // StrongNameSignature
    info.StrongNameSignature <- writer.CreateWriter()
    writer.SkipBytes 8u

    writer.WriteU8 0UL // CodeManagerTable

    // VTableFixups
    writer.WriteU4 0u
    writer.WriteU4 0u

    writer.WriteU8 0UL // ExportAddressTableJumps
    writer.WriteU8 0UL // ManagedNativeHeader

/// Writes the method bodies.
let bodies (info: CliInfo) (writer: ChunkWriter) =
    ()

/// Writes the contents of the #~ stream (II.24.2.6).
let tables (info: CliInfo) (writer: ChunkWriter) =
    writer.WriteU4 0u // Reserved
    writer.WriteU1 2uy // MajorVersion
    writer.WriteU1 0uy // MinorVersion

    do // HeapSizes
        let mutable bits = 0uy
        if info.StringsStream.IndexSize = 4 then bits <- bits ||| 1uy
        if info.GuidStream.IndexSize = 4 then bits <- bits ||| 2uy
        if info.BlobStream.IndexSize = 4 then bits <- bits ||| 4uy
        writer.WriteU1 bits

    writer.WriteU1 1uy // Reserved
    writer.WriteU8 info.Cli.Valid
    writer.WriteU8 0UL // Sorted

    let tables = info.Cli

    // Rows
    for row in tables.RowCounts do
        writer.WriteU4 row

    // Tables

    // Calculate how big indices and coded indices should be.
    let resolutionScope =
        // TODO: Include ModuleRef table when determine how big a ResolutionScope should be.
        let total =
            1 // Module
            + tables.AssemblyRef.Count
            + tables.TypeRef.Count
        function
        | ResolutionScope.AssemblyRef assm -> tables.AssemblyRef.IndexOf assm, 2u
        | bad -> failwithf "Unsupported resolution scope %A" bad
        |> codedIndex total 2

    let extends =
        // TODO: Include TypeSpec table.
        let total = tables.TypeDef.Count + tables.TypeRef.Count
        function
        | Extends.AbstractClass { TypeHandle = tdef }
        | Extends.ConcreteClass { TypeHandle = tdef } -> tables.TypeDef.IndexOf tdef, 0u
        | Extends.TypeRef tref -> tables.TypeRef.IndexOf tref, 1u
        | bad -> failwithf "Unsupported extends %A" bad
        |> codedIndex total 2

    let memberRefParent =
        // TODO: Include ModuleRef and TypeSpec tables.
        let total = tables.TypeDef.Count + tables.TypeRef.Count + tables.MethodDef.Count
        function
        | MemberRefParent.TypeRef tref -> tables.TypeRef.IndexOf tref, 1u
        |> codedIndex total 3

    let customAttriuteParent =
        let total =
            tables.MethodDef.Count
            + tables.Field.Count
            + tables.TypeRef.Count
            + tables.TypeDef.Count
            + tables.Param.Length
            // InterfaceImpl
            + tables.MemberRef.Count
            + 1 // Module
            // Permission
            // Property
            // Event
            // StandAloneSig
            // ModuleRef
            // TypeSpec
            + if tables.Assembly.IsSome then 1 else 0
            + tables.AssemblyRef.Count
            // File
            // ExportedType
            // ManifestResource
            // GenericParam
            // GenericParamConstraint
            // MethodSpec
        function
        | CustomAttributeParent.Assembly _ -> 1u, 14u
        | bad -> failwithf "Unsupported custom attribute parent %A" bad
        |> codedIndex total 5

    let customAttributeType =
        let total = tables.MethodDef.Count + tables.MemberRef.Count
        function
        | CustomAttributeType.MemberRef mref -> tables.MemberRef.IndexOf mref.Handle, 3u
        |> codedIndex total 3

    // Module (0x00)
    writer.WriteU2 0us // Generation
    info.StringsStream.WriteStringIndex(tables.Module.Name, writer)
    info.GuidStream.WriteIndex(tables.Module.Mvid, writer)
    info.GuidStream.WriteZero writer // EncId
    info.GuidStream.WriteZero writer // Encbaseid

    // TypeRef (0x01)
    for tref in tables.TypeRef.Items do
        resolutionScope.WriteIndex(tref.ResolutionScope, writer)
        info.StringsStream.WriteStringIndex(tref.TypeName, writer)
        info.StringsStream.WriteIndex(tref.TypeNamespace, writer)

    // TypeDef (0x02)
    if tables.TypeDef.Count > 0 then
        let mutable field = 1u
        let mutable method = 1u

        for tdef in tables.TypeDef.Items do
            writer.WriteU4 tdef.Flags
            info.StringsStream.WriteStringIndex(tdef.TypeName, writer)
            info.StringsStream.WriteIndex(tdef.TypeNamespace, writer)
            extends.WriteIndex(tdef.Extends, writer)

            // Field
            let field' = if tdef.FieldList.IsEmpty then 0u else field
            field <- uint32 tdef.FieldList.Length
            tables.Field.WriteSimpleIndex(field', writer)

            // Method
            let method' = if tdef.MethodList.IsEmpty then 0u else method
            method <- uint32 tdef.MethodList.Length
            tables.Field.WriteSimpleIndex(method', writer)

    // Field (0x04)
    for row in tables.Field.Items do
        writer.WriteU2 row.Flags
        info.StringsStream.WriteStringIndex(row.Name, writer)
        invalidOp "TODO: Write index for field signatures."

    // MethodDef (0x06)
    if tables.MethodDef.Count > 0 then
        let mutable param = 1u

        for method in tables.MethodDef.Items do
            writer.WriteU4 0u // RVA // TODO: Write the RVA to the method body.
            writer.WriteU2 method.ImplFlags
            writer.WriteU2 method.Flags
            info.StringsStream.WriteStringIndex(method.Name, writer)
            info.BlobStream.WriteIndex(method.Signature, writer) // Signature

            let param' = if method.ParamList.IsEmpty then 0u else param // Param
            param <- param + uint32 method.ParamList.Length
            // TODO: Since both the Param table (which is an ImmutableArray) and the ImmutableTable class have an implementation for writing an index, maybe make it an extension method?
            // If doing the above, maybe add an extension member to allow retrieval of IndexSize as well.
            if tables.Param.Length < 65536
            then writer.WriteU2 param'
            else writer.WriteU4 param'

    // Param (0x08)
    for sequence, row in tables.Param do
        writer.WriteU2 row.Flags.Flags
        writer.WriteU2(sequence + 1)
        info.StringsStream.WriteIndex(row.ParamName, writer)




    // MemberRef (0x0A)
    for row in tables.MemberRef.Items do
        memberRefParent.WriteIndex(row.Class, writer)
        info.StringsStream.WriteStringIndex(row.MemberName, writer)

        // Signature
        match row with
        | MethodRef method -> info.BlobStream.WriteIndex(method.Signature, writer)




    // CustomAttribute (0x0C)
    for row in tables.CustomAttribute do
        customAttriuteParent.WriteIndex(row.Parent, writer)
        customAttributeType.WriteIndex(row.Type, writer)
        info.BlobStream.WriteIndex(row.Value, writer)




    // Assembly (0x20)
    if tables.Assembly.IsSome then
        let assembly = tables.Assembly.Value
        writer.WriteU4 0u // TODO: Determine what HashAlgId should be.
        writer.WriteU2 assembly.Version.Major
        writer.WriteU2 assembly.Version.Minor
        writer.WriteU2 (max assembly.Version.Build 0)
        writer.WriteU2 (max assembly.Version.Revision 0)
        writer.WriteU4 0u // Flags // TODO: Determine what flags an assembly should have.
        info.BlobStream.WriteEmpty writer // PublicKey // TODO: Determine how to write the PublicKey into a blob.
        info.StringsStream.WriteStringIndex(assembly.Name, writer)
        info.StringsStream.WriteStringIndex(assembly.Culture, writer)

    // AssemblyRef (0x23)
    for row in tables.AssemblyRef.Items do
        writer.WriteU2 row.Version.Major
        writer.WriteU2 row.Version.Minor
        writer.WriteU2 row.Version.Build
        writer.WriteU2 row.Version.Revision
        writer.WriteU4 row.Flags
        info.BlobStream.WriteEmpty writer // PublicKeyOrToken // TODO: Figure out how to write the PublicKeyOrToken into a blob.
        info.StringsStream.WriteStringIndex(row.Name, writer)
        info.StringsStream.WriteStringIndex(row.Culture, writer)
        info.BlobStream.WriteEmpty writer // HashValue // TODO: Figure out how to write the HashValue into a blob.

    // NestedClass (0x29)
    for row in tables.NestedClass do
        tables.TypeDef.WriteSimpleIndex(row.NestedClass, writer)
        tables.TypeDef.WriteSimpleIndex(row.EnclosingClass, writer)

    // TODO: Write more tables.
    ()

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) (writer: ChunkWriter) =
    let version = MetadataVersion.toArray info.Cli.MetadataVersion
    writer.WriteBytes Magic.CliSignature
    writer.WriteU2 1us // MajorVersion
    writer.WriteU2 1us // MinorVersion
    writer.WriteU4 0u // Reserved
    writer.WriteU4 version.Length
    writer.WriteBytes version
    writer.WriteU2 0us // Flags

    // Streams
    let streams = writer.CreateWriter()
    writer.SkipBytes 2u

    let mutable offset = writer.Size

    // Stream headers
    let inline streamHeader name =
        if Array.length name % 4 <> 0 then
            invalidArg (nameof name) "The length of the stream header name must be a multiple of four."
        let header = writer.CreateWriter()
        let name' = header.CreateWriter()
        name'.SkipBytes 8u
        name'.WriteBytes name
        let size = name'.Size
        writer.SkipBytes size
        offset <- offset + size
        header

    let metadata = streamHeader "#~\000\000"B
    let strings = streamHeader "#Strings\000\000\000\000"B
    let us =
        // if info. // TODO: Write #US stream header
        // then streamHeader "#US\000"B
        Unchecked.defaultof<ChunkWriter>
    let guid = streamHeader "#GUID\000\000\000"B
    let blob =
        if info.BlobStream.SignatureCount > 0
        then streamHeader "#Blob\000\000\000"B
        else Unchecked.defaultof<ChunkWriter>

    let inline stream (header: ChunkWriter) content =
        let heap = writer.CreateWriter()
        content heap
        // TODO: Align to four-byte boundary.
        let size = heap.Size
        header.WriteU4 offset
        header.WriteU4 size
        offset <- offset + size
        writer.SkipBytes size

    // #~
    stream metadata (tables info)

    // #Strings
    stream strings (Heap.writeStrings info.StringsStream.Count info.Cli)

    // #US

    // #GUID
    stream guid (Heap.writeGuid info.Cli)

    // #Blob
    if blob <> Unchecked.defaultof<ChunkWriter> then
        stream blob (Heap.writeBlob info.BlobStream info.Cli)

    do // Stream count
        let mutable count = 3u // #~, #Strings, #GUID
        // TODO: Include #US stream in stream count.
        if info.BlobStream.SignatureCount > 0 then count <- count + 1u

        streams.WriteU2 count

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (headerRva: uint32) (section: ChunkWriter) =
    let writer = section.CreateWriter()
    let info =
        { HeaderRva = headerRva
          Cli = cli
          Metadata = Unchecked.defaultof<ChunkWriter>
          StrongNameSignature = Unchecked.defaultof<ChunkWriter>
          StringsStream = Heap.strings cli
          GuidStream = Heap.guid cli
          BlobStream = Heap.blob cli }
    let mutable rva = headerRva

    header info writer
    rva <- rva + Size.CliHeader

    do // Strong Name Signature
        info.StrongNameSignature.WriteU4 rva
        writer.ResetSize()
        writer.WriteBytes(cli.Header.StrongNameSignature)
        let size = writer.Size
        info.StrongNameSignature.WriteU4 size
        rva <- rva + size

    do // Method Bodies
        writer.ResetSize()
        bodies info writer
        rva <- rva + writer.Size

    do // CLI metadata
        info.Metadata.WriteU4 rva
        writer.ResetSize()
        root info writer
        info.Metadata.WriteU4 writer.Size
        rva <- rva + writer.Size

    section.SkipBytes (rva - headerRva)

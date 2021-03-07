[<RequireQualifiedAccess>]
module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Collections.Generic

open FSharpIL.Metadata
open FSharpIL.Metadata.Heaps
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48u

    /// The size of a fat method body header, as a number of 4-byte integers (II.25.4.3).
    [<Literal>]
    let FatFormat = 3us

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

[<RequireQualifiedAccess>]
module private ILMethodFlags =
    let [<Literal>] TinyFormat = 0x2uy
    let [<Literal>] FatFormat = 0x3us
    let [<Literal>] MoreSects = 0x8us
    let [<Literal>] InitLocals = 0x10us

[<ReferenceEquality; NoComparison>]
type CliInfo =
    { HeaderRva: uint32
      Cli: CliMetadata
      /// Specifies the RVA and size of the CLI metadata (II.25.3.3).
      mutable Metadata: ChunkWriter
      /// Specifies the RVA and size of the "hash data for this PE file" (II.25.3.3).
      mutable StrongNameSignature: ChunkWriter
      MethodBodies: Dictionary<IMethodBody, uint32>
      StringsStream: Heap<string>
      UserStringStream: UserStringHeap
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

    let entryPointTable, entryPointToken =
        match info.Cli.EntryPointToken with
        | ValueNone -> 0uy, 0u
        | ValueSome (ValidEntryPoint (SimpleIndex main))
        | ValueSome (CustomEntryPoint (SimpleIndex main)) ->
            0x6uy, info.Cli.MethodDef.IndexOf main
        | ValueSome (EntryPointFile file) ->
            0x26uy, info.Cli.File.IndexOf file.Value.File

    MetadataToken.write entryPointToken entryPointTable writer

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

/// <param name="start">
/// The first node that the method bodies will temporarily be written to before being written as part of the CLI metadata.
/// </param>
let methodBody (start: LinkedListNode<byte[]>) (body: IMethodBody) (info: CliInfo) =
    if not body.Exists then invalidArg "body" "The method body should exist"

    let writer = ChunkWriter start
    let content = MethodBodyContentImpl(writer, info.Cli, info.UserStringStream)
    let info = body.WriteBody content
    struct(writer.Size, info)

let bodies rva (info: CliInfo) (writer: ChunkWriter) =
    let chunk = LinkedList<byte[]>().AddFirst(Array.zeroCreate writer.Chunk.Value.Length)
    let mutable offset = rva

    for method in info.Cli.MethodDef.Items do
        match info.MethodBodies.TryGetValue method.Body with
        | false, _ when method.Body.Exists ->
            let struct(size, body) = methodBody chunk method.Body info
            
            // NOTE: For fat (and tiny) formats, "two least significant bits of first byte" indicate the type.
            // TODO: Add checks for no exceptions and extra data sections to generate Tiny format.
            let tiny = size < 64u && body.MaxStack <= 8us (*no exceptions and no extra data sections*)

            // Header
            if tiny
            then ILMethodFlags.TinyFormat ||| (byte size <<< 2) |> writer.WriteU1 // Flags and Size
            else
                let mutable flags = ILMethodFlags.FatFormat // TODO: Set other fat method flags as needed.
                if body.InitLocals then flags <- flags ||| ILMethodFlags.InitLocals
                flags ||| (Size.FatFormat <<< 12) |> writer.WriteU2 // Flags and Size
                writer.WriteU2 body.MaxStack
                writer.WriteU4 size
                writer.WriteU4 0u // LocalVarSigTok // TODO: Figure out how to set local variable information.

            // Body
            writer.WriteBytes(chunk.List, size)
            if not tiny then writer.AlignTo 4u

            let pos = writer.Position
            info.MethodBodies.Item <- method.Body, offset
            offset <- offset + uint32 (writer.Position - pos)

            // TODO: Write extra method data sections.
        | false, _ -> info.MethodBodies.Item <- method.Body, 0u
        | true, _ -> ()

    writer.AlignTo 4u

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
        let total =
            1 // Module
            + tables.ModuleRef.Count
            + tables.AssemblyRef.Count
            + tables.TypeRef.Count
        function
        | ResolutionScope.Module -> 1u, 0u
        | ResolutionScope.ModuleRef moduleRef -> tables.ModuleRef.IndexOf moduleRef, 1u
        | ResolutionScope.AssemblyRef assm -> tables.AssemblyRef.IndexOf assm, 2u
        | bad -> failwithf "Unsupported resolution scope %A" bad
        |> codedIndex total 2

    let extends =
        // TODO: Include TypeSpec table.
        let total = tables.TypeDef.Count + tables.TypeRef.Count
        function
        | Extends.AbstractClass (SimpleIndex tdef)
        | Extends.ConcreteClass (SimpleIndex tdef) -> tables.TypeDef.IndexOf tdef, 0u
        | Extends.TypeRef tref -> tables.TypeRef.IndexOf tref, 1u
        | Extends.Null -> 0u, 0u
        |> codedIndex total 2

    let memberRefParent =
        // TODO: Include TypeSpec table.
        let total =
            tables.TypeDef.Count
            + tables.TypeRef.Count
            + tables.ModuleRef.Count
            + tables.MethodDef.Count
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
            + tables.ModuleRef.Count
            // TypeSpec
            + if tables.Assembly.IsSome then 1 else 0
            + tables.AssemblyRef.Count
            + tables.File.Count
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
        | CustomAttributeType.MemberRef (SimpleIndex mref) -> tables.MemberRef.IndexOf mref, 3u
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

        for index in tables.TypeDef.Handles do
            let tdef = index.Value
            writer.WriteU4 tdef.Flags
            info.StringsStream.WriteStringIndex(tdef.TypeName, writer)
            info.StringsStream.WriteIndex(tdef.TypeNamespace, writer)
            extends.WriteIndex(tdef.Extends, writer)

            tables.Field.WriteSimpleIndex(field, writer)
            field <- field + (tables.TempFieldCounts.GetValueOrDefault index)

            tables.Field.WriteSimpleIndex(method, writer)
            method <- method + (tables.TempMethodCounts.GetValueOrDefault index)

    // Field (0x04)
    for row in tables.Field.Items do
        writer.WriteU2 row.Flags
        info.StringsStream.WriteStringIndex(row.Name, writer)
        info.BlobStream.WriteIndex(row.Signature, writer) // Signature

    // MethodDef (0x06)
    if tables.MethodDef.Count > 0 then
        let mutable param = 1u

        for method in tables.MethodDef.Items do
            writer.WriteU4 info.MethodBodies.[method.Body] // Rva
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
        writer.WriteU2 row.Flags.Value
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




    // ModuleRef (0x1A)
    for moduleRef in tables.ModuleRef.Items do
        info.StringsStream.WriteStringIndex(moduleRef.Name, writer) // Name




    // Assembly (0x20)
    if tables.Assembly.IsSome then
        let assembly = tables.Assembly.Value
        writer.WriteU4 0x8004u // TODO: Determine what HashAlgId should be.
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
        info.BlobStream.WriteIndex(row.PublicKeyOrToken, writer)
        info.StringsStream.WriteStringIndex(row.Name, writer)
        info.StringsStream.WriteStringIndex(row.Culture, writer)
        info.BlobStream.WriteEmpty writer // HashValue // TODO: Figure out how to write the HashValue into a blob.




    // File (0x26)
    for file in tables.File.Items do
        let flags =
            if file.ContainsMetadata
            then 0u
            else 1u
        writer.WriteU4 flags
        info.StringsStream.WriteStringIndex(file.FileName, writer)
        info.BlobStream.WriteIndex(file.HashValue, writer)




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
        if info.UserStringStream.StringCount > 0
        then streamHeader "#US\000"B
        else Unchecked.defaultof<ChunkWriter>
    let guid = streamHeader "#GUID\000\000\000"B
    let blob =
        if info.BlobStream.SignatureCount > 0
        then streamHeader "#Blob\000\000\000"B
        else Unchecked.defaultof<ChunkWriter>

    let inline stream (header: ChunkWriter) content =
        let heap = writer.CreateWriter()
        content heap
        heap.AlignTo 4u
        header.WriteU4 offset
        header.WriteU4 heap.Size
        let size = heap.Size
        offset <- offset + size
        writer.SkipBytes size

    // #~
    stream metadata (tables info)

    // #Strings
    stream strings (Heap.writeStrings info.StringsStream.Count info.Cli)

    // #US
    if us <> Unchecked.defaultof<ChunkWriter> then
        stream us (Heap.writeUS info.UserStringStream)

    // #GUID
    stream guid (Heap.writeGuid info.Cli) // TODO: Fix, GUID heap should be an array of guids, where 1 refers to the first item.

    // #Blob
    if blob <> Unchecked.defaultof<ChunkWriter> then
        stream blob (Heap.writeBlob info.BlobStream)

    do // Stream count
        let mutable count = 3u // #~, #Strings, #GUID
        if info.UserStringStream.StringCount > 0 then count <- count + 1u
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
          MethodBodies = Dictionary<_, _> cli.MethodDef.Count
          StringsStream = Heap.strings cli
          UserStringStream = UserStringHeap 64
          GuidStream = Heap.guid cli
          BlobStream = Heap.blob cli section.Chunk.Value.Length }
    let mutable rva = headerRva

    header info writer
    rva <- rva + Size.CliHeader

    // Strong Name Signature
    if not cli.Header.StrongNameSignature.IsEmpty then
        info.StrongNameSignature.WriteU4 rva
        writer.ResetSize()
        writer.WriteBytes(cli.Header.StrongNameSignature)
        info.StrongNameSignature.WriteU4 writer.Size
        writer.AlignTo 4u
        rva <- rva + writer.Size

    // Method Bodies
    writer.ResetSize()
    bodies rva info writer
    writer.AlignTo 4u
    rva <- rva + writer.Size

    // CLI metadata
    info.Metadata.WriteU4 rva
    writer.ResetSize()
    root info writer
    info.Metadata.WriteU4 writer.Size
    rva <- rva + writer.Size

    section.SkipBytes (rva - headerRva)

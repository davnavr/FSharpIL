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
type private CodedIndex<'T> internal (count: int32, n: int32, indexer: 'T -> uint32 * uint32) =
    let large = count > (65535 <<< n)

    member val IndexSize = if large then 4 else 2

    member _.IndexOf (item: 'T) =
        let index, tag = indexer item
        (index <<< n) ||| tag
    
    member this.WriteIndex(item, writer: ChunkWriter) =
        let index = this.IndexOf item
        if large then writer.WriteU4 index else writer.WriteU2 index

let private codedIndex count n indexer = CodedIndex<_>(count, n, indexer)

[<ReferenceEquality; NoComparison>]
type CliInfo =
    { HeaderRva: uint32
      Metadata: CliMetadata
      mutable MetadataSize: uint32
      mutable MethodBodiesSize: uint32
      StringsStream: Heap<string>
      // US
      GuidStream: Heap<Guid>
      BlobStream: BlobHeap }

    member this.MetadataRva = this.MethodBodiesRva + this.MethodBodiesSize
    member this.StrongNameSignatureRva = this.HeaderRva + Size.CliHeader
    member this.MethodBodiesRva = this.StrongNameSignatureRva + uint32 this.Metadata.Header.StrongNameSignature.Length

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo) (writer: ChunkWriter) =
    let header = info.Metadata.Header
    writer.WriteU4 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    // MetaData
    writer.WriteU4 info.MetadataRva
    writer.WriteU4 info.MetadataSize

    writer.WriteU4 info.Metadata.HeaderFlags // Flags
    writer.WriteU4 0u // EntryPointToken // TODO: Figure out what this token value should be. Is an index into the MethodDef table allowed?

    // Resources
    writer.WriteU4 0u
    writer.WriteU4 0u

    // StrongNameSignature
    writer.WriteU4 info.StrongNameSignatureRva
    writer.WriteU4 info.Metadata.Header.StrongNameSignature.Length

    writer.WriteU8 0UL // CodeManagerTable

    // VTableFixups
    writer.WriteU4 0u
    writer.WriteU4 0u

    writer.WriteU8 0UL // ExportAddressTableJumps
    writer.WriteU8 0UL // ManagedNativeHeader

/// Writes the method bodies.
let bodies (info: CliInfo) (content: ChunkList) =
    ()

/// Writes the contents of the #~ stream (II.24.2.6).
let tables (info: CliInfo) (content: ChunkList) =
    let headers = ChunkWriter.After(content.Tail.Value, 12)
    headers.WriteU4 0u // Reserved
    headers.WriteU1 2uy // MajorVersion
    headers.WriteU1 0uy // MinorVersion

    let heapSizes =
        let mutable bits = 0uy
        if info.StringsStream.IndexSize = 4 then bits <- bits ||| 1uy
        if info.GuidStream.IndexSize = 4 then bits <- bits ||| 2uy
        if info.BlobStream.IndexSize = 4 then bits <- bits ||| 4uy
        bits

    headers.WriteU1 heapSizes
    headers.WriteU1 1uy // Reserved
    headers.WriteU8 info.Metadata.Valid
    headers.WriteU8 0UL // Sorted

    // Rows
    do
        let rows = ChunkWriter.After(content.Tail.Value, info.Metadata.RowCounts.Count * 4)
        for row in info.Metadata.RowCounts do
            rows.WriteU4 row

    // Tables
    let tables = info.Metadata

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

    // TODO: Since we calculate the sizes of each chunk anyway, consider using one ChunkWriter for all of the metadata tables.
    // Module (0x00)
    let mdle =
        let size = 2 + info.StringsStream.IndexSize + (3 * info.GuidStream.IndexSize)
        ChunkWriter.After(content.Tail.Value, size)
    mdle.WriteU2 0us // Generation
    info.StringsStream.WriteStringIndex(tables.Module.Name, mdle)
    info.GuidStream.WriteIndex(tables.Module.Mvid, mdle)
    info.GuidStream.WriteZero mdle // EncId
    info.GuidStream.WriteZero mdle // Encbaseid

    // TypeRef (0x01)
    if tables.TypeRef.Count > 0 then
        let typeRef =
            let size = resolutionScope.IndexSize + (2 * info.StringsStream.IndexSize)
            ChunkWriter.After(content.Tail.Value, size * tables.TypeRef.Count)

        for tref in tables.TypeRef.Items do
            resolutionScope.WriteIndex(tref.ResolutionScope, typeRef)
            info.StringsStream.WriteStringIndex(tref.TypeName, typeRef)
            info.StringsStream.WriteIndex(tref.TypeNamespace, typeRef)

    // TypeDef (0x02)
    if tables.TypeDef.Count > 0 then
        let typeDef =
            let size =
                4
                + (2 * info.StringsStream.IndexSize)
                + extends.IndexSize
                + tables.Field.SimpleIndexSize
                + tables.MethodDef.SimpleIndexSize
            ChunkWriter.After(content.Tail.Value, size * tables.TypeDef.Count)

        let mutable field = 1u
        let mutable method = 1u

        for tdef in tables.TypeDef.Items do
            typeDef.WriteU4 tdef.Flags
            info.StringsStream.WriteStringIndex(tdef.TypeName, typeDef)
            info.StringsStream.WriteIndex(tdef.TypeNamespace, typeDef)
            extends.WriteIndex(tdef.Extends, typeDef)

            // Field
            let field' = if tdef.FieldList.IsEmpty then 0u else field
            field <- uint32 tdef.FieldList.Length
            tables.Field.WriteSimpleIndex(field', typeDef)

            // Method
            let method' = if tdef.MethodList.IsEmpty then 0u else method
            method <- uint32 tdef.MethodList.Length
            tables.Field.WriteSimpleIndex(method', typeDef)

    // Field (0x04)
    if tables.Field.Count > 0 then
        let field =
            let size =
                2 // Flags
                + info.StringsStream.IndexSize // Name
                + info.BlobStream.IndexSize // Signature
            ChunkWriter.After(content.Tail.Value, size * tables.Field.Count)

        for row in tables.Field.Items do
            field.WriteU2 row.Flags
            info.StringsStream.WriteStringIndex(row.Name, field)
            invalidOp "TODO: Write index for field signatures."

    // MethodDef (0x06)
    if tables.MethodDef.Count > 0 then
        let methodDef =
            let size =
                8 // RVA, ImplFlags, Flags
                + info.StringsStream.IndexSize // Name
                + info.BlobStream.IndexSize // Signature
                + if tables.Param.Length < 65536 then 2 else 4 // ParamList
            ChunkWriter.After(content.Tail.Value, size * tables.MethodDef.Count)

        let mutable param = 1u

        for method in tables.MethodDef.Items do
            methodDef.WriteU4 0u // RVA // TODO: Write the RVA to the method body.
            methodDef.WriteU2 method.ImplFlags
            methodDef.WriteU2 method.Flags
            info.StringsStream.WriteStringIndex(method.Name, methodDef)
            info.BlobStream.WriteIndex(method.Signature, methodDef) // Signature

            let param' = if method.ParamList.IsEmpty then 0u else param // Param
            param <- param + uint32 method.ParamList.Length
            // TODO: Since both the Param table (which is an ImmutableArray) and the ImmutableTable class have an implementation for writing an index, maybe make it an extension method?
            // If doing the above, maybe add an extension member to allow retrieval of IndexSize as well.
            if tables.Param.Length < 65536
            then methodDef.WriteU2 param'
            else methodDef.WriteU4 param'

    // Param (0x08)
    if tables.Param.Length > 0 then
        let param =
            let size = 4 + info.StringsStream.IndexSize // Name
            ChunkWriter.After(content.Tail.Value, size * tables.Param.Length)

        for sequence, row in tables.Param do
            param.WriteU2 row.Flags.Flags
            param.WriteU2(sequence + 1)
            info.StringsStream.WriteIndex(row.ParamName, param)




    // MemberRef (0x0A)
    if tables.MemberRef.Count > 0 then
        let memberRef =
            let size =
                memberRefParent.IndexSize // Class
                + info.StringsStream.IndexSize // Name
                + info.BlobStream.IndexSize // Signature
            ChunkWriter.After(content.Tail.Value, size * tables.MemberRef.Count)

        for row in tables.MemberRef.Items do
            memberRefParent.WriteIndex(row.Class, memberRef)
            info.StringsStream.WriteStringIndex(row.MemberName, memberRef)

            // Signature
            match row with
            | MethodRef method -> info.BlobStream.WriteIndex(method.Signature, memberRef)




    // CustomAttribute (0x0C)
    if tables.CustomAttribute.Length > 0 then
        let writer =
            let size =
                customAttriuteParent.IndexSize // Parent
                + customAttributeType.IndexSize // Type
                + info.BlobStream.IndexSize // Value
            ChunkWriter.After(content.Tail.Value, size * tables.CustomAttribute.Length)

        for row in tables.CustomAttribute do
            customAttriuteParent.WriteIndex(row.Parent, writer)
            customAttributeType.WriteIndex(row.Type, writer)
            info.BlobStream.WriteIndex(row.Value, writer)




    // Assembly (0x20)
    if tables.Assembly.IsSome then
        let writer =
            let size =
                16 // HashAlgId, MajorVersion, MinorVersion, BuildNumber, RevisionNumber, Flags
                + info.BlobStream.IndexSize // PublicKey
                + (2 * info.StringsStream.IndexSize) // Name, Culture
            ChunkWriter.After(content.Tail.Value, size)

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
    if tables.AssemblyRef.Count > 0 then
        let assemblyRef =
            let size =
                12 // MajorVersion, MinorVersion, BuildNumber, RevisionNumber, Flags
                + (2 * info.BlobStream.IndexSize) // PublicKeyOrToken, HashValue
                + (2 * info.StringsStream.IndexSize) // Name, Culture
            ChunkWriter.After(content.Tail.Value, size * tables.AssemblyRef.Count)

        for row in tables.AssemblyRef.Items do
            assemblyRef.WriteU2 row.Version.Major
            assemblyRef.WriteU2 row.Version.Minor
            assemblyRef.WriteU2 row.Version.Build
            assemblyRef.WriteU2 row.Version.Revision
            assemblyRef.WriteU4 row.Flags
            info.BlobStream.WriteEmpty assemblyRef // PublicKeyOrToken // TODO: Figure out how to write the PublicKeyOrToken into a blob.
            info.StringsStream.WriteStringIndex(row.Name, assemblyRef)
            info.StringsStream.WriteStringIndex(row.Culture, assemblyRef)
            info.BlobStream.WriteEmpty assemblyRef // HashValue // TODO: Figure out how to write the HashValue into a blob.

    // NestedClass (0x29)
    if tables.NestedClass.Length > 0 then
        let nestedClass =
            ChunkWriter.After(content.Tail.Value, 2 * tables.TypeDef.SimpleIndexSize * tables.NestedClass.Length)

        for row in tables.NestedClass do
            tables.TypeDef.WriteSimpleIndex(row.NestedClass, nestedClass)
            tables.TypeDef.WriteSimpleIndex(row.EnclosingClass, nestedClass)

    // TODO: Write more tables.
    ()

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) (content: ChunkList) =
    let version = MetadataVersion.toArray info.Metadata.MetadataVersion
    let fieldsSize =
        Magic.CliSignature.Length
        + 12 // MajorVersion, MinorVersion, Reserved, Length
        + version.Length
        + 4 // Flags, # of Streams
    let writer = ChunkWriter.After(content.Tail.Value, fieldsSize)
    writer.WriteBytes Magic.CliSignature
    writer.WriteU2 1us // MajorVersion
    writer.WriteU2 1us // MinorVersion
    writer.WriteU4 0u // Reserved
    writer.WriteU4 version.Length
    writer.WriteBytes version
    writer.WriteU2 0us // Flags

    let streams =
        let mutable count = 3u // #~, #Strings, #GUID
        // TODO: Include #US stream in stream count.
        if info.BlobStream.Count > 0 then count <- count + 1u
        count
    writer.WriteU2 streams // Streams

    if writer.FreeBytes <> 0 then
        invalidOp "The metadata root did not fit in the current chunk."

    let mutable offset = uint32 fieldsSize

    // Stream headers
    let streamHeader name =
        if Array.length name % 4 <> 0 then
            invalidArg (nameof name) "The length of the stream header name must be a multiple of four."
        let location = content.AddAfter(content.Tail.Value, Array.zeroCreate<byte> 8)
        content.AddAfter(location, name) |> ignore
        offset <- offset + 8u + uint32 name.Length
        ChunkWriter(location)

    let metadata = streamHeader "#~\000\000"B
    let strings = streamHeader "#Strings\000\000\000\000"B
    let us =
        // if info. // TODO: Write #US stream header
        // then streamHeader "#US\000"B
        null
    let guid = streamHeader "#GUID\000\000\000"B
    let blob =
        if info.BlobStream.Count > 0
        then streamHeader "#Blob\000\000\000"B
        else null

    do // #~ stream
        content.PushSize()
        tables info content
        let size = content.PopSize() // TODO: Make local function for popping size and updating offset.
        metadata.WriteU4 offset
        metadata.WriteU4 size
        offset <- offset + size

    do // #Strings
        
        content.PushSize()
        Heap.writeStrings info.StringsStream content
        // TODO: Should heaps be padded to the nearest 4-byte boundary?
        let size = content.PopSize()
        strings.WriteU4 offset
        strings.WriteU4 size
        offset <- offset + size

    // user string
    // if info.

    do // #GUID
        content.PushSize()
        Heap.writeGuid info.GuidStream content
        let size = content.PopSize()
        guid.WriteU4 offset
        guid.WriteU4 size
        offset <- offset + size

    if blob <> null then
        content.PushSize()
        info.BlobStream.WriteHeap content
        let size = content.PopSize()
        blob.WriteU4 offset
        blob.WriteU4 size
        offset <- offset + size

    // TODO: Consider aligning heaps to 4-byte boundary

    ()

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (headerRva: uint32) (content: ChunkList) =
    let info =
        { HeaderRva = headerRva
          Metadata = cli
          MetadataSize = Unchecked.defaultof<uint32>
          MethodBodiesSize = Unchecked.defaultof<uint32>
          StringsStream = Heap.strings cli
          GuidStream = Heap.guid cli
          BlobStream = BlobHeap cli }
    let tail = content.Tail.Value

    // Since header requires the calculations of sizes, we write the data first and prepend the header afterward.

    let strongNameSignature = cli.Header.StrongNameSignature
    if not strongNameSignature.IsEmpty then
        content.AddAfter(tail, cli.Header.StrongNameSignature.AsSpan().ToArray()) |> ignore

    // Method bodies
    content.PushSize()
    bodies info content
    info.MethodBodiesSize <- content.PopSize()

    // CLI metadata
    content.PushSize()
    root info content
    info.MetadataSize <- content.PopSize()

    ChunkWriter.After(tail, int32 Size.CliHeader) |> header info
    ()

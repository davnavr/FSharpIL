﻿[<RequireQualifiedAccess>]
module ILInfo.ILOutput

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Text

open Microsoft.FSharp.Core.Printf

open FSharpIL.Metadata
open FSharpIL.PortableExecutable
open FSharpIL.Reading

let indented wr = new IndentedTextWriter(wr, 4)

let inline heading name (offset: FileOffset) wr = fprintfn wr "// ----- %s (0x%016X)" name (uint64 offset)

let inline fieldf name size printer wr value = fprintfn wr "// %s (%i bytes) = %a" name size printer value

let inline field name printer wr (value: 'Value) = fieldf name sizeof<'Value> printer wr value

let inline optional printer wr value  = ValueOption.iter (printer wr) value

// TODO: Format identifier strings correctly.
//let inline sqstring
//let inline id

/// Prints a string surrounded by double quotes (II.5.2).
let qstring (wr: #TextWriter) (str: string) =
    wr.Write '"'
    for i = 0 to str.Length - 1 do
        match str.[i] with
        | '\t' | '\n' as c ->
            wr.Write "\\"
            wr.Write c
        | '"' | '\\' | '\r' | '\000' as c ->
            fprintf wr "\\%03o" (uint16 c)
        | c -> wr.Write c
    wr.Write '"'

/// Prints a list of bytes in hexadecimal (II.5.5).
let bytes wr (bytes: ImmutableArray<byte>) =
    let max = bytes.Length - 1
    for i = 0 to max do
        fprintf wr "%02X" bytes.[i]
        if i < max then wr.Write ' '

// TODO: Format values nicely

let coffHeader header offset wr =
    heading "COFF Header" offset wr
    field "Machine" Print.enumeration wr header.Machine
    field "NumberOfSections" Print.integer wr header.NumberOfSections
    field "TimeDateStamp" Print.integer wr header.TimeDateStamp
    field "SymbolTablePointer" Print.integer wr header.SymbolTablePointer
    field "SymbolCount" Print.integer wr header.SymbolCount
    field "OptionalHeaderSize" Print.integer wr header.OptionalHeaderSize
    field "Characteristics" Print.bitfield wr header.Characteristics
    wr

let standardFields fields offset wr =
    heading "Standard Fields" offset wr
    field "Magic" Print.enumeration wr fields.Magic
    field "MajorLinkerVersion" Print.integer wr fields.LMajor
    field "MinorLinkerVersion" Print.integer wr fields.LMinor
    field "SizeOfCode" Print.integer wr fields.CodeSize
    field "SizeOfInitializedData" Print.integer wr fields.InitializedDataSize
    field "SizeOfUninitializedData" Print.integer wr fields.UninitializedDataSize
    field "EntryPointRva" Print.integer wr fields.EntryPointRva
    field "BaseOfCode" Print.integer wr fields.BaseOfCode
    optional (field "BaseOfData" Print.integer) wr fields.BaseOfData
    wr

let ntSpecificFields fields offset wr =
    let (salignment, falignment) = fields.Alignment
    heading "NT Specific Fields" offset wr
    // TODO: Write different size fields if PE32+
    field "ImageBase" Print.integer wr (uint32 fields.ImageBase) // NOTE: This is 8 bytes long instead for PE32+
    field "SectionAlignment" Print.integer wr salignment
    field "FileAlignment" Print.integer wr falignment
    field "MajorOperatingSystemVersion" Print.integer wr fields.OSMajor
    field "MinorOperatingSystemVersion" Print.integer wr fields.OSMinor
    field "MajorImageVersion" Print.integer wr fields.UserMajor
    field "MinorImageVersion" Print.integer wr fields.UserMinor
    field "MajorSubsystemVersion" Print.integer wr fields.SubSysMajor
    field "MinorSubsystemVersion" Print.integer wr fields.SubSysMinor
    field "Win32VersionValue" Print.integer wr fields.Win32VersionValue
    field "SizeOfImage" Print.integer wr fields.ImageSize
    field "SizeOfHeaders" Print.integer wr fields.HeadersSize
    field "FileChecksum" Print.integer wr fields.FileChecksum
    field "Subsystem" Print.enumeration wr fields.Subsystem
    field "DllCharacteristics" Print.bitfield wr fields.DllFlags
    field "SizeOfStackReserve" Print.integer wr (uint32 fields.StackReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfStackCommit" Print.integer wr (uint32 fields.StackCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfHeapReserve" Print.integer wr (uint32 fields.HeapReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfHeapCommit" Print.integer wr (uint32 fields.HeapCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    field "LoaderFlags" Print.integer wr fields.LoaderFlags
    field "NumberOfDataDirectories" Print.integer wr fields.NumberOfDataDirectories
    wr.WriteLine()
    fprintfn wr ".imagebase 0x%08X" fields.ImageBase
    fprintfn wr ".file alignment 0x%08X" falignment
    fprintfn wr ".stackreserve 0x%08X" fields.StackReserveSize
    fprintfn wr ".subsystem 0x%04X" (uint16 fields.Subsystem)
    wr.WriteLine()
    wr

let dataDirectoryNames =
    [|
        "Export Table"
        "Import Table"
        "Resource Table"
        "Exception Table"
        "Certificate Table"
        "Base Relocation Table"
        "Debug"
        "Copyright"
        "Global Pointer"
        "TLS Table"
        "Load Configuration Table"
        "Bound Import"
        "Import Address Table"
        "Delay Import Descriptor"
        "CLI Header"
        "Reserved"
    |]

let dataDirectories (directories: ParsedDataDirectories) offset wr =
    heading "Data Directories" offset wr
    for i = 0 to directories.Length - 1 do
        let name =
            if i < dataDirectoryNames.Length
            then sprintf "%s Table" dataDirectoryNames.[i]
            else "Unknown"
        field name Print.rvaAndSize wr directories.[i]
    wr

let sectionHeaders (headers: ParsedSectionHeaders) (offset: FileOffset) wr =
    for i = 0 to headers.Length - 1 do
        let header = headers.[i]
        heading (sprintf "\"%O\" Section Header" header.SectionName) (offset + (uint64 i * 40UL)) wr
        field "Name" (fun wr () -> wr.Write header.SectionName) wr () // TODO: Make sure length includes padding // TODO: Fix length
        field "VirtualSize" Print.integer wr header.Data.VirtualSize
        field "VirtualAddress" Print.integer wr header.Data.VirtualAddress
        field "SizeOfRawData" Print.integer wr header.Data.RawDataSize
        field "PointerToRawData" Print.integer wr header.Data.RawDataPointer
        field "PointerToRelocations" Print.integer wr header.PointerToRelocations
        field "PointerToLineNumbers" Print.integer wr header.PointerToLineNumbers
        field "NumberOfRelocations" Print.integer wr header.NumberOfRelocations
        field "NumberOfLineNumbers" Print.integer wr header.NumberOfLineNumbers
        field "Characteristics" Print.bitfield wr header.Characteristics
    wr

let cliHeader (header: ParsedCliHeader) offset wr =
    heading "CLI Header" offset wr
    field "Cb" Print.integer wr header.Size
    field "MajorRuntimeVersion" Print.integer wr header.MajorRuntimeVersion
    field "MinorRuntimeVersion" Print.integer wr header.MinorRuntimeVersion
    field "MetaData" Print.rvaAndSize wr header.MetaData
    field "Flags" Print.bitfield wr header.Flags
    field "EntryPointToken" Print.integer wr header.EntryPointToken // TODO: Show what table the EntryPointToken refers to.
    field "Resources" Print.rvaAndSize wr header.Resources
    field "StrongNameSignature" Print.integer wr header.StrongNameSignature
    field "CodeManagerTable" Print.integer wr header.CodeManagerTable
    field "VTableFixups" Print.rvaAndSize wr header.VTableFixups
    field "ExportAddressTableJumps" Print.integer wr header.ExportAddressTableJumps
    field "ManagedNativeHeader" Print.integer wr header.ManagedNativeHeader
    wr.WriteLine()
    fprintfn wr ".corflags 0x%08X" (uint32 header.Flags)
    wr.WriteLine()
    wr

let metadataRoot (root: ParsedMetadataRoot) offset wr =
    heading "CLI Metadata Root" offset wr
    field "MajorVersion" Print.integer wr root.MajorVersion
    field "MinorVersion" Print.integer wr root.MinorVersion
    field "Reserved" Print.integer wr root.Reserved
    field "Length" Print.integer wr root.Version.Length
    // TODO: Make sure length includes padding
    fieldf "Version" root.Version.Length (fun wr () -> fprintf wr "\"%O\"" root.Version) wr ()
    field "Flags" Print.integer wr root.Flags
    field "Streams" Print.integer wr root.Streams
    wr

let streamHeader (header: ParsedStreamHeader) _ offset wr =
    let name = Encoding.ASCII.GetString(header.Name.AsSpan())
    heading (sprintf "\"%s\" Stream Header" name) offset wr
    field "Offset" Print.integer wr header.Offset
    field "Size" Print.integer wr header.Size
    fieldf "Name" header.Name.Length (fun wr -> fprintf wr "\"%s\"") wr name
    wr

let metadataTablesHeader header offset wr =
    let rec rows wr flags =
        match flags with
        | MetadataTableFlags.None -> ()
        | _ ->
            let flags' = flags >>> 1
            match header.Rows.TryGetValue(flags &&& MetadataTableFlags.Module) with
            | true, count -> fprintf wr "0x%08X, " count
            | false, _ -> ()
            rows wr flags'
    heading "Metadata Tables Header" offset wr
    field "Reserved" Print.integer wr header.Reserved1
    field "MajorVersion" Print.integer wr header.MajorVersion
    field "MinorVersion" Print.integer wr header.MinorVersion
    field "HeapSizes" Print.bitfield wr header.HeapSizes
    field "Reserved" Print.integer wr header.Reserved2
    field "Valid" Print.bitfield wr header.Valid
    field "Sorted" Print.bitfield wr header.Sorted
    fieldf
        "Rows"
        (4 * header.Rows.Count)
        (fun wr () ->
            wr.Write '['
            rows wr header.Valid
            wr.Write ']')
        wr
        ()

let inline rowIndex wr i = fprintfn wr "// (0x%08X)" (i + 1)

let moduleTable (tables: ParsedMetadataTables) strings guid (wr: TextWriter) =
    wr.WriteLine()
    wr.WriteLine "// Module (0x00)"
    for i = 0 to int32 tables.Module.RowCount - 1 do
        let row = tables.Module.[i]
        let inline fguid name id = fieldf name tables.Header.HeapSizes.GuidSize (Print.guid guid) wr id
        field "Generation" Print.integer wr row.Generation
        wr.Write ".module "
        Print.identifier strings wr row.Name
        fprintfn wr "// %O" row.Name
        fguid "Mvid" row.Mvid
        fguid "GenerationId" row.EncId
        fguid "BaseGenerationId" row.EncBaseId

// TODO: Print DottedNames correctly, see II.5.3

let assemblyTable (tables: ParsedMetadataTables) (strings: ParsedStringsStream) blobs wr =
    match tables.Assembly with
    | ValueSome table ->
        for i = 0 to int32 table.RowCount - 1 do
            let row = table.[i]
            fprintfn wr ".assembly %s" (strings.GetString row.Name)
            wr.WriteLine '{'
            let wr' = indented wr
            fprintfn wr' ".hash algorithm 0x%08X" (int32 row.HashAlgId)

            // TODO: Reduce amount of duplicated code with AssemblyRef
            match strings.GetString row.Culture with
            | ""
            | null -> ()
            | culture -> fprintfn wr' ".culture %a" qstring culture

            fprintfn wr' ".ver %i:%i:%i:%i" row.MajorVersion row.MinorVersion row.BuildNumber row.RevisionNumber

            wr.WriteLine '}'
            wr.WriteLine()
    | ValueNone -> ()

let assemblyRefTable (tables: ParsedMetadataTables) (strings: ParsedStringsStream) blobs wr =
    match tables.AssemblyRef with
    | ValueSome table ->
        for i = 0 to int32 table.RowCount - 1 do
            let row = table.[i]
            fprintfn wr ".assembly extern %s" (strings.GetString row.Name)
            wr.WriteLine '{'
            let wr' = indented wr

            // TODO: List custom attributes of AssemblyRef.

            match strings.GetString row.Culture with
            | ""
            | null -> ()
            | culture -> fprintfn wr' ".culture %a" qstring culture

            match blobs with
            | ValueSome(blobs': ParsedBlobStream) ->
                match blobs'.TryReadBytes row.PublicKeyOrToken with
                | Ok token ->
                    wr'.Write ".hash = ("
                    bytes wr' token
                    wr'.WriteLine ')'
                | Error err -> fprintfn wr' "// error : Unable to read assembly hash, %O" err

                match blobs'.TryReadBytes row.PublicKeyOrToken with
                | Ok token ->
                    wr'.Write ".publickey"
                    if not(row.Flags.HasFlag AssemblyNameFlags.PublicKey) then
                        wr'.Write "token"
                    wr'.Write " = ("
                    bytes wr' token
                    wr'.WriteLine ')'
                | Error err -> fprintfn wr' "// error : Unable to read public key or token, %O" err
            | ValueNone -> wr'.Write "// TODO: How to deal with missing blob heap when writing assembly references?"

            fprintfn wr' ".ver %i:%i:%i:%i" row.MajorVersion row.MinorVersion row.BuildNumber row.RevisionNumber

            wr.WriteLine '}'
            wr.WriteLine()
    | ValueNone -> ()

let typeRefTable (tables: ParsedMetadataTables) strings (wr: TextWriter) =
    match tables.TypeRef with
    | ValueSome table ->
        wr.WriteLine()
        wr.WriteLine "// TypeRef (0x01)"
        for i = 0 to int32 table.RowCount - 1 do
            let row = table.[i]
            wr.WriteLine()
            rowIndex wr i
            fieldf
                "ResolutionScope"
                (CodedIndex.resolutionScopeParser(tables.Header.Rows).Length)
                (fun wr { ParsedTypeRefRow.ResolutionScope = rscope } ->
                    match rscope with
                    | ParsedResolutionScope.Null -> "Null"
                    | ParsedResolutionScope.Unknown _ -> "Unknown"
                    | ParsedResolutionScope.Module i -> sprintf "Module (0x%08X)" i
                    | ParsedResolutionScope.ModuleRef i -> sprintf "ModuleRef (0x%08X)" i
                    | ParsedResolutionScope.AssemblyRef i -> sprintf "AssemblyRef (0x%08X)" i
                    | ParsedResolutionScope.TypeRef i -> sprintf "TypeRef (0x%08X)" i
                    |> wr.Write)
                wr
                row
            fieldf "TypeName" tables.Header.HeapSizes.StringSize (Print.identifier strings) wr row.TypeName
            fieldf "TypeNamespace" tables.Header.HeapSizes.StringSize (Print.identifier strings) wr row.TypeNamespace
    | ValueNone -> ()

let fieldRow (table: ParsedFieldTable) tables i vfilter (strings: ParsedStringsStream) (blobs: ParsedBlobStream) (wr: #TextWriter) =
    match table.TryGetRow i with
    | Ok row when VisibilityFilter.field vfilter row.Flags ->
        wr.Write ".field "

        match row.Flags &&& FieldAttributes.FieldAccessMask with
        | FieldAttributes.Public -> "public "
        | FieldAttributes.Assembly -> "assembly "
        | FieldAttributes.FamORAssem -> "famorassem "
        | FieldAttributes.FamANDAssem -> "famandassem "
        | FieldAttributes.Family -> "family "
        | FieldAttributes.Private -> "private "
        | FieldAttributes.PrivateScope
        | _ -> "compilercontrolled "
        |> wr.Write

        if row.Flags.HasFlag FieldAttributes.Static then wr.Write "static "
        if row.Flags.HasFlag FieldAttributes.InitOnly then wr.Write "initonly "
        if row.Flags.HasFlag FieldAttributes.Literal then wr.Write "literal "
        if row.Flags.HasFlag FieldAttributes.NotSerialized then wr.Write "notserialized "
        if row.Flags.HasFlag FieldAttributes.SpecialName then wr.Write "specialname "
        if row.Flags.HasFlag FieldAttributes.RTSpecialName then wr.Write "rtspecialname "
        if row.Flags.HasFlag FieldAttributes.HasFieldMarshal then
            wr.Write "marshal ("
            // TODO: Write field marshalling information.
            wr.Write ") "

        match blobs.TryReadFieldSig(row.Signature) with
        | Ok signature ->
            // TODO: Include custom modifiers of field type.
            TypeName.encoded signature.FieldType tables strings blobs wr
        | Error err -> fprintfn wr "Error reading type %O" err

        fprintf wr " '%s' " (strings.GetString row.Name)
        rowIndex wr (int32 i)
        wr.WriteLine()
    | Ok _ -> ()
    | Error err -> fprintfn wr "// error : Cannot find field %i, %s" i err.Message

let typeDefFields
    (tables: ParsedMetadataTables)
    i
    (row: inref<ParsedTypeDefRow>)
    vfilter
    strings
    (blobs: ParsedBlobStream voption)
    (wr: #TextWriter)
    =
    match tables.Field with
    | ValueSome ftable when row.FieldList > 0u ->
        let ttable = tables.TypeDef.Value
        let max =
            if i = int32 ttable.RowCount - 1
            then tables.Header.Rows.[MetadataTableFlags.Field]
            else ttable.[i + 1].FieldList
        let mutable fieldi = row.FieldList
        while fieldi < max do
            // TODO: Report an error if blobs does not exist
            fieldRow ftable tables fieldi vfilter strings blobs.Value wr
            fieldi <- fieldi + 1u
    | ValueSome _
    | ValueNone -> ()

// TODO: Include exception handling information.
let rec methodBodyInstruction (wr: #TextWriter) (operand: outref<ParsedOperand>) (body: byref<MethodBodyParser>) =
    let offset = body.Offset
    match body.Read &operand with
    | _, Error err ->
        fprintfn wr "// error : %O" err
        // TODO: Show remaining method body bytes on error
    | 0u, _ -> ()
    | _, Ok opcode ->
        fprintf wr "IL_%04X: " offset
        wr.Write(ParsedOpcode.name opcode)
        match operand with
        | ParsedOperand.U1 num -> fprintf wr " %i // 0x%02X" num num
        | ParsedOperand.U2 num -> fprintf wr " %i // 0x%04X" num num
        | ParsedOperand.None -> ()
        wr.WriteLine()
        methodBodyInstruction wr &operand &body

let methodBodyInstructions data (body: MethodBodyStream) =
    let wr = body.Parse()
    let mutable operand = ParsedOperand()
    let mutable cont, error = true, None
    while error.IsNone do
        match wr.Read &operand with
        | 0u, _ -> ()
    ()

let methodRow
    (table: ParsedMethodDefTable)
    tables
    i
    vfilter
    (strings: ParsedStringsStream)
    (blobs: ParsedBlobStream)
    (bodies: ParsedMethodBodies)
    (wr: #TextWriter)
    =
    match table.TryGetRow i with
    | Ok row when VisibilityFilter.methodDef vfilter row.Flags ->
        wr.Write ".method "

        match row.Flags &&& MethodAttributes.MemberAccessMask with
        | MethodAttributes.Public -> "public "
        | MethodAttributes.Assembly -> "assembly "
        | MethodAttributes.FamORAssem -> "famorassem "
        | MethodAttributes.FamANDAssem -> "famandassem "
        | MethodAttributes.Family -> "family "
        | MethodAttributes.Private -> "private "
        | MethodAttributes.PrivateScope
        | _ -> "compilercontrolled "
        |> wr.Write
        
        if row.Flags.HasFlag MethodAttributes.Final then wr.Write "final "
        if row.Flags.HasFlag MethodAttributes.CheckAccessOnOverride then wr.Write "strict "
        if row.Flags.HasFlag MethodAttributes.HideBySig then wr.Write "hidebysig "
        if row.Flags.HasFlag MethodAttributes.NewSlot then wr.Write "newslot "
        if row.Flags.HasFlag MethodAttributes.Abstract then wr.Write "abstract "
        if row.Flags.HasFlag MethodAttributes.Virtual then wr.Write "virtual "
        if row.Flags.HasFlag MethodAttributes.Static then wr.Write "static "
        if row.Flags.HasFlag MethodAttributes.SpecialName then wr.Write "specialname "
        if row.Flags.HasFlag MethodAttributes.RTSpecialName then wr.Write "rtspecialname "

        match blobs.TryReadMethodDefSig row.Signature with
        | Ok signature ->
            wr.WriteLine()
            let wr' = indented wr
            match signature.HasThis with
            | ParsedMethodThis.NoThis -> ()
            | ParsedMethodThis.HasThis -> wr'.Write "instance "
            | ParsedMethodThis.ExplicitThis -> wr'.Write "instance explicit "

            match signature.CallingConvention with
            | Default
            | Generic _ -> ()
            | VarArg -> wr'.Write "vararg "

            let struct(retmod, rtype) = signature.ReturnType
            TypeName.retType rtype tables strings blobs wr'
            TypeName.cmodifiers retmod tables strings blobs wr'

            wr'.Write " '"
            wr'.Write(strings.GetString row.Name)
            wr'.Write "' "

            if signature.Parameters.IsEmpty
            then wr'.Write '('
            else
                wr'.WriteLine '('
                let wr'' = indented wr'
                for i = 0 to signature.Parameters.Length - 1 do
                    if i > 0 then wr''.WriteLine ','

                    let prow =
                        match tables.Param with
                        | ValueSome ptable ->
                            match ptable.TryGetRow(row.ParamList + uint32 i) with
                            | Ok prow -> ValueSome prow
                            | Error _ -> ValueNone
                        | ValueNone -> ValueNone

                    match prow with
                    | ValueSome { Flags = pflags } ->
                        if pflags.HasFlag ParameterAttributes.In then wr''.Write "in "
                        if pflags.HasFlag ParameterAttributes.Optional then wr''.Write "opt "
                        if pflags.HasFlag ParameterAttributes.Out then wr''.Write "out "

                        // TODO: Include marshalling information
                    | ValueNone -> ()

                    let param = signature.Parameters.[i]
                    TypeName.paramType param.ParamType tables strings blobs wr''
                    TypeName.cmodifiers param.CustomModifiers tables strings blobs wr''

                    match prow with
                    | ValueSome prow ->
                        match strings.GetString prow.Name with
                        | null
                        | "" -> ()
                        | name ->
                            wr''.Write " '"
                            wr''.Write name
                            wr''.Write '''
                    | ValueNone -> ()
                wr'.WriteLine()
            wr'.Write ')'

            match row.ImplFlags &&& MethodImplAttributes.CodeTypeMask with
            | MethodImplAttributes.Native -> wr'.Write " native"
            | MethodImplAttributes.Runtime -> wr'.Write " runtime"
            | MethodImplAttributes.IL
            | _ -> wr'.Write " cil"

            wr'.Write ' '
            match row.ImplFlags &&& MethodImplAttributes.ManagedMask with
            | MethodImplAttributes.Unmanaged -> wr'.Write "un"
            | _ -> ()
            wr'.Write "managed"

            if row.ImplFlags.HasFlag MethodImplAttributes.ForwardRef then wr'.Write " forwardref"
            if row.ImplFlags.HasFlag MethodImplAttributes.InternalCall then wr'.Write " internalcall"
            if row.ImplFlags.HasFlag MethodImplAttributes.NoInlining then wr'.Write " noinlining"
            if row.ImplFlags.HasFlag MethodImplAttributes.NoOptimization then wr'.Write " nooptimization"
            if row.ImplFlags.HasFlag MethodImplAttributes.Synchronized then wr'.Write " synchronized"
            wr'.WriteLine()
        | Error err -> fprintfn wr "// error : Invalid method signature, %O" err

        wr.WriteLine '{'
        let wr' = indented wr
        if row.Rva > 0u then
            match bodies.TryParseBody row.Rva with
            | Ok(header, data, body) ->
                fprintfn wr' "// Method begins at RVA 0x%08X" row.Rva
                fprintfn wr' "// Code size %i (0x%08X)" header.CodeSize header.CodeSize
                fprintfn wr' ".maxstack %i" header.MaxStack

                match header.LocalVarSigTok with
                | ValueSome _ ->
                    wr'.Write ".locals "
                    if header.Flags.HasFlag ILMethodFlags.InitLocals then
                        wr'.Write "init "
                    wr'.WriteLine '('
                    wr'.WriteLine ')'
                | ValueNone -> ()

                let mutable operand = ParsedOperand()
                let mutable parser = body.Parse()
                methodBodyInstruction wr' &operand &parser
            | Error err -> fprintfn wr' "// %O" err
        wr.WriteLine '}'
    | Ok _ -> ()
    | Error err -> fprintfn wr "// error : Cannot find method %i, %s" i err.Message

let typeDefMethods
    (tables: ParsedMetadataTables)
    i
    (row: inref<ParsedTypeDefRow>)
    vfilter
    strings
    (blobs: _ voption)
    (wr: #TextWriter)
    =
    match tables.MethodDef with
    | ValueSome mtable when row.MethodList > 0u -> // TODO: Avoid duplicating code with field printing.
        let ttable = tables.TypeDef.Value
        let max =
            if i = int32 ttable.RowCount - 1
            then tables.Header.Rows.[MetadataTableFlags.MethodDef]
            else ttable.[i + 1].MethodList
        let mutable methodi = row.MethodList
        while methodi < max do
            if methodi > 0u then wr.WriteLine()
            // TODO: Report an error if blobs does not exist
            methodRow mtable tables methodi vfilter strings blobs.Value (tables.GetMethodBodies()) wr
            methodi <- methodi + 1u
    | ValueSome _
    | ValueNone -> ()

let propertyRow
    (ptable: ParsedPropertyTable)
    (mtable: PropertyMapTable)
    (tables: ParsedMetadataTables)
    i
    vfilter
    (strings: ParsedStringsStream)
    (blobs: ParsedBlobStream)
    (wr: #TextWriter)
    =
    match ptable.TryGetRow i with
    | Ok row -> // TODO: Only include properties with specified visibility
        wr.Write ".property "
        if row.Flags.HasFlag PropertyAttributes.SpecialName then wr.Write "specialname "
        if row.Flags.HasFlag PropertyAttributes.RTSpecialName then wr.Write "rtspecialname "

        match blobs.TryReadPropertySig row.Type with
        | Ok signature ->
            if signature.HasThis then wr.Write "instance "
            TypeName.encoded signature.PropertyType tables strings blobs wr
            TypeName.cmodifiers signature.CustomModifiers tables strings blobs wr
            wr.Write " '"
            wr.Write(strings.GetString row.Name)
            wr.Write '''
            if not signature.Parameters.IsEmpty then
                wr.WriteLine '('
                let wr' = indented wr
                for i = 0 to signature.Parameters.Length - 1 do
                    if i > 0 then wr'.WriteLine ','
                    let param = signature.Parameters.[i]
                    TypeName.paramType param.ParamType tables strings blobs wr'
                    TypeName.cmodifiers param.CustomModifiers tables strings blobs wr'
                wr.WriteLine ')'
            else wr.WriteLine()
        | Error err -> fprintfn wr "// error : Cannot read property signature, %O" err

        wr.WriteLine '{'
        // TODO: Access property map for get, set, and other methods.
        wr.WriteLine '}'

    | Error err -> fprintfn wr "// error : Cannot find property %i, %s" i err.Message

let typeDefProperties
    trow
    (tables: ParsedMetadataTables)
    vfilter
    (strings: ParsedStringsStream)
    (blobs: ParsedBlobStream voption)
    (wr: #TextWriter)
    =
    match blobs, tables.Property, tables.PropertyMap with
    | ValueSome blobs', ValueSome ptable, ValueSome mtable ->
        // TODO: Store property map in a dictionary to avoid iterating through the map table for each type.
        let mutable i = 0u
        while i < mtable.RowCount do
            let row = mtable.[int32 i]
            if row.Parent = trow then
                propertyRow
                    ptable
                    mtable
                    tables
                    i
                    vfilter
                    strings
                    blobs'
                    wr
            i <- i + 1u
    | ValueSome _, _, _ -> wr.Write "// error : Cannot access property tables for some reason?"
    | ValueNone, _, _ -> wr.Write "// error : Cannot access blob stream to get property signatures"

let typeDefTable (tables: ParsedMetadataTables) vfilter (strings: ParsedStringsStream) blobs (wr: TextWriter) =
    match tables.TypeDef with
    | ValueSome table ->
        for i = 0 to int32 table.RowCount - 1 do
            let row = table.[i]
            if VisibilityFilter.typeDef vfilter row.Flags then
                wr.WriteLine()
                rowIndex wr i
                field "Flags" Print.bitfield wr row.Flags
                //field "TypeName"
                //field "TypeNamespace"
                //field "Extends"
                field "FieldList" Print.integer wr row.FieldList
                field "MethodList" Print.integer wr row.MethodList

                // TODO: Print this differently for nested classes.
                wr.Write ".class "

                if row.Flags.HasFlag TypeAttributes.Interface then wr.Write "interface "
                if row.Flags &&& TypeAttributes.VisibilityMask > TypeAttributes.Public then wr.Write "nested "

                match row.Flags &&& TypeAttributes.VisibilityMask with
                | TypeAttributes.NestedPublic 
                | TypeAttributes.Public -> "public "
                | TypeAttributes.NestedFamily -> "family "
                | TypeAttributes.NestedAssembly -> "assembly "
                | TypeAttributes.NestedFamANDAssem -> "famandassem "
                | TypeAttributes.NestedFamORAssem -> "famorassem "
                | TypeAttributes.NestedPrivate
                | TypeAttributes.NotPublic
                | _ -> "private "
                |> wr.Write

                match row.Flags &&& TypeAttributes.LayoutMask with
                | TypeAttributes.SequentialLayout -> "sequential "
                | TypeAttributes.ExplicitLayout -> "explicit "
                | TypeAttributes.AutoLayout
                | _ -> "auto "
                |> wr.Write

                match row.Flags &&& TypeAttributes.StringFormatMask with
                | TypeAttributes.AnsiClass -> wr.Write "ansi "
                | TypeAttributes.UnicodeClass -> wr.Write "unicode "
                | TypeAttributes.AutoClass -> wr.Write "autochar "
                | _ -> ()

                if row.Flags.HasFlag TypeAttributes.Abstract then wr.Write "abstract "
                if row.Flags.HasFlag TypeAttributes.Sealed then wr.Write "sealed "
                if row.Flags.HasFlag TypeAttributes.SpecialName then wr.Write "specialname "
                if row.Flags.HasFlag TypeAttributes.RTSpecialName then wr.Write "rtspecialname "
                if row.Flags.HasFlag TypeAttributes.Serializable then wr.Write "serializable "
                if row.Flags.HasFlag TypeAttributes.BeforeFieldInit then wr.Write "beforefieldinit "

                wr.Write '''
                let ns = strings.GetString row.TypeNamespace 
                if ns.Length > 0 then
                    wr.Write ns
                    wr.Write '.'
                wr.Write(strings.GetString row.TypeName)
                wr.WriteLine '''

                match ParsedExtends.toTypeDefOrRefOrSpec row.Extends with
                | ValueNone -> ()
                | ValueSome extends ->
                    wr.Write "    extends "
                    // TODO: Check that blob stream exists for typespec.
                    TypeName.ofTypeDefOrRefOrSpec extends tables strings (ValueOption.get blobs) wr
                    wr.WriteLine()

                wr.WriteLine '{'
                let wr' = indented wr
                typeDefFields tables i &row vfilter strings blobs wr'
                typeDefMethods tables i &row vfilter strings blobs wr'
                typeDefProperties (uint32 i) tables vfilter strings blobs wr'
                // TODO: Write other members.
                wr.WriteLine '}'
    | ValueNone -> ()

let metadataTables headers il vfilter strings guid blobs (tables: ParsedMetadataTables) offset wr =
    match headers with
    | NoHeaders -> ()
    | IncludeHeaders ->
        metadataTablesHeader tables.Header offset wr
    match il, strings, guid with
    | _, ValueNone, _ -> eprintfn "error : \"#Strings\" metadata heap is missing."
    | _, _, ValueNone -> eprintfn "error : \"#GUID\" metadata heap is missing."
    | NoIL, _, _ -> ()
    | IncludeIL, ValueSome strings', ValueSome guid' ->
        moduleTable tables strings' guid' wr
        assemblyTable tables strings' blobs wr
        assemblyRefTable tables strings' blobs wr
        typeRefTable tables strings' wr
        typeDefTable tables vfilter strings' blobs wr
    wr

let handleError state error offset wr =
    eprintfn "error : %s" (ReadError.message state error offset)
    wr

let write headers il vfilter =
    let inline header printer =
        match headers with
        | IncludeHeaders -> ValueSome printer
        | NoHeaders -> ValueNone
    { MetadataReader.empty with
       ReadCoffHeader = header coffHeader
       ReadStandardFields = header standardFields
       ReadNTSpecificFields = header ntSpecificFields
       ReadDataDirectories = header dataDirectories
       ReadSectionHeaders = header sectionHeaders
       ReadCliHeader = header cliHeader
       ReadMetadataRoot = header metadataRoot
       ReadStreamHeader = header streamHeader
       //ReadString
       ReadMetadataTables = ValueSome(metadataTables headers il vfilter)
       HandleError = handleError }

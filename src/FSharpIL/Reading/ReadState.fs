namespace FSharpIL.Reading

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type MetadataReadState =
    | FindCliHeader
    /// Indicates that the CLI header is being read (II.25.3.3).
    | ReadCliHeader
    | FindMetadataRoot
    | ReadMetadataSignature
    | ReadMetadataRoot
    | ReadStreamHeaders
    | ReadStringsStream
    | ReadGuidStream
    | ReadUserStringStream
    | ReadBlobStream
    | ReadMetadataTablesHeader
    | ReadMetadataTables
    | MoveToEnd

    member this.Description =
        match this with
        | FindCliHeader -> "searching for CLI header"
        | ReadCliHeader -> "reading CLI header"
        | FindMetadataRoot -> "searching for CLI metadata root"
        | ReadMetadataSignature -> "reading CLI metadata root signature"
        | ReadMetadataRoot -> "reading CLI metadata root"
        | ReadStreamHeaders -> "reading metadata stream headers"
        | ReadStringsStream -> "reading metadata strings stream"
        | ReadGuidStream -> "reading metadata GUID stream"
        | ReadUserStringStream -> "reading metadata user strings stream"
        | ReadBlobStream -> "reading metadata blob stream"
        | ReadMetadataTablesHeader -> "reading metadata tables header"
        | ReadMetadataTables -> "reading metadata tables"
        | MoveToEnd -> "moving to end of metadata"

[<IsReadOnly; Struct>]
type FileReadState =
    | ReadPEMagic
    | MoveToLfanew
    /// <summary>The <c>lfanew</c> field pointing to the PE signature is being read (II.25.2.1).</summary>
    | ReadLfanew
    | MoveToPESignature
    | ReadPESignature
    /// The PE file header after the PE signature is being read (II.25.2.2).
    | ReadCoffHeader
    | ReadOptionalHeader
    | ReadDataDirectories
    | ReadSectionHeaders
    | ReadSectionData

    member this.Description =
        match this with
        | ReadPEMagic -> "reading Portable Executable magic"
        | MoveToLfanew -> "moving to lfanew field"
        | ReadLfanew -> "reading lfanew field"
        | MoveToPESignature -> "moving to PE signature"
        | ReadPESignature -> "reading PE signature"
        | ReadCoffHeader -> "reading COFF header"
        | ReadOptionalHeader -> "reading PE optional header"
        | ReadDataDirectories -> "reading PE header data directories"
        | ReadSectionHeaders -> "reading section headers"
        | ReadSectionData -> "reading section data"

[<RequireQualifiedAccess>]
type ReadState =
    | File of FileReadState
    | Metadata of MetadataReadState

    member this.Description =
        match this with
        | File state -> state.Description
        | Metadata state -> state.Description

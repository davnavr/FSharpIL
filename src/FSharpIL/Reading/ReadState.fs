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

    member this.Description =
        match this with
        | FindCliHeader -> "searching for CLI header"
        | ReadCliHeader -> "reading CLI header"
        | FindMetadataRoot -> "searching for CLI metadata root"
        | ReadMetadataSignature -> "reading CLI metadata root signature"
        | ReadMetadataRoot -> "reading CLI metadata root"
        | ReadStreamHeaders -> "reading metadata stream headers"

[<IsReadOnly; Struct>]
type FileReadState =
    | ReadPEMagic
    | MoveToLfanew
    /// <summary>Indicates that the <c>lfanew</c> field pointing to the PE signature is being read (II.25.2.1).</summary>
    | ReadLfanew
    | MoveToPESignature
    | ReadPESignature
    /// Indicates that the PE file header after the PE signature is being read (II.25.2.2).
    | ReadCoffHeader
    | ReadStandardFields
    | ReadNTSpecificFields
    | ReadDataDirectories
    | ReadSectionHeaders
    | MoveToTextSectionData
    | ReadTextSectionData

    member this.Description =
        match this with
        | ReadPEMagic -> "reading Portable Executable magic"
        | MoveToLfanew -> "moving to lfanew field"
        | ReadLfanew -> "reading lfanew field"
        | MoveToPESignature -> "moving to PE signature"
        | ReadPESignature -> "reading PE signature"
        | ReadCoffHeader -> "reading COFF header"
        | ReadStandardFields -> "reading PE header standard fields of the PE optional header"
        | ReadNTSpecificFields -> "reading NT-specific fields of the PE optional header"
        | ReadDataDirectories -> "reading PE header data directories"
        | ReadSectionHeaders -> "reading section headers"
        | MoveToTextSectionData -> "moving to text section data"
        | ReadTextSectionData -> "reading text section data"

[<RequireQualifiedAccess>]
type ReadState =
    | File of FileReadState
    | Metadata of MetadataReadState

    member this.Description =
        match this with
        | File state -> state.Description
        | Metadata state -> state.Description

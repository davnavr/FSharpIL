namespace FSharpIL.Reading

open System.Runtime.CompilerServices

type IReadState = interface end

[<IsReadOnly; Struct>]
type MetadataReadState =
    /// Indicates that the CLI header is being read (II.25.3.3).
    | ReadCliHeader
    | FindMetadataRoot
    | ReadMetadataRoot
    | ReadStreamHeaders
    | ReadStringsStream
    | ReadGuidStream

    override this.ToString() =
        match this with
        | ReadCliHeader -> "reading CLI header"
        | FindMetadataRoot -> "locating metadata root"
        | ReadMetadataRoot -> "reading metadata root"
        | ReadStreamHeaders -> "reading metadata stream headers"
        | ReadStringsStream -> "reading metadata strings stream"
        | ReadGuidStream -> "reading metadata GUID stream"

    interface IReadState

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

    override this.ToString() =
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

    interface IReadState

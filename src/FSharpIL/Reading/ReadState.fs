namespace FSharpIL.Reading

[<Struct>]
type ReadState =
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
    | MoveToCliHeader
    /// Indicates that the CLI header is being read (II.25.3.3).
    | ReadCliHeader

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
        | MoveToCliHeader -> "moving to CLI header"
        | ReadCliHeader -> "reading CLI header"

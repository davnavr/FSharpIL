namespace FSharpIL.Reading

open System

[<Struct>]
type ReadState =
    | ReadPEMagic
    | MoveToLfanew
    /// Indicates that the <c>lfanew</c> field pointing to the PE signature is being read (II.25.2.1).
    | ReadLfanew
    | MoveToPESignature
    | ReadPESignature
    /// Indicates that the PE file header after the PE signature is being read (II.25.2.2).
    | ReadCoffHeader
    | ReadStandardFields
    | ReadNTSpecificFields
    | ReadDataDirectories
    | ReadSectionHeaders
    | MoveToCliHeader
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
        | MoveToCliHeader -> "moving to CLI header"
        | ReadCliHeader -> "reading CLI header"

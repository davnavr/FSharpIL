/// Contains functions for reading CLI metadata (II.24).
[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open FSharpIL
open FSharpIL.PortableExecutable

/// <summary>Reads the CLI metadata contained within the specified <paramref name="section"/>.</summary>
/// <param name="section">A managed poitner to the contents of the section.</param>
/// <param name="sectionRva">
/// A relative virtual address pointing to the first byte of the section data, corresponds to the <c>VirtualAddress</c> field of
/// the section header.
/// </param>
/// <param name="sectionOffset">
/// Offset from the start of the Portable Executable file to the first byte of the section data, corresponds to the
/// <c>PointerToRawData</c> field of the section header.
/// </param>
/// <param name="cliHeaderOffset">Offset from the start of the section to the first byte of the CLI header.</param>
/// <param name="state">The initial state.</param>
/// <param name="reader">The reader used to process the CLI metadata.</param>
val fromChunkedMemory<'State> :
    section: inref<ChunkedMemory> ->
    sectionRva: Rva ->
    sectionOffset: FileOffset ->
    cliHeaderOffset: SectionOffset ->
    state: 'State ->
    reader: MetadataReader<'State> ->
    'State

[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open FSharpIL

/// <summary>Writes the CLI metadata to the specified section.</summary>
/// <param name="section">A builder positioned at the start of the CLI metadata.</param>
/// <param name="cliHeaderRva">A relative virtual address pointing to the start of the CLI metadata.</param>
/// <param name="builder">The CLI metadata to write to the section.</param>
val metadata : section: byref<ChunkedMemoryBuilder> -> cliHeaderRva: Rva -> builder: CliMetadataBuilder -> unit

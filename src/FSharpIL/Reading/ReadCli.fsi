/// Contains functions for reading CLI metadata (II.24).
[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadCli

open FSharpIL.PortableExecutable

val fromChunkedMemory<'State> :
    section: inref<FSharpIL.ChunkedMemory> ->
    cliHeaderOffset: SectionOffset ->
    state: 'State ->
    MetadataReader<'State> ->
    'State

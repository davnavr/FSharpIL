/// Contains functions for reading Portable Executable files (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadPE

val fromArray<'State> : file: byte[] -> state: 'State -> PEFileReader<'State> -> 'State

val fromBlock<'State> :
    file: System.Collections.Immutable.ImmutableArray<byte> ->
    state: 'State ->
    PEFileReader<'State> ->
    'State

val fromChunkedMemory<'State> : file: FSharpIL.ChunkedMemory -> state: 'State -> PEFileReader<'State> -> 'State

val fromMemory<'State> : file: System.ReadOnlyMemory<byte> -> state: 'State -> PEFileReader<'State> -> 'State

/// Reads a Portable Executable file from the specified stream.
val fromStream<'Stream, 'State when 'Stream :> System.IO.Stream> :
    stream: 'Stream ->
    state: 'State ->
    PEFileReader<'State> ->
    'State

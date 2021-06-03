/// Contains functions for reading Portable Executable files (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadPE

open System
open System.Collections.Immutable
open System.IO

open FSharpIL

/// Reads a Portable Executable file from the specified stream.
val fromStream<'State> : stream: Stream -> state: 'State -> MetadataReader<'State> -> 'State

val fromMemory<'State> : file: ReadOnlyMemory<byte> -> state: 'State -> MetadataReader<'State> -> 'State

val fromArray<'State> : file: byte[] -> state: 'State -> MetadataReader<'State> -> 'State

val fromBlock<'State> : file: ImmutableArray<byte> -> state: 'State -> MetadataReader<'State> -> 'State

val fromChunkedMemory<'State> : file: ChunkedMemory -> state: 'State -> MetadataReader<'State> -> 'State

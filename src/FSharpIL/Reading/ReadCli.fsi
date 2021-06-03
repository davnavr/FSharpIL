module FSharpIL.Reading.ReadCli

open System.IO

val fromStream<'State> : stream: Stream -> state: 'State -> MetadataReader<'State> -> 'State

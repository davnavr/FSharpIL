module FSharpIL.ReadCli

open System.IO

val fromStream<'State> : stream: Stream -> state: 'State -> FSharpIL.Reading.MetadataReader<'State> -> 'State

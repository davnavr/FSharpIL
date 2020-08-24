/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): _ -> ReadResult =
    fun() -> PEReader(name).Invoke(stream)

let public fromPath (path: string) =
    File.OpenRead path |> fromStream path

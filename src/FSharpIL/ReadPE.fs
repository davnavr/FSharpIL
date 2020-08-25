/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

/// Reads a <see cref="T:FSharpIL.Types.PortableExecutable"/> from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: _ -> #Stream): _ ->ReadResult =
    fun() -> stream() |> PEReader(name).Read

let public fromPath (path: string) =
    fromStream path (fun() -> File.OpenRead path)

/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

open FSharpIL.Utilities

open FSharpIL.Parser

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): IO<_> =
    io {
        use source = new ByteStream(name, stream)
        invalidOp "bad"
    }

let public fromPath (path: string): IO<_> =
    File.OpenRead path |> fromStream path

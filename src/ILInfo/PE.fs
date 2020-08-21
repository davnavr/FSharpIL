[<RequireQualifiedAccess>]
module ILInfo.PE

open System.IO

open ILInfo.Utilities

open ILInfo.Parser

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): IO<_> =
    io {
        use source = new ByteStream(name, stream)
        invalidOp "bad"
    }

let public fromPath (path: string): IO<_> =
    File.OpenRead path |> fromStream path

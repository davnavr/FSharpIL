[<RequireQualifiedAccess>]
module ILInfo.PE

open System.IO

open ILInfo.Utilities

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (stream: Stream): IO<unit> =
    io {
        use reader = new StreamReader(stream)
        invalidOp "bad"
    }

/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

open FSharpIL.Utilities

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): _ -> ReadResult =
    proc {
        use source = new ByteStream(name, stream)
        let! header =
            match (source.ReadByte(), source.ReadByte()) with
            | (Some 0x4Duy, Some 0x5Auy) ->
                // TODO Read lfanew & rest of dos stub
                invalidOp "bad"
            | (Some b1, Some b2) ->
                IncorrectDOSMagic(b1, b2) |> Process.fail
            | _ -> Process.fail FileTooSmall
        return { DosHeader = header }
    }

let public fromPath (path: string) =
    File.OpenRead path |> fromStream path

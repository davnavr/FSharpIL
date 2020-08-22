/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

open FSharpIL.Utilities

open FSharpIL.Reader

[<AutoOpen>]
module private Readers =
    let dosHeader =
        let magic bytes = array bytes (fun _ -> InvalidDOSHeader)
        magic Magic.dosHeader
        >>. count 4 (fun _ -> InvalidDOSHeader)
        .>> magic Magic.dosStub
        >>= function
        | [| b1; b2; b3; b4 |] ->
            let stub = DosStub(b1, b2, b3, b4)
            if uint stub >= 0x80u then
                retn stub
            else
                InvalidPESignatureOffset(b1, b2, b3, b4) |> fail
        | _ -> invalidOp "Invalid number of bytes for lfanew"
    let file =
        dosHeader
        |>> fun header ->
            { DosHeader = header }

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): IO<ReadResult> =
    io {
        use source = new ByteStream(name, stream)
        return file source
    }

let public fromPath (path: string): IO<_> =
    File.OpenRead path |> fromStream path

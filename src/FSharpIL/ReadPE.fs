/// Providings functions for reading PE files containing CIL code.
[<RequireQualifiedAccess>]
module FSharpIL.ReadPE

open System.IO

open FSharpIL.Utilities

[<AutoOpen>]
module private Readers =
    let dosHeader (source: ByteStream) result =
        let lfanew pe =
            match (source.TryMove 0x3Cu, source.ReadUInt32()) with
            | (Ok(), Ok lfanew) ->
                result := Ok { pe with DosHeader = DosStub lfanew }
            | (_, _) ->
                result := Error MissingPESignatureOffset

        match (!result, source.ReadByte(), source.ReadByte()) with
        | (Ok pe, Some 0x4Duy, Some 0x5Auy) ->
            lfanew pe
        | (_, Some b1, Some b2) ->
            result := IncorrectDOSMagic(b1, b2) |> Error
        | _ -> result := Error FileTooSmall

/// Reads a [PortableExecutable] from a <see cref="T:System.IO.Stream"/>.
let public fromStream (name: string) (stream: Stream): _ -> ReadResult =
    fun() ->
        use source = new ByteStream(name, stream)
        let result =
            { DosHeader = Unchecked.defaultof<DosStub> }
            |> Result.Ok
            |> ref
        dosHeader source result
        // TODO: Move to the offset
        !result

let public fromPath (path: string) =
    File.OpenRead path |> fromStream path

[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable

type Builder internal() =
    member _.Combine(headers: PEHeaders, pe: PEFile) =
        { pe with Headers = headers }
    member _.Delay(f): PEFile = f()
    member _.Yield(headers: PEHeaders) = headers
    member _.Zero(): PEFile = PEFile.Default

let internal writer (pe: PEFile) body output = // TODO: Use a fancy "state machine" when writing?
    body (fun source ->
        let write = output source
        write [| 0x4Duy |] // TODO: Write DOS header.
        ())

let toStream (stream: Stream) pe =
    writer
        pe
        (new BinaryWriter(stream) |> using)
        (fun source -> source.Write)

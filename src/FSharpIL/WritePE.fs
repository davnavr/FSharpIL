[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PETypes

type Builder internal() =
    member _.Combine(header: PEFileHeader, pe: PortableExecutable) =
        { pe with PEFileHeader = header }
    member _.Delay(f): PortableExecutable = f()
    member _.Yield(header: PEFileHeader) = header
    member _.Zero(): PortableExecutable = PortableExecutable.Empty

let internal writer (pe: PortableExecutable) body output =
    body (fun source ->
        let write = output source
        write [| 0x4Duy; 0x5Auy |]
        ())

let toStream (stream: Stream) pe =
    writer
        pe
        (new BinaryWriter(stream) |> using)
        (fun source -> source.Write)

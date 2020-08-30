﻿[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable

type Builder internal() =
    member _.Combine(header: CoffHeader, pe: PEFile) =
        { pe with FileHeader = header }
    member _.Delay(f): PEFile = f()
    member _.Yield(header: CoffHeader) = header
    member _.Zero(): PEFile = PEFile.Default

let internal writer (pe: PEFile) body output =
    body (fun source ->
        let write = output source
        write [| 0x4Duy |] // TODO: Write DOS header.
        ())

let toStream (stream: Stream) pe =
    writer
        pe
        (new BinaryWriter(stream) |> using)
        (fun source -> source.Write)

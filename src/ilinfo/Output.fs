[<RequireQualifiedAccess>]
module ILInfo.Output

open Microsoft.FSharp.Core.Printf

open FSharpIL.PortableExecutable
open FSharpIL.Reading

let private filter cond reader = if cond then ValueSome reader else ValueNone

let coffHeader (header: CoffHeader<_, _>) () =
    Field.printField "Machine" header.Machine
    ()

let console coff =
    { MetadataReader.empty with
        ReadCoffHeader = filter coff coffHeader
        HandleError =
            fun offset state error () ->
                eprintfn "Error occured at offset (0x%X) while %s: %s" offset state.Description error.Message }

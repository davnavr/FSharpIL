[<AutoOpen>]
module FSharpIL.WritePE

type PEBuilder internal () =
    member _.Yield(header: DosStub) = ()

/// Builds a PE file containing CIL code using computation expression syntax.
let pe = PEBuilder()

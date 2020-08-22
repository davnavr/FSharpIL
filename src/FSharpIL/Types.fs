[<AutoOpen>]
module FSharpIL.Types

// II.25.2.3.1
type DosStub =
    internal
    | DosStub of lfanew: byte * byte * byte * byte

type PortableExecutable =
    { DosHeader: DosStub }

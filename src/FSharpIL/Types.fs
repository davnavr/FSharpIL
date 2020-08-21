[<AutoOpen>]
module FSharpIL.Types

// II.25.2.3.1
type DosHeader =
    | DosHeader of lfanew: byte * byte * byte * byte

type PortableExecutable =
    { DosHeader: DosHeader }

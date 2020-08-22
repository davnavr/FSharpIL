[<AutoOpen>]
module FSharpIL.Types

// II.25.2.3.1
type DosStub =
    internal
    | DosStub of lfanew: byte * byte * byte * byte

    // The value here is never less than 0x80
    static member op_Explicit(DosStub (b1, b2, b3, b4)) =
        uint b1 + (uint b2 <<< 8) + (uint b3 <<< 16) + (uint b4 <<< 24)

type PortableExecutable =
    { DosHeader: DosStub }

type ReadError =
    | InvalidDOSHeader
    | InvalidPESignatureOffset of byte * byte * byte * byte

    override this.ToString() =
        match this with
        | InvalidDOSHeader -> "The DOS header is invalid"
        | InvalidPESignatureOffset _ ->
            "The file offset to the PE signature is invalid"

type ReadResult = Result<PortableExecutable, ReadError>

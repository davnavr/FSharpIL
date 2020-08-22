[<AutoOpen>]
module FSharpIL.Types

// II.25.2.3.1
type DosStub =
    internal
    | DosStub of lfanew: byte * byte * byte * byte

    override this.ToString() =
        let (DosStub (b1, b2, b3, b4)) = this
        sprintf "0x%02x 0x%02x 0x%02x 0x%02x" b1 b2 b3 b4

    // The value here is never less than 0x80
    static member op_Explicit(DosStub (b1, b2, b3, b4)) =
        uint b1 + (uint b2 <<< 8) + (uint b3 <<< 16) + (uint b4 <<< 24)

type PortableExecutable =
    { DosHeader: DosStub }

type ReadError =
    | InvalidDOSHeader
    | InvalidPESignatureOffset of DosStub

    override this.ToString() =
        match this with
        | InvalidDOSHeader -> "The DOS header is invalid"
        | InvalidPESignatureOffset(stub) ->
            sprintf "The file offset to the PE signature (%O) is invalid" stub

type ReadResult = Result<PortableExecutable, ReadError>

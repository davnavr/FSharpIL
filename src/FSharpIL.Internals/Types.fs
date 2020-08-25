[<AutoOpen>]
module FSharpIL.Types

// II.25.2.3.1
type DosStub =
    internal
    | DosStub of lfanew: uint32

    member this.PESignatureOffset =
        let (DosStub lfanew) = this
        lfanew

    override this.ToString() =
        sprintf "0x%02x" this.PESignatureOffset

type PortableExecutable =
    { DosHeader: DosStub }

    member internal this.SetDosHeader(header) =
        { this with DosHeader = header }

type ReadError =
    | NonPEFile
    | IncorrectDOSMagic of byte * byte
    | InvalidPESignatureOffset of DosStub option
    | InvalidPESignature of (byte * byte * byte * byte) option
    | MissingField of name: string * size: uint
    | IncorrectIntField of name: string * uint64

    override this.ToString() =
        match this with
        | NonPEFile -> "The is not a valid PE file"
        | IncorrectDOSMagic(b1, b2) ->
            sprintf "The magic number of the file is incorrect and should be 0x4d 0x5a instead of 0x%02x 0x%02x" b1 b2
        | InvalidPESignatureOffset None ->
            "The offset to the PE signature in the DOS header (lfanew) is missing"
        | InvalidPESignatureOffset(Some stub) ->
            sprintf "The offset to the PE signature is an invalid value: 0x%02x" stub.PESignatureOffset
        | InvalidPESignature None ->
            "The PE signature marking the start of the PE file header is missing"
        | InvalidPESignature(Some(b1, b2, b3, b4)) ->
            sprintf "The PE signature is invalid and should be 0x50 0x45 0x00 0x00 instead of 0x%02x 0x%02x 0x%02x 0x%02x" b1 b2 b3 b4
        | MissingField(name, size) ->
            sprintf "The field \"%s\" of size %i is missing" name size
        | IncorrectIntField(name, value) ->
            sprintf "The value of the field \"%s\" is invalid: 0x%02x" name value

type ReadResult = Result<PortableExecutable, ReadError>

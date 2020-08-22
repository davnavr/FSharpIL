namespace FSharpIL

type ReadResult =
    | ValidFile of PortableExecutable
    | InvalidDOSHeader of byte * at: byte
    | InvalidPESignatureOffset of byte * byte * byte * byte

    override this.ToString() =
        match this with
        | ValidFile file -> string file
        | InvalidDOSHeader(value, at) ->
            sprintf "The DOS header contains an invalid byte 0x%02X at offset 0x%02X" value at
        | InvalidPESignatureOffset _ ->
            "The file offset to the PE signature is invalid"

[<RequireQualifiedAccess>]
module ReadResult =
    let get result =
        match result with
        | ValidFile file -> file
        | err -> string err |> invalidArg "result"

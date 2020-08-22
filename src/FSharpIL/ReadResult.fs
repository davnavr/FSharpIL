namespace FSharpIL

type ReadResult =
    | ValidFile of PortableExecutable
    | InvalidPESignatureOffset of byte * byte * byte * byte

    override this.ToString() =
        match this with
        | ValidFile file -> string file
        | InvalidPESignatureOffset _ ->
            "The file offset to the PE signature is invalid"

[<RequireQualifiedAccess>]
module ReadResult =
    let get result =
        match result with
        | ValidFile file -> file
        | err -> string err |> invalidArg "result"

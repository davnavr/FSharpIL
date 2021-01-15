namespace FSharpIL.Metadata

open System.Text
open System.Runtime.CompilerServices

open FSharpIL

[<Struct; IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type MetadataVersion =
    internal
    | MetadataVersion of byte * byte[]

    override this.ToString() =
        let (MetadataVersion (len, bytes)) = this
        Array.take (int len) bytes |> Encoding.UTF8.GetString

    /// The length of the version string including the null terminator, rounded up to a multiple of 4.
    member this.Length = let (MetadataVersion (_, bytes)) = this in uint32 bytes.Length

[<RequireQualifiedAccess>]
module MetadataVersion =
    let tryOfStr (str: string) =
        let str' = Encoding.UTF8.GetBytes str
        let len = Round.upTo 4 (str'.Length + 1)
        if len < 256 then
            let bytes =
                Array.init
                    len
                    (fun i ->
                        if i < str'.Length
                        then Array.get str' i
                        else 0uy)
            MetadataVersion(byte str'.Length, bytes) |> Some
        else None

    let ofStr (str: string) =
        match tryOfStr str with
        | Some ver -> ver
        | None -> invalidArg "str" "Version strings are limited to a length of 255 bytes."

    let toArray (MetadataVersion (_, bytes)) = bytes

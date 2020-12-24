namespace FSharpIL.Metadata

open System
open System.Text

[<StructuralComparison; StructuralEquality>]
type MetadataVersion =
    internal
    | MetadataVersion of string

    override this.ToString() =
        let (MetadataVersion version) = this in version

    // NOTE: Should always be <= 255, and the length should include the null terminator.
    member this.Length =
        let length = uint32 (this.ToString().Length) + 1u
        (length - 1u) - ((length - 1u) % 4u) + 4u

[<RequireQualifiedAccess>]
module MetadataVersion =
    let ofString (str: string) =
        if str.Length < 255
        then MetadataVersion str |> Some
        else None

    let toArray (MetadataVersion str as version) =
        let bytes = int version.Length |> Array.zeroCreate<byte>
        let str' = Encoding.UTF8.GetBytes str
        Array.Copy(str', bytes, str'.Length)
        bytes

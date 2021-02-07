namespace FSharpIL.Metadata.Heaps

open System.Collections.Generic
open System.Text

/// <summary>Represents the <c>#US</c> heap, which stores string literals (II.24.2.4).</summary>
type internal UserStringHeap internal (capacity: int32) =
    let strings = List<string> capacity
    let lookup = Dictionary<string, BlobIndex> capacity
    let mutable i = 1u

    member _.StringCount = strings.Count

    member _.Contains str = lookup.ContainsKey str

    member _.ByteLength = i

    member _.Add str =
        match str with
        | null
        | "" -> ()
        | _ when lookup.ContainsKey str -> ()
        | _ ->
            let length = Encoding.Unicode.GetByteCount str |> uint32
            let index = { BlobIndex.Index = i; BlobIndex.Size = length + 1u }
            i <- i + index.TotalSize
            lookup.Item <- str, index
            strings.Add str

    member _.IndexOf str =
        match str with
        | null -> nullArg (nameof str)
        | "" -> Unchecked.defaultof<BlobIndex>
        | _ -> lookup.Item str

    member _.GetEnumerator() = strings.GetEnumerator()

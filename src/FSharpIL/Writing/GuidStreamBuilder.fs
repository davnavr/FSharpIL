namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

open FSharpIL.Metadata

/// <summary>Builds the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
[<Sealed>]
type GuidStreamBuilder (capacity: int32) =
    static let empty = Guid.Empty
    let guids = RefArrayList<Guid> capacity
    let lookup = Dictionary<Guid, GuidIndex> capacity
    do guids.Add &empty |> ignore // First GUID is at index 1, so 0 is treated as null.
    do lookup.[Guid.Empty] <- GuidIndex.Zero
    member _.IsEmpty = guids.Count = 1
    /// Gets the number of GUIDs stored in this stream.
    member _.Count = guids.Count - 1
    /// <summary>The length of the <c>#GUID</c> metadata stream, in bytes.</summary>
    member this.StreamLength = uint32 this.Count * 16u

    member _.Add guid =
        match lookup.TryGetValue guid with
        | true, existing -> existing
        | false, _ ->
            let i = { GuidIndex = uint32 guids.Count }
            lookup.[guid] <- i
            guids.Add &guid |> ignore
            i

    /// Generates a new GUID and adds it to the stream.
    member inline this.AddNew() = this.Add(Guid.NewGuid())

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.guid
        member this.Serialize(builder, _) =
            let mutable buffer = Span.stackalloc<byte> sizeof<Guid>
            for i = 1 to this.Count do
                if not (guids.[i].TryWriteBytes buffer) then failwithf "Could not write GUID at index %i" i
                builder.Write buffer

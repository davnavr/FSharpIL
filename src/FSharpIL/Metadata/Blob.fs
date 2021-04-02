namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type internal BlobIndex =
    { /// <summary>An index that points to this blob in the <c>#Blob</c> heap.</summary>
      Index: uint32
      /// The size of the contents of the blob pointed to by this index.
      Size: uint32 }

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type Blob<'Item> internal (index: int32) =
    member internal _.Index = index
    member internal _.ChangeTag<'To>() = Blob<'To> index
    override _.ToString() = sprintf "%s(%i)" typeof<'Item>.Name index

[<RequireQualifiedAccess>]
module internal Blob =
    let tryAddTo (lookup: Dictionary<'Item, int32>) item =
        let i = lookup.Count
        match lookup.TryGetValue item with
        | true, existing -> Error(Blob<'Item> existing)
        | false, _ ->
            lookup.[item] <- i
            Ok(Blob<'Item> i)

[<IsReadOnly; Struct>]
type BlobLookup<'Item> internal (blobs: 'Item[]) =
    member _.Count = blobs.Length
    member _.Item with get (i: Blob<'Item>) = blobs.[i.Index]
    member _.ItemRef(i: Blob<'Item>): inref<'Item> = &blobs.[i.Index]

[<Sealed>]
type BlobLookupBuilder<'Item when 'Item : equality> internal () =
    let lookup = Dictionary<'Item, int32>()

    member _.Count = lookup.Count

    member _.TryAdd(item: 'Item) = Blob.tryAddTo lookup item
    member this.GetOrAdd item = this.TryAdd item |> Result.any

    member internal _.ToImmutable() =
        let blobs = Array.zeroCreate lookup.Count
        for KeyValue(item, i) in lookup do
            blobs.[i] <- item
        BlobLookup<'Item> blobs

namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// <summary>Represents a <c>MethodSpec</c> item in the <c>#Blob</c> heap (II.23.2.15).</summary>
[<IsReadOnly>]
type MethodSpec = struct
    val GenericArguments: ImmutableArray<EncodedType>

    /// <exception cref="T:System.ArgumentException">Thrown when the generic argument list is empty.</exception>
    new (args: ImmutableArray<_>) =
        if args.Length <= 0 then invalidArg "garguments" "The generic argument list cannot be empty."
        { GenericArguments = args }

    new (args: seq<_>) = MethodSpec(args.ToImmutableArray())

    member this.Count = this.GenericArguments.Length
end

// TODO: Reduce code duplication with TypeSpecBlobLookup.
[<IsReadOnly; Struct>]
type MethodSpecBlobLookup internal (blobs: MethodSpec[]) =
    member _.Count = blobs.Length
    member _.Item with get (i: MethodSpecBlob) = blobs.[i.Index]
    member _.ItemRef(i: MethodSpecBlob): inref<MethodSpec> = &blobs.[i.Index]

[<Sealed>]
type MethodSpecBlobLookupBuilder internal () =
    let blobs = Dictionary<MethodSpec, int32>()
    member _.Count = blobs.Count
    member _.TryAdd(spec: inref<MethodSpec>) =
        let count = blobs.Count
        match blobs.TryGetValue spec with
        | true, existing -> Error(MethodSpecBlob existing)
        | false, _ ->
            blobs.[spec] <- count
            Ok(MethodSpecBlob count)
    member internal _.ToImmutable() =
        let blobs' = Array.zeroCreate blobs.Count
        for KeyValue(item, i) in blobs do
            blobs'.[i] <- item
        MethodSpecBlobLookup blobs'

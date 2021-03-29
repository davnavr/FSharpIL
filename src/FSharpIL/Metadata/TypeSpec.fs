namespace FSharpIL.Metadata

open System.Collections.Generic

/// <summary>Represents a <c>TypeSpec</c> item in the <c>#Blob</c> heap (II.23.2.14).</summary>
[<RequireQualifiedAccess>]
type TypeSpec =
    // NOTE: According to ECMA-336 augmentations, TypeSpec blob format extends the Type blob format.
    // TODO: Figure out how to just use existing EncodedType union here, instead of copying and pasting.
    /// <summary>Represents a <c>GENERICINST</c> followed by a <c>TypeRef</c>.</summary>
    | GenericInst of GenericInst
    | MVar of number: uint32
    | Var of number: uint32

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeSpecBlobLookup internal (blobs: TypeSpec[]) =
    member _.Count = blobs.Length
    member _.Item with get (i: TypeSpecBlob) = blobs.[i.Index]
    member _.ItemRef(i: TypeSpecBlob): inref<TypeSpec> = &blobs.[i.Index]

[<Sealed>]
type TypeSpecBlobLookupBuilder internal () =
    let blobs = Dictionary<TypeSpec, int32>()
    member _.Count = blobs.Count
    member _.Add(spec, duplicate: outref<bool>) =
        let count = blobs.Count
        let i =
            let (duplicate', existing) = blobs.TryGetValue spec
            duplicate <- duplicate'
            if duplicate'
            then existing
            else
                blobs.[spec] <- count
                count
        TypeSpecBlob i
    member internal _.ToImmutable() =
        let blobs' = Array.zeroCreate blobs.Count
        for KeyValue(item, i) in blobs do
            blobs'.[i] <- item
        TypeSpecBlobLookup blobs'

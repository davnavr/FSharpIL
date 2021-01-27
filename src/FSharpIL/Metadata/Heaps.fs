namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata

open FSharpIL.Writing

[<Sealed>]
type private HeapCollection<'Key when 'Key : equality> internal (capacity: int32, offset: uint32) =
    [<Literal>]
    let MaxSmallIndex = 0xFFFFu
    let items = Array.zeroCreate<'Key> capacity
    let lookup = Dictionary<'Key, uint32> capacity
    let mutable i = 0

    new(capacity) = HeapCollection(capacity, 0u)

    member _.Add item =
        if lookup.ContainsKey item
        then false
        else
            items.[i] <- item
            i <- i + 1
            lookup.Item <- item, (uint32 i) + offset
            true

    member _.Count = lookup.Count

    member this.Offset = uint32 this.Count + offset

    member _.IndexOf item = lookup.Item item

    member this.IndexSize = if this.Offset > MaxSmallIndex then 4 else 2

    // TODO: Make an HeapCollectonEnumerator struct?
    member _.GetEnumerator() = ArraySegment(items, 0, lookup.Count).GetEnumerator() :> IEnumerator<_>

    member this.WriteRawIndex(i, writer: ChunkWriter) =
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    interface IReadOnlyCollection<'Key> with
        member this.Count = lookup.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

// TODO: Determine if adding strings first and then allowing retrieval of index is faster than assigning an index to each string as it is written.
/// <summary>Represents the <c>#Strings</c> metadata stream (II.24.2.3).</summary>
[<Sealed>]
type internal StringsHeap internal (metadata: CliMetadata) =
    let strings =
        // Estimated number of strings, actual count may be less.
        let capacity =
            1 // Module
            + (2 * metadata.TypeRef.Count)
            + (2 * metadata.TypeDef.Count)
            + metadata.Field.Count
            + metadata.MethodDef.Count
            + metadata.Param.Length

            + metadata.MemberRef.Count

            + if metadata.Assembly.IsSome then 2 else 0
            + (2 * metadata.AssemblyRef.Count)
        HeapCollection<string> capacity
    let mutable size = 1

    do
        let add =
            function
            | null
            | "" -> ()
            | str ->
                if strings.Add str then
                    size <- size + 1 + (Encoding.UTF8.GetByteCount str)

        string metadata.Module.Name |> add

        for tref in metadata.TypeRef.Items do
            string tref.TypeName |> add
            string tref.TypeNamespace |> add

        for tdef in metadata.TypeDef.Items do
            string tdef.TypeName |> add
            string tdef.TypeNamespace |> add

        for field in metadata.Field.Items do
            string field.Name |> add

        for method in metadata.MethodDef.Items do
            string method.Name |> add

        for _, param in metadata.Param do
            add param.ParamName


        for mref in metadata.MemberRef.Items do
            string mref.MemberName |> add



        match metadata.Assembly with
        | Some assembly ->
            string assembly.Name |> add
            string assembly.Culture |> add
        | None -> ()

        for assembly in metadata.AssemblyRef.Items do
            string assembly.Name |> add
            string assembly.Culture |> add

        ()

    member val Count = strings.Count

    member _.ByteLength = size

    member _.IndexOf str =
        match str with
        | null
        | "" -> 0u
        | _ -> strings.IndexOf str

    member val IndexSize = strings.IndexSize

    member this.WriteIndex(str, writer) = strings.WriteRawIndex(this.IndexOf str, writer)
    member this.WriteIndex(o, writer: ChunkWriter) = this.WriteIndex(o.ToString(), writer)

    member _.WriteHeap(content: ChunkList) =
        let writer = ChunkWriter.After(content.Tail.Value, size)
        writer.WriteU1 0uy
        for str in strings do
            Encoding.UTF8.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy

/// <summary>Represents the <c>#US</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type internal UserStringHeap internal (metadata: CliMetadata) =
    let strings =
        1
        |> Dictionary<string, uint32>

    // NOTE: When writing this heap, see II.24.2.4 to see how lengths of the bytes are encoded.

/// <summary>Represents the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
[<Sealed>]
type internal GuidHeap internal (metadata: CliMetadata) =
    let guids =
        let capacity = 1 // Module table is always present.
        HeapCollection<Guid> capacity

    do guids.Add metadata.Module.Mvid |> ignore

    member val Count = guids.Count

    member val ByteLength = 16 * guids.Count

    member _.IndexOf guid =
        if Guid.Empty = guid
        then 0u
        else guids.IndexOf guid

    member val IndexSize = guids.IndexSize

    member this.WriteIndex(guid, writer: ChunkWriter) = guids.WriteRawIndex(this.IndexOf guid, writer)
    member this.WriteZero writer = this.WriteIndex(Guid.Empty, writer)

    member this.WriteHeap (content: ChunkList) =
        let writer = ChunkWriter.After(content.Tail.Value, this.ByteLength)
        for guid in guids do
            guid.ToByteArray() |> writer.WriteBytes

[<AutoOpen>]
module private WriterExtensions =
    [<Literal>]
    let MaxCompressedUnsigned = 0x1FFF_FFFFu

    type ChunkWriter with
        /// <summary>Writes an unsigned compressed integer (II.23.2).</summary>
        /// <exception cref="System.ArgumentException">The <paramref name="value"/> is greater than the maximum compressed unsigned integer.</exception>
        member this.WriteCompressed(value: uint32) =
            if value > MaxCompressedUnsigned then
                sprintf
                    "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x"
                    value
                    MaxCompressedUnsigned
                |> invalidArg (nameof value)
            elif value > 0x3FFFu then // 4 bytes
                // Sets bit 31 and bit 30, bit 29 remains clear
                value ||| 0xC000_0000u |> this.WriteU4
            elif value > 0x7Fu then // 2 bytes
                // Sets bit 15, bit 14 remains clear
                value ||| 0x8000u |> this.WriteU2
            else // 1 byte
                // Bit 7 remains clear
                this.WriteU1 value
        member inline this.WriteCompressed value = this.WriteCompressed(uint32 value)

/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type internal BlobHeap internal (metadata: CliMetadata) =
    // Couldn't find documentation indicating what the first index of the first blob is, so it is assumed that index 0 corresponds to the empty blob.

    // let field = HeapCollection<FieldSignature> metadata.Field.Count // TODO: Add field signatures

    let methodDef =
        let signatures = HeapCollection<MethodDefSignature>(metadata.MethodDef.Count, 0u (* field.Count *))
        for row in metadata.MethodDef.Items do
            signatures.Add row.Signature |> ignore
        signatures

    // MemberRef
    let methodRef, fieldRef =
        let methods = HeapCollection<MethodRefSignature>(metadata.MemberRef.Count, methodDef.Offset) // TODO: Get count of previous one.
        let fields = () // NOTE: Can't use methods.Offset here since it hasn't been populated yet.
        for row in metadata.MemberRef.Items do
            match row with
            | MethodRef method -> methods.Add method.Signature
            |> ignore

        methods, fields



    let attributes =
        let signatures = HeapCollection<CustomAttributeSignature>(metadata.CustomAttribute.Length, methodRef.Offset) // TODO: Get the previous one.
        for row in metadata.CustomAttribute do
            match row.Value with
            | Some value -> signatures.Add value |> ignore
            | None -> ()
        signatures

    // TODO: Add other blobs.
    let last = attributes

    member _.IndexOf signature = methodDef.IndexOf signature
    member _.IndexOf signature = methodRef.IndexOf signature
    member _.IndexOf signature = attributes.IndexOf signature

    member _.Count =
        methodDef.Count
        
        + methodRef.Count
        //+ fieldRef.Count

        + attributes.Count
    member _.IndexSize = last.IndexSize

    member private _.WriteIndex(i: uint32, writer: ChunkWriter) = last.WriteRawIndex(i, writer)

    member this.WriteEmpty writer = this.WriteIndex(0u, writer)
    member this.WriteIndex(signature: MethodDefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
    member this.WriteIndex(signature: MethodRefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
    member this.WriteIndex(signature: CustomAttributeSignature option, writer) =
        let index =
            signature
            |> Option.map this.IndexOf
            |> Option.defaultValue 0u
        this.WriteIndex(index, writer)

    member _.WriteHeap(content: ChunkList) =
        // TODO: Figure out how big chunks should be, or calculate sizes of all blobs beforehand.
        let writer = ChunkWriter.After(content.Tail.Value, 32)
        writer.WriteU1 0uy // Empty

        for signature in methodDef do
            writer.WriteU1 signature.Flags

            // match signature.CallingConventions with
            // | MethodCallingConventions.Generic count -> invalidOp "TODO: Write number of generic parameters."

            // TODO: Create extension methods/create subclass/create methods to write compressed integers and items such as Param.

            writer.WriteCompressed signature.Parameters.Length

            ()
        ()

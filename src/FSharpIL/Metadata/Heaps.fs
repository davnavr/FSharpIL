namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata

open FSharpIL.Bytes
open FSharpIL.Writing

type internal IHeap<'T> =
    inherit IReadOnlyCollection<'T>

    abstract IndexOf: 'T -> uint32
    abstract ByteLength: uint32

[<AutoOpen>]
module internal HeapExtensions =
    [<Literal>]
    let private MaxSmallIndex = 0xFFFFu

    type IHeap<'T> with
        member this.IndexSize = if this.ByteLength > MaxSmallIndex then 4 else 2

        member this.WriteIndex(item, writer: ChunkWriterOld) =
            let i = this.IndexOf item
            if this.IndexSize = 4
            then writer.WriteU4 i
            else writer.WriteU2 i

    [<Extension>]
    [<AbstractClass; Sealed>]
    type SpecificHeapExtensions =
        [<Extension>]
        static member WriteStringIndex<'T>(strings: IHeap<string>, name: 'T, writer) = strings.WriteIndex(name.ToString(), writer)
        [<Extension>]
        static member WriteZero(guids: IHeap<Guid>, writer) = guids.WriteIndex(Guid.Empty, writer)

[<RequireQualifiedAccess>]
module internal Heap =
    type Collection<'T when 'T : equality> internal (capacity: int32, initialSize, sizeOf, indexOf) =
        let items = List<'T> capacity
        let lookup = Dictionary<'T, uint32> capacity
        let mutable size = initialSize

        new(capacity, initialSize, sizeOf) = Collection(capacity, initialSize, sizeOf, Some)
        new(capacity, initialSize) = Collection(capacity, initialSize, (fun _ -> 1u))

        member _.Count = lookup.Count

        member _.ByteLength = size

        member _.IndexOf item: uint32 =
            match indexOf item with
            | Some item' -> lookup.Item item'
            | None -> 0u

        member _.Add item =
            if lookup.ContainsKey item
            then false
            else
                items.Add item
                lookup.Item <- item, size
                size <- size + (sizeOf item)
                true

        interface IHeap<'T> with
            member this.Count = this.Count
            member this.IndexOf item = this.IndexOf item
            member this.ByteLength = this.ByteLength
            member _.GetEnumerator() = items.GetEnumerator() :> IEnumerator<_>
            member _.GetEnumerator() = items.GetEnumerator() :> System.Collections.IEnumerator

    /// <summary>Creates the <c>#Strings</c> metadata stream (II.24.2.3).</summary>
    let strings (metadata: CliMetadata) =
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
            let length (str: string) =
                1u + (Encoding.UTF8.GetByteCount str |> uint32)
            let indexOf =
                function
                | ""
                | null -> None
                | str -> Some str
            Collection<_>(capacity, 1u, length, indexOf)
        let add =
            function
            | ""
            | null -> ()
            | str -> strings.Add str |> ignore

        string metadata.Module.Name |> add

        for tref in metadata.TypeRef.Items do
            string tref.TypeName |> add
            add tref.TypeNamespace

        for tdef in metadata.TypeDef.Items do
            string tdef.TypeName |> add
            add tdef.TypeNamespace

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

        strings

    /// <summary>Creates the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
    let guid (metadata: CliMetadata) =
        let guids =
            let indexOf guid =
                if guid = Guid.Empty
                then None
                else Some guid
            Collection<Guid>(1, 1u, (fun _ -> 16u), indexOf) // Module table is always present.
        guids.Add metadata.Module.Mvid |> ignore
        guids

    let writeStrings (strings: IHeap<string>) (content: ChunkListOld) =
        let writer = ChunkWriterOld.After(content.Tail.Value, int32 strings.ByteLength)
        writer.WriteU1 0uy
        for str in strings do
            Encoding.UTF8.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy

    let writeGuid (guids: IHeap<Guid>) (content: ChunkListOld) =
        let writer = ChunkWriterOld.After(content.Tail.Value, int32 guids.ByteLength)
        for guid in guids do
            guid.ToByteArray() |> writer.WriteBytes

type internal Heap<'T when 'T : equality> = Heap.Collection<'T>

[<Obsolete>]
[<Sealed>]
type private HeapCollection<'Key when 'Key : equality> internal (capacity: int32, offset: uint32) =
    [<Literal>]
    let MaxSmallIndex = 0xFFFFu
    let items = Array.zeroCreate<'Key> capacity
    let lookup = Dictionary<'Key, uint32> capacity
    let mutable i = 0

    new(capacity) = HeapCollection(capacity, 0u)

    member _.Add(item, index) =
        if lookup.ContainsKey item
        then false
        else
            items.[i] <- item
            i <- i + 1
            lookup.Item <- item, index
            true

    member this.Add item = this.Add(item, (uint32 i) + 1u + offset)

    member _.Count = lookup.Count

    member this.Offset = uint32 this.Count + offset

    member _.IndexOf item = lookup.Item item

    member this.IndexSize = if this.Offset > MaxSmallIndex then 4 else 2

    // TODO: Make an HeapCollectonEnumerator struct?
    member _.GetEnumerator() = ArraySegment(items, 0, lookup.Count).GetEnumerator() :> IEnumerator<_>

    member this.WriteRawIndex(i, writer: ChunkWriterOld) =
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    interface IReadOnlyCollection<'Key> with
        member this.Count = lookup.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

// TODO: Determine if adding strings first and then allowing retrieval of index is faster than assigning an index to each string as it is written.
// TODO: Implement merging of strings that end the same. Ex: "HelloWorld" and "World" end in the same, so indices would be x and x + 5.
// TODO: Prevent null character from being used in strings.

/// <summary>Represents the <c>#US</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type internal UserStringHeap internal (metadata: CliMetadata) =
    let strings =
        1
        |> Dictionary<string, uint32>

    // NOTE: When writing this heap, see II.24.2.4 to see how lengths of the bytes are encoded.

[<AutoOpen>]
module private WriterExtensions =
    [<Literal>]
    let MaxCompressedUnsigned = 0x1FFF_FFFFu

    type ChunkWriterOld with
        /// <summary>Writes an unsigned compressed integer in big-endian order (II.23.2).</summary>
        /// <exception cref="System.ArgumentException">The <paramref name="value"/> is greater than the maximum compressed unsigned integer.</exception>
        member this.WriteCompressed(value: uint32) =
            if value > MaxCompressedUnsigned then
                sprintf
                    "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x."
                    value
                    MaxCompressedUnsigned
                |> invalidArg (nameof value)
            elif value > 0x3FFFu then // 4 bytes
                // Sets bit 31 and bit 30, bit 29 remains clear
                let (U4 (msb, b2, b3, lsb)) = value ||| 0xC000_0000u
                this.WriteU1 msb
                this.WriteU1 b2
                this.WriteU1 b3
                this.WriteU1 lsb
            elif value > 0x7Fu then // 2 bytes
                // Sets bit 15, bit 14 remains clear
                let (U2 (msb, lsb)) = uint16 (value ||| 0x8000u)
                this.WriteU1 msb
                this.WriteU1 lsb
            else // 1 byte
                // Bit 7 remains clear
                this.WriteU1 value

        member inline this.WriteCompressed value = this.WriteCompressed(uint32 value)

        member _.WriteCompressedSigned(value: int32) =
            failwith "TODO: Implement writing of compressed integers"
            ()

        member inline this.WriteCompressedSigned value = this.WriteCompressedSigned(int32 value)

        member this.WriteArrayShape(shape: ArrayShape) =
            this.WriteCompressed shape.Rank
            this.WriteCompressed shape.Sizes.Length // NumSizes
            for size in shape.Sizes do this.WriteCompressed size
            this.WriteCompressed shape.LowerBounds.Length // NumLoBounds
            for bound in shape.LowerBounds do this.WriteCompressedSigned bound // LoBound

        member this.WriteType(item: EncodedType) =
            match item with
            | EncodedType.Array(element, shape) ->
                this.WriteU1 ElementType.Array
                this.WriteType element
                this.WriteArrayShape shape

            | EncodedType.String -> this.WriteU1 ElementType.String

            | bad -> failwithf "Unsupported type %A" bad

        member _.WriteCustomMod(modifiers: #IReadOnlyCollection<CustomModifier>) =
            if modifiers.Count > 0 then
                failwith "TODO: Implement writing of custom modifiers"
                ()

        member this.WriteRetType(item: ReturnTypeItem) =
            this.WriteCustomMod item.CustomMod
            match item.ReturnType with
            | ReturnType.Void -> this.WriteU1 ElementType.Void

        member this.WriteParam(item: ParamItem) =
            this.WriteCustomMod item.CustomMod
            this.WriteType item.ParamType

        member this.WriteParameters(items: System.Collections.Immutable.ImmutableArray<ParamItem>) =
            for param in items do this.WriteParam param

        member this.WriteElem(elem: Elem) =
            match elem with
            | ValBool true -> this.WriteU1 1uy
            | ValBool false -> this.WriteU1 0uy
            | SerString "" -> this.WriteCompressed 0u
            | SerString null -> this.WriteCompressed 0xFFu
            | SerString str ->
                let bytes = Encoding.UTF8.GetBytes str
                this.WriteCompressed bytes.Length // PackedLen
                this.WriteBytes bytes
            | bad -> failwithf "Unsupported element %A" bad

        member this.WriteFixedArg(arg: FixedArg) =
            match arg with
            | FixedArg.Elem elem -> this.WriteElem elem
            | FixedArg.SZArray _ -> failwith "TODO: Implement writing of SZArray for FixedArg"

// TODO: Figure out why method signature is empty byte array. Maybe indices into #Blob point to individual bytes similar to #Strings?
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

    member private _.WriteIndex(i: uint32, writer: ChunkWriterOld) = last.WriteRawIndex(i, writer)

    member this.WriteEmpty writer = this.WriteIndex(0u, writer)
    member this.WriteIndex(signature: MethodDefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
    member this.WriteIndex(signature: MethodRefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
    member this.WriteIndex(signature: CustomAttributeSignature option, writer) =
        let index =
            signature
            |> Option.filter
                (fun signature' -> not signature'.FixedArg.IsEmpty && not signature'.NamedArg.IsEmpty)
            |> Option.map this.IndexOf
            |> Option.defaultValue 0u
        this.WriteIndex(index, writer)

    member _.WriteHeap(content: ChunkListOld) =
        // TODO: Figure out how big chunks should be, or calculate sizes of all blobs beforehand.
        let writer = ChunkWriterOld.After(content.Tail.Value, 32)
        writer.WriteU1 0uy // Empty

        // Field

        for signature in methodDef do
            writer.WriteU1 signature.Flags

            // match signature.CallingConventions with
            // | MethodCallingConventions.Generic count -> invalidOp "TODO: Write number of generic parameters."

            writer.WriteCompressed signature.Parameters.Length // ParamCount
            writer.WriteRetType signature.ReturnType
            writer.WriteParameters signature.Parameters

        for signature in methodRef do
            writer.WriteU1 signature.CallingConventions
            writer.WriteCompressed signature.Parameters.Length // ParamCount
            writer.WriteRetType signature.ReturnType
            writer.WriteParameters signature.Parameters
            // TODO: Write SENTINEL and extra parameters.
            if not signature.VarArgParameters.IsEmpty then
                failwith "TODO: Implement writing of VarArg parameters"

        // FieldRef




        for signature in attributes do
            if signature.FixedArg.IsEmpty && signature.NamedArg.IsEmpty then
                invalidOp "Attributes without any fixed or named arguments should use the empty blob instead."
            writer.WriteU2 1us // Prolog
            for arg in signature.FixedArg do
                writer.WriteFixedArg arg
            writer.WriteU2 signature.NamedArg.Length // NumNamed
            for arg in signature.NamedArg do
                failwithf "TODO: Implement writing of named arguments for custom attributes"

        // TODO: Write other blobs.
        ()

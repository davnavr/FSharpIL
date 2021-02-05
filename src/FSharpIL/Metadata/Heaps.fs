namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata

open FSharpIL.Bytes
open FSharpIL.Writing

type internal IHeap =
    abstract ByteLength: uint32

type internal IHeap<'T> =
    inherit IReadOnlyCollection<'T>
    inherit IHeap

    abstract IndexOf: 'T -> uint32

[<AutoOpen>]
module internal HeapExtensions =
    [<Literal>]
    let private MaxSmallIndex = 0xFFFFu

    type IHeap with
        member this.IsEmpty = this.ByteLength = 0u
        member this.IndexSize = if this.ByteLength > MaxSmallIndex then 4 else 2

    type IHeap<'T> with
        member this.WriteIndex(item, writer: ChunkWriter) =
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

[<AutoOpen>]
module private ChunkWriterExtensions =
    [<Literal>]
    let MaxCompressedUnsigned = 0x1FFF_FFFFu

    type ChunkWriter with
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

/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<ReferenceEquality; NoComparison>]
type internal BlobHeap =
    { // Field: Dictionary< , uint32>
      MethodDef: Dictionary<MethodDefSignature, uint32>
      // MemberRef contains both MethodRef and FieldRef
      MethodRef: Dictionary<MethodRefSignature, uint32>
      // FieldRef: Dictionary< , uint32>
      
      CustomAttribute: Dictionary<CustomAttributeSignature, uint32>
      mutable ByteLength: uint32 }

    member this.SignatureCount =
        this.MethodDef.Count
        + this.MethodRef.Count

        + this.CustomAttribute.Count

    member private this.WriteRawIndex(i: uint32, writer: ChunkWriter) =
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    member this.WriteEmpty writer = this.WriteRawIndex(0u, writer)
    member this.WriteIndex(methodDef, writer) = this.WriteRawIndex(this.MethodDef.Item methodDef, writer)
    member this.WriteIndex(methodRef, writer) = this.WriteRawIndex(this.MethodRef.Item methodRef, writer)

    member this.WriteIndex(attribute, writer) =
        match attribute with
        | Some item -> this.WriteRawIndex(this.CustomAttribute.Item item, writer)
        | None -> this.WriteEmpty writer

    interface IHeap with
        member this.ByteLength = this.ByteLength

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

        member _.GetEnumerator() = items.GetEnumerator()

        interface IHeap<'T> with
            member this.Count = this.Count
            member this.IndexOf item = this.IndexOf item
            member this.ByteLength = this.ByteLength
            member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
            member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

    // TODO: Implement merging of strings that end the same. Ex: "HelloWorld" and "World" end in the same, so indices would be x and x + 5.
    // TODO: Prevent null character from being used in strings.
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

    let writeStrings (strings: IHeap<string>) (writer: ChunkWriter) =
        writer.WriteU1 0uy
        for str in strings do
            Encoding.UTF8.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy

    let writeGuid (guids: IHeap<Guid>) (writer: ChunkWriter) =
        for guid in guids do
            guid.ToByteArray() |> writer.WriteBytes

    // TODO: Since each blob includes its length in bytes, and the length information is variable width, calculate length of #Blob heap before writing.
    /// <summary>Writes the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
    let blob (metadata: CliMetadata) =
        { 
          MethodDef = Dictionary<_, _> metadata.MethodDef.Count
          MethodRef = Dictionary<_, _> metadata.MemberRef.Count

          CustomAttribute = Dictionary<_, _> metadata.CustomAttribute.Length
          ByteLength = 0u }

    let writeBlob (blob: BlobHeap) (metadata: CliMetadata) (writer: ChunkWriter) =
        let mutable i = 1u
        writer.WriteU1 0uy // Empty blob?
        // TODO: Figure out how indices into #Blob are formatted?

        // TODO: Iterate through each signature and add it to the dictionaries after writing

        // Field
        for methodDef in metadata.MethodDef.Items do
            let signature = methodDef.Signature
            if blob.MethodDef.ContainsKey signature |> not then
                let size = writer.Size
                writer.WriteU1 signature.Flags
        
                // match signature.CallingConventions with
                // | MethodCallingConventions.Generic count -> invalidOp "TODO: Write number of generic parameters."
        
                writer.WriteCompressed signature.Parameters.Length // ParamCount
                writer.WriteRetType signature.ReturnType
                writer.WriteParameters signature.Parameters
                i <- i + writer.Size - size
                blob.MethodDef.Item <- signature, (i |> invalidOp "TODO: Fancy index calculations.")

        ()

type internal Heap<'T when 'T : equality> = Heap.Collection<'T>

/// <summary>Represents the <c>#US</c> metadata stream (II.24.2.4).</summary>
[<Obsolete; Sealed>]
type internal UserStringHeap internal (metadata: CliMetadata) =
    let strings =
        1
        |> Dictionary<string, uint32>

    // NOTE: When writing this heap, see II.24.2.4 to see how lengths of the bytes are encoded.

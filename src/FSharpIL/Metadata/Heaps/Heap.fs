namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module internal Heap =
    type Lookup<'T when 'T : equality> (heap: Dictionary<'T, uint32>, byteLength: uint32, indexOf) =
        member _.ByteLength = byteLength
        member _.Count = heap.Count
        member _.ContainsKey item = heap.ContainsKey item
        member _.IndexOf item =
            match indexOf item with
            | Some i -> i
            | None -> heap.Item item

        interface IHeap with
            member this.ByteLength = this.ByteLength

        interface IReadOnlyDictionary<'T, uint32> with
            member this.Count = this.Count
            member this.Item with get item = this.IndexOf item
            member _.Keys = heap.Keys :> IEnumerable<_>
            member _.Values = heap.Values :> IEnumerable<_>
            member this.ContainsKey item = this.ContainsKey item
            member _.GetEnumerator() = heap.GetEnumerator() :> IEnumerator<_>
            member _.GetEnumerator() = heap.GetEnumerator() :> System.Collections.IEnumerator
            member _.TryGetValue(item, index) = heap.TryGetValue(item, &index)

    /// <summary>
    /// Applies the given function to each string in the <c>#Strings</c> heap referenced in
    /// the CLI metadata tables (II.24.2.6).
    /// </summary>
    let inline internal iterStrings action (metadata: CliMetadata) =
        metadata.Module.Name.ToString() |> action
    
        for tref in metadata.TypeRef.Rows do
            tref.TypeName.ToString() |> action
            action tref.TypeNamespace
    
        for tdef in metadata.TypeDef.Rows do
            tdef.TypeName.ToString() |> action
            action tdef.TypeNamespace
    
        for field in metadata.Field.Rows do
            field.Name.ToString() |> action
    
        for method in metadata.MethodDef.Rows do
            method.Name.ToString() |> action
    
        for _, param in metadata.Param do
            action param.ParamName
    
    
    
        for mref in metadata.MemberRef.Rows do
            mref.Name.ToString() |> action
    
        for property in metadata.Property.Rows do
            property.Name.ToString() |> action
    
        match metadata.Assembly with
        | ValueSome assembly ->
            assembly.Name.ToString() |> action
            assembly.Culture.ToString() |> action
        | ValueNone -> ()
    
        for assembly in metadata.AssemblyRef.Rows do
            assembly.Name.ToString() |> action
            assembly.Culture.ToString() |> action
    
        for { FileName = name } in metadata.File.Rows do
            name.ToString() |> action
    
        for gparam in metadata.GenericParam.Rows do
            gparam.Name.ToString() |> action

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

                + metadata.Property.Count

                + if metadata.Assembly.IsSome then 2 else 0
                + (2 * metadata.AssemblyRef.Count)
            Dictionary<_, _> capacity
        let mutable size = 1u

        iterStrings
            (function
            | ""
            | null -> ()
            | str ->
                if strings.TryAdd(str, size) then
                    size <- size + 1u + (Encoding.UTF8.GetByteCount str |> uint32))
            metadata

        let indexOf =
            function
            | ""
            | null -> Some 0u
            | _ -> None

        Lookup<_>(strings, size, indexOf)

    /// <summary>Creates the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
    let guid (metadata: CliMetadata) =
        let guids = Dictionary<_, _> 1 // Module table is always present

        guids.Add(metadata.Module.Mvid, 1u)

        let indexOf guid =
            if guid = Guid.Empty
            then Some 0u
            else None

        Lookup<_>(guids, uint32 guids.Count * 16u, indexOf)

    let private methodRefSig (blob: SerializedBlobHeap) (writer: inref<BlobWriter>) tag i =
        let i' = MethodRefSignature(tag, i)
        if not(blob.MethodRefSig.ContainsKey i') then
            writer.MethodRefSig i'
            blob.CreateIndex(i', blob.MethodRefSig)

    /// <summary>Creates the <c>#Blob</c> metadata stream, which contains signatures (II.24.2.4).</summary>
    let blob (metadata: CliMetadata) size =
        let blob = SerializedBlobHeap(metadata.Blobs, size)
        let writer = BlobWriter(metadata, blob.Content)

        // FieldSig
        for i = 0 to metadata.Blobs.FieldSig.Count - 1 do
            let i' = Blob i
            if not(blob.FieldSig.ContainsKey i') then
                writer.FieldSig &metadata.Blobs.FieldSig.[i']
                blob.CreateIndex(i', blob.FieldSig)

        // MethodDefSig
        for i = 0 to metadata.Blobs.MethodDefSig.Count - 1 do
            let i' = Blob i
            if not(blob.MethodDefSig.ContainsKey i') then
                writer.MethodDefSig(&metadata.Blobs.MethodDefSig.[i'])
                blob.CreateIndex(i', blob.MethodDefSig)

        // MethodRefSig
        for i = 0 to metadata.Blobs.MethodRefSig.DefaultCount - 1 do
            methodRefSig blob &writer MemberRefSignatureTag.MethodDefault i
        for i = 0 to metadata.Blobs.MethodRefSig.GenericCount - 1 do
            methodRefSig blob &writer MemberRefSignatureTag.MethodGeneric i
        for i = 0 to metadata.Blobs.MethodRefSig.VarArgCount - 1 do
            methodRefSig blob &writer MemberRefSignatureTag.MethodVarArg i

        // Constant
        for i = 0 to metadata.Blobs.Constant.Integers.Length - 1 do
            let value = metadata.Blobs.Constant.Integers.[i]
            let i' = ConstantBlob(value.Tag.ConstantType, i)
            if not(blob.Constant.ContainsKey i') then
                match value.Tag with
                | IntegerType.Bool
                | IntegerType.I1
                | IntegerType.U1 -> writer.Writer.WriteU1 value.U1
                | IntegerType.Char
                | IntegerType.I2
                | IntegerType.U2 -> writer.Writer.WriteU2 value.U2
                | IntegerType.I4
                | IntegerType.U4 -> writer.Writer.WriteU4 value.U4
                | IntegerType.I8
                | IntegerType.U8 -> writer.Writer.WriteU4 value.U4
                | _ -> invalidOp "Cannot write integer constant blob for unknown integer type"
                blob.CreateIndex(i', blob.Constant)
        for i = 0 to metadata.Blobs.Constant.Floats.Length - 1 do
            let value = metadata.Blobs.Constant.Floats.[i]
            let i' = ConstantBlob(value.Tag, i)
            if not(blob.Constant.ContainsKey i') then
                match value.Tag with
                | ConstantValueType.R8 -> writer.Writer.WriteU8 value.U8
                | ConstantValueType.R4 -> writer.Writer.WriteU4 value.U4
                | _ -> invalidOp "Invalid floating-point type"
                blob.CreateIndex(i', blob.Constant)
        for i = 0 to metadata.Blobs.Constant.Strings.Length - 1 do
            let (StringConstant value) = metadata.Blobs.Constant.Strings.[i]
            let i' = ConstantBlob(ConstantValueType.String, i)
            if not(blob.Constant.ContainsKey i') then
                writer.Writer.WriteBytes(System.Text.Encoding.Unicode.GetBytes value)
                blob.CreateIndex(i', blob.Constant)
        if metadata.Blobs.Constant.Count > metadata.Blobs.Constant.Integers.Length + metadata.Blobs.Constant.Strings.Length then
            failwith "TODO: Write Float constants."

        // CustomAttribute
        for i = 0 to metadata.Blobs.CustomAttribue.Count - 1 do
            let i' = Blob i
            if not(blob.CustomAttribute.ContainsKey i') then
                let signature = &metadata.Blobs.CustomAttribue.[i']
                writer.Writer.WriteU2 1us // Prolog
                for arg in signature.FixedArg do
                    writer.FixedArg arg
                writer.Writer.WriteU2 signature.NamedArg.Length // NumNamed
                for arg in signature.NamedArg do
                    failwithf "TODO: Implement writing of named arguments for custom attributes"
                blob.CreateIndex(i', blob.CustomAttribute)
                ()

        // PropertySig
        for i = 0 to metadata.Blobs.PropertySig.Count - 1 do
            let i' = Blob i
            if not(blob.PropertySig.ContainsKey i') then
                let signature = &metadata.Blobs.PropertySig.[i']
                let property = if signature.HasThis then 0x28uy else 0x8uy
                writer.Writer.WriteU1 property
                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.CustomMod signature.CustomMod
                writer.EncodedType signature.Type
                writer.Parameters signature.Parameters
                blob.CreateIndex(i', blob.PropertySig)

        // TypeSpec
        for i = 0 to metadata.Blobs.TypeSpec.Count - 1 do
            let i' = TypeSpecBlob i
            if not(blob.TypeSpec.ContainsKey i') then
                match metadata.Blobs.TypeSpec.[i'] with
                | TypeSpec.GenericInst inst -> writer.GenericInst &inst
                // TODO: Factor out common code shared between writing of TypeSpec and encoded Type.
                | TypeSpec.MVar num ->
                    writer.Writer.WriteU1 ElementType.MVar
                    writer.CompressedUnsigned num
                | TypeSpec.Var num ->
                    writer.Writer.WriteU1 ElementType.Var
                    writer.CompressedUnsigned num
                blob.CreateIndex(i', blob.TypeSpec)

        // MethodSpec
        for i = 0 to metadata.Blobs.MethodSpec.Count - 1 do
            let i' = MethodSpecBlob i
            if not(blob.MethodSpec.ContainsKey i') then
                let inst = &metadata.Blobs.MethodSpec.[i']
                writer.Writer.WriteU1 0xAuy // GENERICINST
                writer.CompressedUnsigned inst.Count
                for gparam in inst.GenericArguments do writer.EncodedType gparam
                blob.CreateIndex(i', blob.MethodSpec)

        // LocalVarSig
        for i = 0 to metadata.Blobs.LocalVarSig.Count - 1 do
            let i' = Blob i
            if not(blob.LocalVarSig.ContainsKey i') then
                let signature = &metadata.Blobs.LocalVarSig.[i']
                writer.Writer.WriteU1 0x7uy // LOCAL_SIG
                writer.CompressedUnsigned signature.Length // Count
                for local in signature do
                    match local with
                    | LocalVariable.TypedByRef -> writer.Writer.WriteU1 ElementType.TypedByRef
                    | _ ->
                        writer.CustomMod local.CustomMod
                        for constr in local.Constraints do
                            match constr with
                            | LocalVariableConstraint.Pinned -> writer.Writer.WriteU1 ElementType.Pinned
                        if local.Tag = LocalVariableTag.ByRef then
                            writer.Writer.WriteU1 ElementType.ByRef
                        writer.EncodedType local.LocalType
                blob.CreateIndex(i', blob.LocalVarSig)

        // Miscellaneous
        for i = 0 to metadata.Blobs.MiscBytes.Count - 1 do
            let i' = Blob i
            if not(blob.MiscBytes.ContainsKey i') then
                writer.Writer.WriteBytes metadata.Blobs.MiscBytes.[i']
                blob.CreateIndex(i', blob.MiscBytes)

        blob

    let writeStrings (count: int32) (metadata: CliMetadata) (writer: ChunkWriter) =
        let lookup = HashSet<string> count
        writer.WriteU1 0uy
        iterStrings
            (function
            | ""
            | null -> ()
            | str ->
                if lookup.Add str then
                    Encoding.UTF8.GetBytes str |> writer.WriteBytes
                    writer.WriteU1 0uy)
            metadata

    let writeGuid (metadata: CliMetadata) (writer: ChunkWriter) =
        metadata.Module.Mvid.ToByteArray() |> writer.WriteBytes

    let writeBlob (blobs: SerializedBlobHeap) metadata (writer: ChunkWriter) =
        writer.WriteU1 0uy

        let mutable chunk, i = blobs.Content.Chunk.List.First, 0
        let writeAll (dict: Dictionary<_, BlobIndex>) =
            let writer' = BlobWriter(metadata, writer)
            for KeyValue(_, index) in dict do
                writer'.CompressedUnsigned index.Size
                let struct(chunk', i') = writer.WriteBytes(chunk, i, index.Size)
                chunk <- chunk'
                i <- i'

        writeAll blobs.FieldSig
        writeAll blobs.MethodDefSig
        writeAll blobs.MethodRefSig
        writeAll blobs.Constant
        writeAll blobs.CustomAttribute
        writeAll blobs.PropertySig
        writeAll blobs.TypeSpec
        writeAll blobs.MethodSpec
        writeAll blobs.LocalVarSig
        writeAll blobs.MiscBytes

    let writeUS (us: UserStringHeap) metadata (writer: ChunkWriter) =
        let writer' = BlobWriter(metadata, writer)
        writer.WriteU1 0uy
        for str in us do
            let { BlobIndex.Size = size } = us.IndexOf str
            writer'.CompressedUnsigned size
            Encoding.Unicode.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy // TODO: Determine value of terminal byte.

type internal Heap<'T when 'T : equality> = Heap.Lookup<'T>

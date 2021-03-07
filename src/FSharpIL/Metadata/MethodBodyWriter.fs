namespace rec FSharpIL.Metadata

open FSharpIL.Metadata.Heaps

// TODO: Come up with better name for this class.
[<Sealed>]
type internal MethodBodyContentImpl (writer, metadata, us: UserStringHeap) =
    inherit MethodBodyContent(writer)
    do if writer.Size > 0u then invalidArg "writer" "The method body writer must be a new instance"
    member _.Metadata: CliMetadata = metadata
    member _.UserString = us
    member this.CreateWriter() = MethodBodyWriter this

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
[<System.Runtime.CompilerServices.IsByRefLike; Struct>]
type MethodBodyWriter internal (content: MethodBodyContentImpl) =
    new (content: MethodBodyContent) = MethodBodyWriter(content :?> MethodBodyContentImpl)

    member private _.WriteMetadataToken(index, table) = MetadataToken.write index table content.Writer

    /// Gets the number of bytes that have been written.
    member _.Size = content.Writer.Size
    member _.WriteU1 value = content.Writer.WriteU1 value

    /// (0x00) Writes an instruction that does nothing (III.3.51).
    member this.Nop() = this.WriteU1 0uy
    /// <summary>
    /// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
    /// tripped" (III.3.16).
    /// </summary>
    member this.Break() = this.WriteU1 1uy
    /// (0x2A) Writes an instruction used to return from the current method (III.3.56).
    member this.Ret() = this.WriteU1 0x2Auy
    /// (0x14) Writes an instruction used to load a null pointer (III.3.45).
    member this.Ldnull() = this.WriteU1 0x14uy

    // TODO: Figure out how numbers are written in the IL. Are they compressed?

    /// <summary>
    /// (0x15 to 0x1E, 0x20) Writes an instruction that pushes a signed four-byte integer onto the stack (III.3.40).
    /// </summary>
    /// <remarks>
    /// This method automatically writes the short forms of these opcodes depending on the number that is to be pushed onto the
    /// stack.
    /// </remarks>
    member this.Ldc_i4(num: int32) =
        match num with
        | -1 -> this.WriteU1 0x15uy
        | _ when num <= 8 -> this.WriteU1(0x16uy + uint8 num)
        | _ ->
            this.WriteU1 0x20uy
            failwith "TODO: How is an int32 written?"

    /// <summary>
    /// (0x1F) Writes an instruction that pushes a signed one-byte integer onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    member this.Ldc_i4(num: int8) =
        this.WriteU1 0x1Fuy
        failwith "TODO: How is an int8 written?"

    /// (0x21) Writes an instruction that pushes a signed eight-byte integer onto the stack (III.3.40).
    member this.Ldc_i8(num: int64) =
        this.WriteU1 0x21uy
        failwith "TODO: How is an int64 written?"

    /// (0x22)
    member this.Ldc_r4(num: float32) =
        this.WriteU1 0x22uy
        failwith "TODO: How is a r4 written?"

    /// (0x23)
    member this.Ldc_r8(num: double) =
        this.WriteU1 0x23uy
        failwith "TODO: How is a r8 written?"

    // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    /// <summary>(0x28) Writes an instruction that calls a method (III.3.19).</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member this.Call(SimpleIndex method: MemberRefIndex<MethodRef>) =
        IndexOwner.checkIndex content.Metadata.Owner method
        this.WriteU1 0x28uy
        this.WriteMetadataToken(content.Metadata.MemberRef.IndexOf method, 0xAuy)

    /// <summary>
    /// (0x72) Writes an instruction that loads a string from the <c>#US</c> heap (III.4.16).
    /// </summary>
    /// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
    /// <exception cref="T:System.ArgumentNullException">Thrown when the input string is <see langword="null"/>.</exception>
    member this.Ldstr(str: string) =
        match str with
        | null -> nullArg "str"
        | _ ->
            this.WriteU1 0x72uy
            MetadataToken.userString str content.UserString content.Writer

    member private this.WriteFieldInstruction(opcode, SimpleIndex field: FieldIndex<_>) =
        this.WriteU1 opcode
        this.WriteMetadataToken(content.Metadata.Field.IndexOf field, 0x4uy)
    // TODO: Allow a FieldRef to be used when loading an instance field.
    /// <summary>(0x7B) Writes an instruction that pushes the value of an object's field onto the stack (III.4.10).</summary>
    /// <param name="field">The field to load the value of.</param>
    member this.Ldfld field = this.WriteFieldInstruction(0x7Buy, field)

    /// <summary>(0x7D) Writes an instruction that stores a value into an object's field (III.4.28).</summary>
    member this.Stfld field = this.WriteFieldInstruction(0x7Duy, field)
    // TODO: Allow a FieldRef to be used when loading a static field.
    /// <summary>(0x7E) Writes an instruction that pushes the value of a static field onto the stack (III.4.14).</summary>
    /// <param name="field">The static field to load the value of.</param>
    member this.Ldsfld field = this.WriteFieldInstruction(0x7Euy, field)

    /// <summary>(0x7D) Writes an instruction that stores a value into a static field (III.4.30).</summary>
    member this.Stsfld field = this.WriteFieldInstruction(0x80uy, field)

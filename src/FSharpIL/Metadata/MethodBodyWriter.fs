namespace rec FSharpIL.Metadata

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Heaps
open FSharpIL.Writing

// TODO: Come up with better name for this class.
/// <exception cref="T:System.ArgumentException">
/// The number of bytes written by the <paramref name="writer"/> is greater than zero.
/// </exception>
[<Sealed>]
type internal MethodBodyContentImpl (writer, metadata, us: UserStringHeap) =
    inherit MethodBodyContent(writer)
    do if writer.Size > 0u then invalidArg "writer" "The method body writer must be a new instance"
    member _.Metadata: CliMetadata = metadata
    member _.UserString = us
    member this.CreateWriter() = MethodBodyWriter this

/// Represents the target of a branch instruction (III.1.7.2).
[<IsByRefLike>]
type BranchTarget = struct
    val mutable private existing: bool
    val private writer: ChunkWriter
    /// Gets a value indicating whether this branch target is a 1-byte offset.
    val IsByte: bool
    /// Gets the position of the next byte immediately after this offset.
    val Position: uint32

    internal new (position, isByte, writer) =
        { existing = false
          writer = writer
          IsByte = isByte
          Position = position }

    member internal this.ReserveBytes(writer: ChunkWriter) =
        if this.IsByte
        then writer.SkipBytes 1u
        else writer.SkipBytes 4u

    /// <summary>Sets the target of a branch instruction.</summary>
    /// <param name="offset">
    /// A byte offset from the branch instruction, where a value of <c>0</c> targets the next instruction.
    /// </param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when this target is a 1-byte offset, and the <paramref name="offset"/> cannot be represented in 1 byte.
    /// </exception>
    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown when this method is called more than once, since the target index cannot be rewritten.
    /// </exception>
    member this.SetTarget(offset: int32) =
        if this.existing then invalidOp "The target index has already been written"
        this.existing <- true
        if this.IsByte then
            if offset < -128 || offset > 127 then
                ArgumentOutOfRangeException("offset", offset, "Expected one-byte integer offset") |> raise
            this.writer.WriteU1(uint8 offset)
        else
            failwith "TODO: Write the offset as a signed 4-byte integer"
end

[<RequireQualifiedAccess>]
module private LocalVarIndex =
    let (|Simple|Short|Long|) (index: uint16) =
        if index <= 0xFFus then
            if index <= 3us then Simple else Short
        elif index < 0xFFFFus then Long
        else ArgumentOutOfRangeException("indx", index, "A local variable index of 65535 is invalid") |> raise

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
[<IsByRefLike; Struct>]
type MethodBodyWriter internal (content: MethodBodyContentImpl) =
    new (content: MethodBodyContent) = MethodBodyWriter(content :?> MethodBodyContentImpl)

    member private _.WriteMetadataToken(index, table) = MetadataToken.write index table content.Writer

    /// Gets the number of bytes that have been written.
    member _.ByteCount = content.Writer.Size

    member private _.WriteU1 value = content.Writer.WriteU1 value

    /// (0x00) Writes an instruction that does nothing (III.3.51).
    member this.Nop() = this.WriteU1 0uy

    /// <summary>
    /// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
    /// tripped" (III.3.16).
    /// </summary>
    member this.Break() = this.WriteU1 1uy

    /// <summary>
    /// (0x02 to 0x05, 0x0E, 0xFE 0x09) Writes an instruction that loads an argument onto the stack (III.3.38).
    /// </summary>
    /// <remarks>
    /// For argument numbers 0 through 3, and for argument numbers less than 255, the short forms of the opcodes are used.
    /// </remarks>
    member this.Ldarg(num: uint16) =
        if num <= 255us then
            let num' = uint8 num
            if num' <= 3uy
            then this.WriteU1(0x2uy + num')
            else this.WriteU1 0xEuy; this.WriteU1 num'
        else
            this.WriteU1 0xFEuy
            this.WriteU1 0x09uy
            content.Writer.WriteU2 num

    /// <summary>
    /// (0x06 to 0x09, 0x11, 0xFE 0x0C) Writes an instruction that pushes the contents of a local variable onto the stack
    /// (III.3.43).
    /// </summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the local variable <paramref name="index"/> is equal to <c>65535</c>, which is an invalid index.
    /// </exception>
    member this.Ldloc(index: uint16) =
        match index with
        | LocalVarIndex.Simple -> this.WriteU1(6uy + uint8 index)
        | LocalVarIndex.Short -> this.WriteU1 0x11uy; content.Writer.WriteU1 index
        | LocalVarIndex.Long ->
            this.WriteU1 0xFEuy
            this.WriteU1 0x0Cuy
            content.Writer.WriteU2 index

    /// <summary>
    /// (0x0A to 0x0D, 0x13, 0xFE 0x0E) Writes an instruction that pops a value from the stack and stores in into a local
    /// variable (III.3.43).
    /// </summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the local variable <paramref name="index"/> is equal to <c>65535</c>, which is an invalid index.
    /// </exception>
    member this.Stloc(index: uint16) =
        match index with
        | LocalVarIndex.Simple -> this.WriteU1(0xAuy + uint8 index)
        | LocalVarIndex.Short -> this.WriteU1 0x13uy; content.Writer.WriteU1 index
        | LocalVarIndex.Long ->
            this.WriteU1 0xFEuy
            this.WriteU1 0x0Euy
            content.Writer.WriteU2 index


    /// (0x2A) Writes an instruction used to return from the current method (III.3.56).
    member this.Ret() = this.WriteU1 0x2Auy

    /// (0x14) Writes an instruction used to load a null pointer (III.3.45).
    member this.Ldnull() = this.WriteU1 0x14uy

    /// <summary>
    /// (0x15 to 0x1F) Writes an instruction that pushes a signed one-byte integer onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    member this.Ldc_i4(num: int8) =
        match num with
        | -1y -> this.WriteU1 0x15uy
        | _ when num <= 8y -> this.WriteU1(0x16uy + uint8 num)
        | _ ->
            this.WriteU1 0x1Fuy
            content.Writer.WriteI1 num

    /// <summary>
    /// (0x15 to 0x1F) Writes an instruction that pushes a <c>char</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    member this.Ldc_i4(c: char) = this.Ldc_i4(int8 c)

    /// <summary>
    /// (0x15 to 0x1F, 0x20) Writes an instruction that pushes a signed four-byte integer onto the stack (III.3.40).
    /// </summary>
    /// <remarks>
    /// This method automatically writes the short forms of these opcodes depending on the number that is to be pushed onto the
    /// stack.
    /// </remarks>
    member this.Ldc_i4(num: int32) =
        if num >= -1 && num <= 127
        then this.Ldc_i4(int8 num)
        else
            this.WriteU1 0x20uy
            failwith "TODO: How is an int32 written?"

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

    /// (0x25) Writes an instruction that duplicates the value on the stack (III.3.33).
    member this.Dup() = this.WriteU1 0x25uy

    /// (0x26) Writes an instruction that "removes the top element from the stack" (III.3.54).
    member this.Pop() = this.WriteU1 0x26uy

    member private this.Call(opcode, index: RawIndex<_>, table) =
        this.WriteU1 opcode
        this.WriteMetadataToken(uint32 index, table)

    // TODO: Allow call and callvirt to accept a MethodSpec.
    member private this.CallMethodRef method = this.Call(0x28uy, method, 0xAuy)

    /// <summary>
    /// (0x28) Writes an instruction that calls a <c>MethodRef</c> with a <c>DEFAULT</c> calling convention (III.3.19).
    /// </summary>
    member this.Call(method: RawIndex<MethodRefDefault>) = this.CallMethodRef method

    /// <summary>
    /// (0x28) Writes an instruction that calls a <c>MethodRef</c> with a <c>GENERIC</c> calling convention (III.3.19).
    /// </summary>
    member this.Call(method: RawIndex<MethodRefGeneric>) = this.CallMethodRef method

    /// <summary>
    /// (0x28) Writes an instruction that calls a <c>MethodRef</c> with a <c>VARARG</c> calling convention (III.3.19).
    /// </summary>
    member this.Call(method: RawIndex<MethodRefVarArg>) = this.CallMethodRef method

    /// <summary>(0x28) Writes an instruction that calls a <c>MethodDef</c> (III.3.19).</summary>
    member this.Call(method: RawIndex<MethodDefRow>) = this.Call(0x28uy, method, 0x6uy)

    /// <summary>(0x28) Writes an instruction that calls a static method specified by a <c>MethodDef</c> (III.3.19).</summary>
    member this.Call(method: RawIndex<StaticMethod>) = this.Call(method.AsMethodIndex())

    member private this.Branch(opcode, isByte) =
        this.WriteU1 opcode
        let offset = if isByte then 1u else 4u
        let target = BranchTarget(this.ByteCount + offset, isByte, content.Writer.CreateWriter())
        target.ReserveBytes content.Writer
        target

    // TODO: Create methods or helper functions that make writing branching instructions easier.
    /// <summary>
    /// (0x2C) Writes the short form of an instruction that branches to the specified target if the <c>value</c> is false or zero
    /// (III.3.17).
    /// </summary>
    member this.Brfalse_s() = this.Branch(0x2Cuy, true)
    /// <summary>
    /// (0x2C) Writes the short form of an instruction that branches to the specified target if the <c>value</c> is
    /// <see langword="null"/> (III.3.17).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brfalse.s</c> instruction.</remarks>
    member this.Brnull_s() = this.Brfalse_s()
    /// <summary>
    /// (0x2C) Writes the short form of an instruction that branches to the specified target if the <c>value</c> is zero
    /// (III.3.17).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brfalse.s</c> instruction.</remarks>
    member this.Brzero_s() = this.Brfalse_s()

    /// <summary>
    /// (0x2D) Writes the short form of an instruction that branches to the specified target if the <c>value</c> is true or
    /// non-zero (III.3.18).
    /// </summary>
    member this.Brtrue_s() = this.Branch(0x2Duy, true)
    /// <summary>
    /// (0x2D) Writes the short form of an instruction that branches to the specified target if the <c>value</c> is a non-null
    /// object reference (III.3.18).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brtrue.s</c> instruction.</remarks>
    member this.Brinst_s() = this.Brtrue_s()

    /// (0x2E)
    member this.Beq_s() = this.Branch(0x2Euy, true)

    /// <summary>
    /// (0x33) Writes the short form of an instruction that branches to the specified target if <c>value1</c> does not equal
    /// <c>value2</c> (III.3.14).
    /// </summary>
    member this.Bne_un_s() = this.Branch(0x33uy, true)

    /// <summary>
    /// (0x35) Writes the short form of an instruction that branches to the specified target if <c>value1 > value2</c> (III.3.9).
    /// </summary>
    member this.Bgt_un_s() = this.Branch(0x35uy, true)
    
    /// <summary>
    /// (0x39) Writes an instruction that branches to the specified target if the <c>value</c> is false or zero (III.3.17).
    /// </summary>
    member this.Brfalse() = this.Branch(0x39uy, false)
    /// <summary>
    /// (0x39) Writes an instruction that branches to the specified target if the <c>value</c> is <see langword="null"/>
    /// (III.3.17).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brfalse</c> instruction.</remarks>
    member this.Brnull() = this.Brfalse()
    /// <summary>
    /// (0x39) Writes an instruction that branches to the specified target if the <c>value</c> is zero (III.3.17).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brfalse</c> instruction.</remarks>
    member this.Brzero() = this.Brfalse()

    /// <summary>
    /// (0x3A) Writes an instruction that branches to the specified target if the <c>value</c> is true or non-zero (III.3.18).
    /// </summary>
    member this.Brtrue() = this.Branch(0x3Auy, false)
    /// <summary>
    /// (0x3A) Writes an instruction that branches to the specified target if the <c>value</c> is a non-null object reference
    /// (III.3.18).
    /// </summary>
    /// <remarks>This opcode is an alias for the <c>brtrue</c> instruction.</remarks>
    member this.Brinst() = this.Brtrue()

    /// (0x3B)
    member this.Beq() = this.Branch(0x3Buy, false)

    /// <summary>
    /// (0x42) Writes an instruction that branches to the specified target if <c>value1 > value2</c> (III.3.9).
    /// </summary>
    member this.Bgt_un() = this.Branch(0x42uy, false)

    /// <summary>
    /// (0x58) Writes an instruction that adds <c>value2</c> to <c>value1</c> and pushes the result onto the stack without an
    /// overflow check (III.3.1).
    /// </summary>
    member this.Add() = this.WriteU1 0x58uy

    // member this.Add_ovf

    /// <summary>
    /// (0x59) Writes an instruction that subtracts <c>value2</c> from <c>value1</c> and pushes the result onto the stack without
    /// an overflow check (III.3.64).
    /// </summary>
    member this.Sub() = this.WriteU1 0x59uy

    /// <summary>
    /// (0x5A) Writes an instruction that multiplies <c>value1</c> by <c>value2</c> and pushes the result onto the stack without
    /// an overflow check (III.3.48).
    /// </summary>
    member this.Mul() = this.WriteU1 0x5Auy

    // member this.Mul_ovf

    /// <summary>
    /// (0x5B) Writes an instruction that divides two values and pushes the result onto the stack without an overflow check
    /// (III.3.48).
    /// </summary>
    member this.Div() = this.WriteU1 0x5Buy

    // member this.Div_un

    /// <summary>
    /// (0x5D) Writes an instruction that divides <c>value1</c> by <c>value2</c> and pushes the remainder onto the stack
    /// (III.3.55).
    /// </summary>
    member this.Rem() = this.WriteU1 0x5Duy

    // member this.Rem_un
    
    /// <summary>
    /// (0x6B) Writes an instruction that converts the value on top of the stack to a <c>float32</c> without an overflow check
    /// (III.3.27).
    /// </summary>
    member this.Conv_r4() = this.WriteU1 0x6Buy

    /// <summary>
    /// (0x6C) Writes an instruction that converts the value on top of the stack to a <c>float64</c> without an overflow check
    /// (III.3.27).
    /// </summary>
    member this.Conv_r8() = this.WriteU1 0x6Cuy

    /// <summary>
    /// (0x6D) Writes an instruction that converts the value on top of the stack to a unsigned four-byte integer without an
    /// overflow check (III.3.27).
    /// </summary>
    member this.Conv_u4() = this.WriteU1 0x6Duy

    // member this.Conv_ovf
    // member this.Conv_ovf__un

    member private this.CallvirtMethodRef method = this.Call(0x6Fuy, method, 0xAuy)

    /// <summary>
    /// (0x6F) Writes an instruction that calls a late-bound method specified by a <c>MethodRef</c> with a <c>DEFAULT</c>
    /// calling convention (III.4.2).
    /// </summary>
    member this.Callvirt(method: RawIndex<MethodRefDefault>) = this.CallvirtMethodRef method

    /// <summary>
    /// (0x6F) Writes an instruction that calls a late-bound method specified by a <c>MethodRef</c> with a <c>GENERIC</c>
    /// calling convention (III.4.2).
    /// </summary>
    member this.Callvirt(method: RawIndex<MethodRefGeneric>) = this.CallvirtMethodRef method

    /// <summary>
    /// (0x6F) Writes an instruction that calls a late-bound method specified by a <c>MethodRef</c> with a <c>VARARG</c>
    /// calling convention (III.4.2).
    /// </summary>
    member this.Callvirt(method: RawIndex<MethodRefVarArg>) = this.CallvirtMethodRef method

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

    // TODO: Figure out of Newobj can accept a .ctor with a VARARG signature.
    member private this.Newobj(ctor, table) = this.Call(0x73uy, ctor, table)

    /// <summary>
    /// (0x73) Writes an instruction that allocates "an uninitialized object or value type" and calls the constructor specified
    /// by a <c>MemberRef</c> with a <c>DEFAULT</c> calling convention (III.4.21).
    /// </summary>
    member this.Newobj(ctor: RawIndex<MethodRefDefault>) = this.Newobj(ctor, 0xAuy)

    /// <summary>
    /// (0x73) Writes an instruction that allocates "an uninitialized object or value type" and calls the constructor method
    /// (III.4.21).
    /// </summary>
    member this.Newobj(ctor: RawIndex<ObjectConstructor>) = this.Newobj(ctor, 0x6uy)

    member private this.WriteFieldInstruction(opcode, field: RawIndex<_>, table) =
        this.WriteU1 opcode
        this.WriteMetadataToken(uint32 field, table)
    /// <summary>(0x7B) Writes an instruction that pushes the value of an object's field onto the stack (III.4.10).</summary>
    /// <param name="field">The field to load the value of.</param>
    member this.Ldfld(field: RawIndex<InstanceField>) = this.WriteFieldInstruction(0x7Buy, field, 0x4uy)

    /// <summary>
    /// (0x7B) Writes an instruction that pushes the value of an instance field specified by a <c>FieldRef</c> onto the stack
    /// (III.4.10).
    /// </summary>
    member this.Ldfld(field: RawIndex<FieldRef>) = this.WriteFieldInstruction(0x7Buy, field, 0xAuy)

    /// <summary>(0x7D) Writes an instruction that stores a value into an object's field (III.4.28).</summary>
    member this.Stfld(field: RawIndex<InstanceField>) = this.WriteFieldInstruction(0x7Duy, field, 0x4uy)

    /// <summary>
    /// (0x7D) Writes an instruction that stores a value into an instance field specified by a <c>FieldRef</c> (III.4.28).
    /// </summary>
    member this.Stfld(field: RawIndex<FieldRef>) = this.WriteFieldInstruction(0x7Duy, field, 0xAuy)

    // TODO: Allow a FieldRef to be used when loading and storing a static field.
    /// <summary>(0x7E) Writes an instruction that pushes the value of a static field onto the stack (III.4.14).</summary>
    /// <param name="field">The static field to load the value of.</param>
    member this.Ldsfld(field: RawIndex<StaticField>) = this.WriteFieldInstruction(0x7Euy, field, 0x4uy)

    /// <summary>(0x7D) Writes an instruction that stores a value into a static field (III.4.30).</summary>
    member this.Stsfld field = this.WriteFieldInstruction(0x80uy, field, 0x4uy)

    member private this.Newarr(index: RawIndex<_>, table) =
        this.WriteU1 0x8Duy
        this.WriteMetadataToken(uint32 index, table)

    /// <summary>
    /// (0x8D) Writes an instruction that creates a new one-dimensional array containing <c>numElem</c> elements of a type specified by a
    /// <c>TypeRef</c> token (III.4.20).
    /// </summary>
    member this.Newarr(etype: RawIndex<TypeRef>) = this.Newarr(etype, 0x1uy)
    /// <summary>
    /// (0x8D) Writes an instruction that creates a new one-dimensional array containing <c>numElem</c> elements of a type specified by a
    /// <c>TypeDef</c> token (III.4.20).
    /// </summary>
    member this.Newarr(etype: RawIndex<TypeDefRow>) = this.Newarr(etype, 0x2uy)
    /// <summary>
    /// (0x8D) Writes an instruction that creates a new one-dimensional array containing <c>numElem</c> elements of a type specified by a
    /// <c>TypeSpec</c> token (III.4.20).
    /// </summary>
    member this.Newarr(etype: RawIndex<TypeSpecRow>) = this.Newarr(etype, 0x1Buy)

    // TODO: Add checks to ensure that the next written instruction after tail. is ret
    member private this.Tail() = this.WriteU1 0xFEuy; this.WriteU1 0x14uy

    /// <summary>
    /// (0xFE 0x14 0x28) Writes an instruction that discards the current stack frame before calling a <c>MethodRef</c> with the
    /// <c>DEFAULT</c> calling convention (III.2.4)
    /// </summary>
    member this.Tail_call(method: RawIndex<MethodRefDefault>) = this.Tail(); this.Call method

    /// <summary>
    /// (0xFE 0x14 0x28) Writes an instruction that discards the current stack frame before calling a <c>MethodRef</c> with the
    /// <c>GENERIC</c> calling convention (III.2.4)
    /// </summary>
    member this.Tail_call(method: RawIndex<MethodRefGeneric>) = this.Tail(); this.Call method

    /// <summary>
    /// (0xFE 0x14 0x28) Writes an instruction that discards the current stack frame before calling a <c>MethodRef</c> with the
    /// <c>VARARG</c> calling convention (III.2.4)
    /// </summary>
    member this.Tail_call(method: RawIndex<MethodRefVarArg>) = this.Tail(); this.Call method

    /// <summary>
    /// (0xFE 0x14 0x28) Writes an instruction that discards the current stack frame before calling a <c>MethodDef</c> (III.2.4)
    /// </summary>
    member this.Tail_call(method: RawIndex<MethodDefRow>) = this.Tail(); this.Call method

    /// <summary>
    /// (0xFE 0x14 0x28) Writes an instruction that discards the current stack frame before calling a static method specified by
    /// a <c>MethodDef</c> (III.2.4)
    /// </summary>
    member this.Tail_call(method: RawIndex<StaticMethod>) = this.Tail(); this.Call method

    // member this.Tail_callvirt

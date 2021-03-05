﻿namespace FSharpIL.Metadata

open FSharpIL.Metadata.Heaps

// TODO: Come up with better name for this class.
[<Sealed>]
type internal MethodBodyContentImpl (writer, metadata, us: UserStringHeap) =
    inherit MethodBodyContent(writer)
    do if writer.Size > 0u then invalidArg "writer" "The method body writer must be a new instance"
    member _.Metadata: CliMetadata = metadata
    member _.UserString = us

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
[<System.Runtime.CompilerServices.IsByRefLike; Struct>]
type MethodBodyWriter internal (content: MethodBodyContentImpl) =
    new (content: MethodBodyContent) = MethodBodyWriter(content :?> MethodBodyContentImpl)

    member private _.WriteMetadataToken(index, table) = MetadataToken.write index table content.Writer

    /// Gets the number of bytes that have been written.
    member _.Size = content.Writer.Size
    /// (0x00) Writes an instruction that does nothing (III.3.51).
    member _.Nop() = content.Writer.WriteU1 0uy
    /// <summary>
    /// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
    /// tripped" (III.3.16).
    /// </summary>
    member _.Break() = content.Writer.WriteU1 1uy
    /// (0x2A) Writes an instruction used to return from the current method (III.3.56).
    member _.Ret() = content.Writer.WriteU1 0x2Auy
    /// (0x14) Writes an instruction used to load a null pointer (III.3.45).
    member _.Ldnull() = content.Writer.WriteU1 0x14uy

    // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    /// <summary>(0x28) Writes an instruction that calls a method (III.3.19).</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member this.Call(SimpleIndex method: MemberRefIndex<MethodRef>) =
        IndexOwner.checkIndex content.Metadata.Owner method
        content.Writer.WriteU1 0x28uy
        this.WriteMetadataToken(content.Metadata.MemberRef.IndexOf method, 0xAuy)

    /// <summary>
    /// (0x72) Writes an instruction that loads a string from the <c>#US</c> heap (III.4.16).
    /// </summary>
    /// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
    /// <exception cref="T:System.ArgumentNullException">Thrown when the input string is <see langword="null"/>.</exception>
    member _.Ldstr(str: string) =
        match str with
        | null -> nullArg "str"
        | _ ->
            content.Writer.WriteU1 0x72uy
            MetadataToken.userString str content.UserString content.Writer

    member private this.WriteFieldInstruction(opcode, SimpleIndex field: FieldIndex<_>) =
        content.Writer.WriteU1 opcode
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

[<AutoOpen>]
module MethodBodyContentExtensions =
    type MethodBodyContent with
        member this.CreateWriter() = MethodBodyWriter(this :?> MethodBodyContentImpl)

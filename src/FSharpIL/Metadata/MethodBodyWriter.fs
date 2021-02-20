namespace FSharpIL.Metadata

open FSharpIL.Metadata.Heaps

// TODO: Come up with better name for this class.
[<Sealed>]
type internal MethodBodyContentImpl (writer, metadata, us: UserStringHeap) =
    inherit MethodBodyContent(writer)
    member _.Metadata: CliMetadata = metadata
    member _.UserString = us

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
[<System.Runtime.CompilerServices.IsByRefLike; Struct>]
type MethodBodyWriter internal (content: MethodBodyContentImpl) =
    new (content: MethodBodyContent) = MethodBodyWriter(content :?> MethodBodyContentImpl)

    /// Writes an instruction that does nothing (III.3.51).
    member _.Nop() = content.Writer.WriteU1 0uy
    /// Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been tripped" (III.3.16).
    member _.Break() = content.Writer.WriteU1 1uy
    /// Writes an instruction used to return from the current method (III.3.56).
    member _.Ret() = content.Writer.WriteU1 0x2Auy
    /// Writes an instruction used to load a null pointer (III.3.45).
    member _.Ldnull() = content.Writer.WriteU1 0x14uy

    // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    /// <summary>Writes an instruction that calls a method (III.3.19).</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member _.Call(SimpleIndex method: MemberRefIndex<MethodRef>) =
        IndexOwner.checkIndex content.Metadata.Owner method
        content.Writer.WriteU1 0x28uy
        MetadataToken.write (content.Metadata.MemberRef.IndexOf method) 0xAuy content.Writer

    /// <summary>
    /// Writes an instruction that loads a string from the <c>#Strings</c> heap (III.4.16).
    /// </summary>
    /// <remarks>
    /// If the string to load is <see langword="null"/>, then generates the <c>ldnull</c> opcode.
    /// </remarks>
    member this.Ldstr(str: string) =
        match str with
        | null -> this.Ldnull()
        | _ -> MetadataToken.userString str content.UserString content.Writer

[<AutoOpen>]
module MethodBodyContentExtensions =
    type MethodBodyContent with
        member this.CreateWriter() = MethodBodyWriter(this :?> MethodBodyContentImpl)

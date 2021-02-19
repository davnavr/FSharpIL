[<AutoOpen>]
module FSharpIL.Metadata.MethodBodyWriterExtensions // TODO: Move this file below Heaps folder to allow it to use ChunkWriterExtensions

// TODO: Figure out how exception handling information will be included.
// TODO: Figure out how to prevent (some) invalid method bodies.
type MethodBodyWriter with
    // TODO: Allow call to accept a MethodDef, MethodRef, or MethodSpec.
    /// <summary>Writes an instruction that calls a method (III.3.19).</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member this.Call(method: MemberRefIndex<MethodRef>) =
        IndexOwner.checkIndex this.Owner method.Index
        this.Writer.WriteU1 0x28uy
        failwith "TODO: Write token"

    /// <summary>
    /// Writes an instruction that loads a string from the <c>#Strings</c> heap (III.4.16).
    /// </summary>
    /// <remarks>
    /// If the string to load is <see langword="null"/>, then generates the <c>ldnull</c> opcode.
    /// </remarks>
    member this.Ldstr(str: string) =
        match str with
        | null -> this.Ldnull()
        | _ -> failwith "TODO: Write string index"

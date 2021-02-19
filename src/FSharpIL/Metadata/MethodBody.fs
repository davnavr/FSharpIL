namespace FSharpIL.Metadata

open FSharpIL.Writing

[<System.Runtime.CompilerServices.IsByRefLike; Struct>]
type MethodBodyWriter internal (owner: IndexOwner, writer: ChunkWriter) =
    member internal _.Owner = owner
    member internal _.Writer = writer

    // Basic instructions that take no arguments

    /// Writes an instruction that does nothing (III.3.51).
    member _.Nop() = writer.WriteU1 0uy
    /// Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been tripped" (III.3.16).
    member _.Break() = writer.WriteU1 1uy
    /// Writes an instruction used to return from the current method (III.3.56).
    member _.Ret() = writer.WriteU1 0x2Auy
    /// Writes an instruction used to load a null pointer (III.3.45).
    member _.Ldnull() = writer.WriteU1 0x14uy

type IMethodBody =
    /// <summary>Writes the CLI opcodes that make up the body of the method.</summary>
    /// <returns><c>Some</c> if a method body was written; or <c>None</c> if the method has no body.</returns>
    abstract WriteBody: MethodBodyWriter -> unit option

[<RequireQualifiedAccess>]
module MethodBody =
    /// <summary>Represents a method whose <c>RVA</c> column has a value of zero, indicating that it does not have a body.</summary>
    let none = { new IMethodBody with member _.WriteBody _ = None }

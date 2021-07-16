/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module FSharpIL.Writing.Cil.MethodBodyInstructions

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

module Unsafe =
    val getInstructionStream: byref<MethodBodyBuilder> -> byref<ChunkedMemoryBuilder>

    /// <summary>Increments the estimated <c>MaxStack</c> value for the method body.</summary>
    val incrMaxStack: byref<MethodBodyBuilder> -> unit

    val writeRawOpcode: byref<MethodBodyBuilder> -> opcode: Opcode -> unit
    
    /// Writes an opcode that pushes a single item onto the stack.
    val inline writePushingOpcode: byref<MethodBodyBuilder> -> opcode: Opcode -> unit

    /// <summary>Writes a metadata token (III.1.9).</summary>
    val writeMetadataToken: byref<MethodBodyBuilder> -> token: MetadataToken -> unit

    val writeBranchInstruction: short: Opcode -> long: Opcode -> byref<MethodBodyBuilder> -> byref<BranchTarget>

    val writeCallInstruction:
        byref<MethodBodyBuilder> ->
        opcode: Opcode ->
        method: MethodMetadataToken ->
        hasRetValue: bool ->
        unit

    val writeFieldInstruction:
        byref<MethodBodyBuilder> ->
        opcode: Opcode ->
        field: FieldMetadataToken ->
        pushesFieldValue: bool ->
        unit

    val writeTypeInstruction:
        byref<MethodBodyBuilder> ->
        opcode: Opcode ->
        typeTok: TypeMetadataToken ->
        unit

[<RequireQualifiedAccess>]
module Branch =
    /// Creates a destination for a branch instruction. The branch instruction will jump to the last byte that was written.
    val inline createLabel: inref<MethodBodyBuilder> -> Label
    /// Sets the target of a branch instruction.
    val setTarget: branch: byref<BranchTarget> -> destination: Label -> unit

/// Contains functions for generating instructions that call methods specified by a metadata token.
module Call =
    /// <summary>(0x28) Writes an instruction that calls the specified <paramref name="method"/> (III.3.19).</summary>
    /// <param name="method">The method to call, either a <c>MethodDef</c>, <c>MemberRef</c>, or <c>MethodSpec</c>.</param>
    /// <param name="hasRetValue">
    /// Indicates whether or not calling the method would push a return value onto the stack, and updates the estimated
    /// <c>MaxStack</c> of the method accordingly.
    /// </param>
    /// <param name="stream">The method body stream that the <c>call</c> instruction is written to.</param>
    val call: stream: byref<MethodBodyBuilder> -> method: MethodMetadataToken -> hasRetValue: bool -> unit

    /// <summary>(0x6F) Writes an instruction that calls the <paramref name="method"/> associated with an object (III.4.2).</summary>
    val callvirt: stream: byref<MethodBodyBuilder> -> method: MethodMetadataToken -> hasRetValue: bool -> unit

// TODO: Have module for versions of field instructions that take a FieldMetadataToken instead.

// TODO: Have module for versions of instrucions like `newarr` that take a TypeMetadataToken instead.

/// (0x00) Writes an instruction that does nothing (III.3.51).
val inline nop: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
/// tripped" (III.3.16).
/// </summary>
val inline ``break``: byref<MethodBodyBuilder> -> unit

/// (0x02) Writes an instruction that loads argument #0 for static methods or the "this" pointer for instance methods onto the
/// stack (III.3.38).
val inline ldarg_0: byref<MethodBodyBuilder> -> unit

/// (0x03) Writes an instruction that loads argument #1 onto the stack (III.3.38).
val inline ldarg_1: byref<MethodBodyBuilder> -> unit

/// (0x04) Writes an instruction that loads argument #2 onto the stack (III.3.38).
val inline ldarg_2: byref<MethodBodyBuilder> -> unit

/// (0x05) Writes an instruction that loads argument #3 onto the stack (III.3.38).
val inline ldarg_3: byref<MethodBodyBuilder> -> unit



/// <summary>
/// (0x0E) Writes the short form of an instruction that loads the specified argument onto the stack (III.3.38).
/// </summary>
/// <param name="stream">The method body stream that the <c>ldarg.s</c> instruction is written to.</param>
/// <param name="num">The index of the argument to load onto the stack.</param>
val inline ldarg_s: stream: byref<MethodBodyBuilder> -> num: uint8 -> unit



/// (0x26) Writes an instruction that pops the value at the top of the stack (III.3.54).
val inline pop: byref<MethodBodyBuilder> -> unit



/// (0x28) Writes an instruction that calls the specified method (III.3.19).
val call: byref<MethodBodyBuilder> -> method: FSharpIL.Cli.MethodCallTarget -> MethodTokenSource -> unit



/// (0x2A) Writes an instruction used to return from the current method (III.3.56).
val inline ret: byref<MethodBodyBuilder> -> unit



/// <summary>(0x6F) Writes an instruction that calls a method associated with an object (III.4.2).</summary>
val callvirt: byref<MethodBodyBuilder> -> method: FSharpIL.Cli.MethodCallTarget -> MethodTokenSource -> unit



/// <summary>
/// (0x72) Writes an instruction that loads a literal string from the <c>#US</c> heap (III.4.16).
/// </summary>
/// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
val ldstr: byref<MethodBodyBuilder> -> offset: UserStringOffset -> unit



/// (0x7B) Writes an instruction that pops a member reference off of the stack and pushes the value of an instance field onto the
/// stack (III.4.10).
// TODO: Instead take a FieldDefinition<FieldKinds.Instance> or FieldReference<FieldKinds.Instance>
val ldfld: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit

/// (0x7C) Writes an instruction that pops a member reference off of the stack and pushes an unmanaged pointer to an instance
/// field onto the stack (III.4.11).
val ldflda: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit

/// (0x7D) Writes an instruction that pops a member reference and a value off of the stack, storing the value into an instance
/// field (III.4.28).
val stfld: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit

/// (0x7E) Writes an instruction that loads the value of a static field onto the stack (III.4.14).
val ldsfld: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit

/// (0x7F) Writes an instruction that pushes an unmanaged pointer to a static field onto the stack (III.4.15).
val ldsflda: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit

/// (0x80) Writes an instruction that pops a value off of the stack and stores it into a static field (III.4.30).
val stsfld: byref<MethodBodyBuilder> -> field: FSharpIL.Cli.FieldArg -> FieldTokenSource -> unit



/// (0x8D) Writes an instruction that pops an integer length off of the stack and creates "a zero-based, one-dimensional array"
/// of the specified type (III.4.20).
val newarr: byref<MethodBodyBuilder> -> etype: FSharpIL.Cli.NamedType -> TypeTokenSource -> unit



/// (0xFE 0x09) Writes the long form of an instruction that loads the specified argument onto the stack (III.3.38).
val inline ldarg: byref<MethodBodyBuilder> -> num: uint16 -> unit

//val inline instruction: byref<MethodBodyBuilder> -> unit
//val writeCallInstruction: byref<MethodBodyBuilder> -> unit

/// Contains functions used to write the most shortened from of CIL opcodes whenever possible.
module Shortened =
    /// <summary>
    /// (0x02 to 0x05, 0x0E, 0xFE 0x09) Writes the shortest form of an instruction that loads an argument onto the stack
    /// (III.3.38).
    /// </summary>
    val ldarg: byref<MethodBodyBuilder> -> num: uint16 -> unit

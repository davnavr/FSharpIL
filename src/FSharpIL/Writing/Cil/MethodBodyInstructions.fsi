/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module FSharpIL.Writing.Cil.MethodBodyInstructions

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

open FSharpIL.Cli

module Unsafe =
    val getInstructionStream : byref<MethodBodyBuilder> -> byref<ChunkedMemoryBuilder>

    /// <summary>Increments the estimated <c>MaxStack</c> value for the method body.</summary>
    val incrMaxStack : stream: byref<MethodBodyBuilder> -> unit

    val writeRawOpcode : stream: byref<MethodBodyBuilder> -> opcode: Opcode -> unit

    /// Writes an opcode that pushes a single item onto the stack.
    val inline writePushingOpcode : stream: byref<MethodBodyBuilder> -> opcode: Opcode -> unit

    /// <summary>Writes a metadata token (III.1.9).</summary>
    val inline writeMetadataToken : stream: byref<MethodBodyBuilder> -> token: MetadataToken -> unit

    /// <summary>Writes a metadata token representing a byte offset into the <c>#US</c> heap (III.1.9).</summary>
    val writeStringOffset : stream: byref<MethodBodyBuilder> -> token: UserStringOffset -> unit

    val writeBranchInstruction : short: Opcode -> long: Opcode -> stream: byref<MethodBodyBuilder> -> byref<BranchTarget>

    val inline writeStringToken : stream: byref<MethodBodyBuilder> -> string -> tokens: MetadataTokenSource -> unit

    val inline writeMethodToken : stream: byref<MethodBodyBuilder> -> method: MethodTok -> tokens: MetadataTokenSource -> unit

    val inline writeFieldToken : stream: byref<MethodBodyBuilder> -> field: FieldTok -> tokens: MetadataTokenSource -> unit

    val inline writeTypeToken : stream: byref<MethodBodyBuilder> -> TypeTok -> tokens: MetadataTokenSource -> unit

    val inline writeFieldInstruction :
        stream: byref<MethodBodyBuilder> ->
        opcode: Opcode ->
        pushesFieldValue: bool ->
        field: FieldTok ->
        tokens: MetadataTokenSource ->
        unit

    /// <summary>
    /// Writes an instruction that takes a metadata token pointing to the <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c>
    /// table, and increments the estimated <c>MaxStack</c> value.
    /// </summary>
    val inline writeTypeInstruction :
        stream: byref<MethodBodyBuilder> ->
        opcode: Opcode ->
        TypeTok ->
        tokens: MetadataTokenSource ->
        unit

[<RequireQualifiedAccess>]
module Branch =
    /// Creates a destination for a branch instruction, control will jump to the next instruction after the label.
    val inline createLabel : stream: inref<MethodBodyBuilder> -> Label
    /// Sets the target of a branch instruction.
    val setTarget : branch: byref<BranchTarget> -> destination: Label -> unit

/// Contains functions for generating instructions that call methods specified by a metadata token.
module Call =
    /// <summary>(0x28) Writes an instruction that calls the specified <paramref name="method"/> (III.3.19).</summary>
    /// <param name="method">The method to call, either a <c>MethodDef</c>, <c>MemberRef</c>, or <c>MethodSpec</c>.</param>
    /// <param name="hasRetValue">
    /// Indicates whether or not calling the method would push a return value onto the stack, and updates the estimated
    /// <c>MaxStack</c> of the method accordingly.
    /// </param>
    /// <param name="stream">The method body stream that the <c>call</c> instruction is written to.</param>
    val call : stream: byref<MethodBodyBuilder> -> method: MethodMetadataToken -> hasRetValue: bool -> unit

    /// <summary>(0x6F) Writes an instruction that calls the <paramref name="method"/> associated with an object (III.4.2).</summary>
    val callvirt : stream: byref<MethodBodyBuilder> -> method: MethodMetadataToken -> hasRetValue: bool -> unit

/// (0x00) Writes an instruction that does nothing (III.3.51).
val inline nop : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
/// tripped" (III.3.16).
/// </summary>
val inline ``break`` : stream: byref<MethodBodyBuilder> -> unit

/// (0x02) Writes an instruction that loads argument #0 for static methods or the "this" pointer for instance methods onto the
/// stack (III.3.38).
val inline ldarg_0 : stream: byref<MethodBodyBuilder> -> unit

/// (0x03) Writes an instruction that loads argument #1 onto the stack (III.3.38).
val inline ldarg_1 : stream: byref<MethodBodyBuilder> -> unit

/// (0x04) Writes an instruction that loads argument #2 onto the stack (III.3.38).
val inline ldarg_2 : stream: byref<MethodBodyBuilder> -> unit

/// (0x05) Writes an instruction that loads argument #3 onto the stack (III.3.38).
val inline ldarg_3 : stream: byref<MethodBodyBuilder> -> unit

/// (0x06) Writes an instruction that loads local variable #0 onto the stack (III.3.43).
val inline ldloc_0 : stream: byref<MethodBodyBuilder> -> unit

/// (0x07) Writes an instruction that loads local variable #1 onto the stack (III.3.43).
val inline ldloc_1 : stream: byref<MethodBodyBuilder> -> unit

/// (0x08) Writes an instruction that loads local variable #2 onto the stack (III.3.43).
val inline ldloc_2 : stream: byref<MethodBodyBuilder> -> unit

/// (0x09) Writes an instruction that loads local variable #3 onto the stack (III.3.43).
val inline ldloc_3 : stream: byref<MethodBodyBuilder> -> unit

/// (0x0A) Writes an instruction that pops a value from the stack and stores it into local variable #0
/// (III.3.63).
val inline stloc_0 : stream: byref<MethodBodyBuilder> -> unit

/// (0x0B) Writes an instruction that pops a value from the stack and stores it into local variable #1
/// (III.3.63).
val inline stloc_1 : stream: byref<MethodBodyBuilder> -> unit

/// (0x0C) Writes an instruction that pops a value from the stack and stores it into local variable #2
/// (III.3.63).
val inline stloc_2 : stream: byref<MethodBodyBuilder> -> unit

/// (0x0D) Writes an instruction that pops a value from the stack and stores it into local variable #3
/// (III.3.63).
val inline stloc_3 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x0E) Writes the short form of an instruction that loads the specified argument onto the stack (III.3.38).
/// </summary>
/// <param name="stream">The method body stream that the <c>ldarg.s</c> instruction is written to.</param>
/// <param name="num">The index of the argument to load onto the stack.</param>
val inline ldarg_s : stream: byref<MethodBodyBuilder> -> num: uint8 -> unit



/// <summary>
/// (0x11) Writes the short form of an instruction that loads the local variable at the specified <paramref name="index"/> onto
/// the stack (III.3.43).
/// </summary>
/// <param name="stream">The method body stream that the <c>stloc.s</c> instruction is written to.</param>
/// <param name="index">The index of the local variable that is loaded onto the stack.</param>
val inline ldloc_s : stream: byref<MethodBodyBuilder> -> index: uint8 -> unit



/// <summary>
/// (0x13) Writes the short form of an instruction that pops a value from the stack and stores it into local variable at the
/// specified <paramref name="index"/> (III.3.63).
/// </summary>
/// <param name="stream">The method body stream that the <c>stloc.s</c> instruction is written to.</param>
/// <param name="index">The index of the local variable that the value popped off of the stack is stored into.</param>
val inline stloc_s : stream: byref<MethodBodyBuilder> -> index: uint8 -> unit

//val inline ldnull : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x15) Writes the short form of an instruction that pushes the integer <c>-1</c> onto the stack as an <c>int32</c>
/// (III.3.40).
/// </summary>
val inline ldc_i4_m1 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x16) Writes the short form of an instruction that pushes the integer <c>0</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_0 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x17) Writes the short form of an instruction that pushes the integer <c>1</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_1 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x18) Writes the short form of an instruction that pushes the integer <c>2</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_2 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x19) Writes the short form of an instruction that pushes the integer <c>3</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_3 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1A) Writes the short form of an instruction that pushes the integer <c>4</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_4 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1B) Writes the short form of an instruction that pushes the integer <c>5</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_5 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1C) Writes the short form of an instruction that pushes the integer <c>6</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_6 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1D) Writes the short form of an instruction that pushes the integer <c>7</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_7 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1E) Writes the short form of an instruction that pushes the integer <c>8</c> onto the stack as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_8 : stream: byref<MethodBodyBuilder> -> unit

/// <summary>
/// (0x1F) Writes the short form of an instruction that pushes the signed byte constant <paramref name="number"/> onto the stack
/// as an <c>int32</c> (III.3.40).
/// </summary>
val inline ldc_i4_s : stream: byref<MethodBodyBuilder> -> number: int8 -> unit



/// (0x26) Writes an instruction that pops the value at the top of the stack (III.3.54).
val inline pop : stream: byref<MethodBodyBuilder> -> unit



/// (0x28) Writes an instruction that calls the specified method (III.3.19).
val call : stream: byref<MethodBodyBuilder> -> method: MethodTok -> tokens: MetadataTokenSource -> unit



/// (0x2A) Writes an instruction used to return from the current method (III.3.56).
val inline ret : stream: byref<MethodBodyBuilder> -> unit



/// <summary>
/// (0x69) Writes an instruction that converts the value on top of the stack into an <c>int32</c> without an overflow check
/// (III.3.27).
/// </summary>
val inline conv_i4 : stream: byref<MethodBodyBuilder> -> unit



/// <summary>(0x6F) Writes an instruction that calls a method associated with an object (III.4.2).</summary>
val callvirt : stream: byref<MethodBodyBuilder> -> method: MethodTok -> tokens: MetadataTokenSource -> unit // TODO: Take a non-static method here.



/// <summary>
/// (0x72) Writes an instruction that pushes a string literal onto the stack (III.4.16).
/// </summary>
val inline ldstr : stream: byref<MethodBodyBuilder> -> string -> tokens: MetadataTokenSource -> unit

/// <summary>
/// Contains functions for generating the <c>ldstr</c> (0x72) instruction, which loads a literal string from the <c>#US</c> heap
/// (III.4.16).
/// </summary>
/// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
[<RequireQualifiedAccess>]
module Ldstr =
    /// (0x72) Writes an instruction that loads a literal string at the specified offset from the <c>#US</c> heap (III.4.16).
    val inline ofOffset : stream: byref<MethodBodyBuilder> -> offset: UserStringOffset -> unit

    val inline ofString : stream: byref<MethodBodyBuilder> -> string -> tokens: MetadataTokenSource -> unit

    val inline ofMemory :
        stream: byref<MethodBodyBuilder> ->
        inref<System.ReadOnlyMemory<char>> ->
        tokens: MetadataTokenSource ->
        unit

/// <summary>
/// Contains functions for generating the <c>newobj</c> (0x73) instruction, which "creates a new object or a new instance of a
/// value type" (III.4.21).
/// </summary>
/// <remarks>For value types, the <c>initobj</c> is usually used instead.</remarks>
[<RequireQualifiedAccess>]
module Newobj =
    val inline ofToken : stream: byref<MethodBodyBuilder> -> ctor: MethodMetadataToken -> unit

    val inline ofMethod : stream: byref<MethodBodyBuilder> -> ctor: MethodTok -> tokens: MetadataTokenSource -> unit

    val inline ofDefinedMethod :
        stream: byref<MethodBodyBuilder> ->
        ctor: MethodTok<TypeDefinition<'Kind>, MethodDefinition<MethodKinds.ObjectConstructor>> ->
        tokens: MetadataTokenSource ->
        unit when 'Kind :> TypeKinds.IHasConstructors



/// (0x7B) Writes an instruction that pops a member reference off of the stack and pushes the value of an instance field onto the
/// stack (III.4.10).
// TODO: Instead take a FieldDefinition<FieldKinds.Instance> or FieldReference<FieldKinds.Instance>
val inline ldfld : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit

/// (0x7C) Writes an instruction that pops a member reference off of the stack and pushes an unmanaged pointer to an instance
/// field onto the stack (III.4.11).
val inline ldflda : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit

/// (0x7D) Writes an instruction that pops a member reference and a value off of the stack, storing the value into an instance
/// field (III.4.28).
val inline stfld : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit // TODO: How to accept both DefinedField and ReferencedField that are static only?

/// (0x7E) Writes an instruction that loads the value of a static field onto the stack (III.4.14).
val inline ldsfld : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit

/// (0x7F) Writes an instruction that pushes an unmanaged pointer to a static field onto the stack (III.4.15).
val inline ldsflda : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit // TODO: How to accept both DefinedField and ReferencedField that are static only?

/// (0x80) Writes an instruction that pops a value off of the stack and stores it into a static field (III.4.30).
val inline stsfld : stream: byref<MethodBodyBuilder> -> FieldTok -> tokens: MetadataTokenSource -> unit // TODO: How to accept both DefinedField and ReferencedField that are static only?



/// (0x8D) Writes an instruction that pops a signed native or 32-bit integer <c>length</c> off of the stack and creates "a zero-based,
/// one-dimensional array" of the specified type (III.4.20).
val inline newarr : stream: byref<MethodBodyBuilder> -> etype: TypeTok -> tokens: MetadataTokenSource -> unit

/// (0x8E) Writes an instruction that pops an object reference to an array off of the stack and pushes the length of the array
/// onto the stack as an unsigned native integer (III.4.12).
val inline ldlen : stream: byref<MethodBodyBuilder> -> unit



/// (0xFE 0x09) Writes the long form of an instruction that loads the specified argument onto the stack (III.3.38).
val inline ldarg : stream: byref<MethodBodyBuilder> -> num: uint16 -> unit



/// <summary>
/// (0xFE 0x0C) Writes the long form of an instruction that loads the local variable at the specified <paramref name="index"/>
/// onto the stack (III.3.43).
/// </summary>
val inline ldloc : stream: byref<MethodBodyBuilder> -> index: LocalVarIndex -> unit



/// <summary>
/// (0xFE 0x0E) Writes the long form of an instruction that pops a value from the stack and stores it into the local variable at
/// the specified <paramref name="index"/> (III.3.63).
/// </summary>
val inline stloc : stream: byref<MethodBodyBuilder> -> index: LocalVarIndex -> unit

/// Contains functions used to write the most shortened from of CIL opcodes whenever possible.
module Shortened =
    /// (0x02 to 0x05, 0x0E, 0xFE 0x09) Writes the shortest form of an instruction that loads the specified argument onto the
    /// stack (III.3.38).
    val ldarg : stream: byref<MethodBodyBuilder> -> num: uint16 -> unit

    /// <summary>
    /// (0x06 to 0x09, 0x11, 0xFE 0x0C) Writes the shortest form of an instruction that loads the local variable at the specified
    /// <paramref name="index"/> onto the stack (III.3.43).
    /// </summary>
    val ldloc : stream: byref<MethodBodyBuilder> -> index: LocalVarIndex -> unit

    /// <summary>
    /// (0x0A to 0x0D, 0x13, 0xFE 0x0E) Writes the shortest from of an instruction that pops a value from the stack and stores it
    /// into the local variable at the specified <paramref name="index"/> (III.3.63).
    /// </summary>
    val stloc : stream: byref<MethodBodyBuilder> -> index: LocalVarIndex -> unit

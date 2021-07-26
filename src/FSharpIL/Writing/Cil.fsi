/// Contains functions and types used for writing Common Intermediate Language method bodies (II.25.4 and III).
module FSharpIL.Writing.Cil

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli.MemberTok
open FSharpIL.Cli.TypeSystem

[<IsReadOnly; Struct; NoComparison; NoEquality; RequireQualifiedAccess>]
type LocalVariables =
    /// <summary>
    /// An index into the <c>StandaloneSig</c> (0x11) table describing the types of the local variables of the method.
    /// </summary>
    | Token of index: TableIndex<StandaloneSigRow>
    | Locals of ImmutableArray<LocalType>

    member IsNull: bool

[<RequireQualifiedAccess>]
module LocalVariables =
    val Null : LocalVariables

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodHeader = // TODO: Make this type generic so separate type isn't needed for method bodies that use local variable types in FSharpIL.Cli
    { InitLocals: InitLocals
      MaxStack: MaxStack voption
      LocalVariables: LocalVariables }

[<Sealed>]
type Label = interface IEquatable<Label>

[<IsReadOnly; Struct; NoComparison; NoEquality; RequireQualifiedAccessAttribute>]
type BranchKind =
    /// The target of the branch instruction is always represented as a 1-byte integer.
    | Short
    /// The target of the bracnh instruction is always represented as a 4-byte integer.
    | Long

[<NoComparison; NoEquality; RequireQualifiedAccess>]
type Operand

type Operand with
    override ToString: unit -> string

[<IsReadOnly; Struct; NoComparison; NoEquality; RequireQualifiedAccess>]
type StackBehavior =
    | PopOrPush of int8

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Instruction =
    { Opcode: Opcode
      Operand: Operand
      StackBehavior: StackBehavior }

    override ToString: unit -> string

[<NoComparison; NoEquality>]
type InstructionBlock

[<RequireQualifiedAccess>]
module InstructionBlock =
    val ofList : instructions: Instruction list -> InstructionBlock
    val ofBlock : instructions: System.Collections.Immutable.ImmutableArray<Instruction> -> InstructionBlock
    val ofSeq : instructions: seq<Instruction> -> InstructionBlock

    /// <summary>
    /// Creates a new label, any branch instruction that jumps to this label will execute the instructions following the label
    /// (II.5.4).
    /// </summary>
    /// <param name="next">The instructions to execute after jumping to the label.</param>
    val label : next: InstructionBlock -> struct(Label * InstructionBlock)

[<NoComparison; ReferenceEquality>]
type MethodBody =
    { Header: MethodHeader
      Instructions: seq<InstructionBlock> }

[<RequireQualifiedAccess>]
module MethodBody =
    [<Literal>] val internal MaxTinyBodySize: uint32 = 63u
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    [<Literal>] val internal FatFormatSize: uint32 = 3u
    [<Literal>] val internal FatMethodAlignment: uint32 = 4u
    [<Literal>] val internal MaxTinyStackCount: uint16 = 8us

    val inline create : InitLocals -> MaxStack voption -> LocalVariables -> instructions: seq<InstructionBlock> -> MethodBody

    /// Creates a simple method body from a sequence of instructions.
    val inline ofSeq : instructions: seq<Instruction> -> MethodBody

[<Interface>]
type IMetadataTokenSource =
    abstract GetLocalVariables: ImmutableArray<FSharpIL.Cli.TypeSystem.LocalType> -> TableIndex<StandaloneSigRow>
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: TypeTok -> TypeMetadataToken

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyStream =
    internal new: unit -> MethodBodyStream

    member Add: body: MethodBody -> struct(MaxStack * uint32)

    member ToMemory:
        metadataTokenSource: IMetadataTokenSource -> FSharpIL.ChunkedMemory * IReadOnlyDictionary<MethodBody, MethodBodyLocation>

[<AutoOpen>]
module Instructions =
    [<RequireQualifiedAccess>]
    module Instruction =
        /// Creates an instruction that takes no arguments and pushes values to or pops values from the stack.
        val simple : opcode: Opcode -> StackBehavior -> Instruction

        /// Creates an instruction that takes no arguments and does not modify the stack.
        val inline op : opcode: Opcode -> Instruction

        /// Creates an instruction that takes no arguments and pushes one value onto the stack.
        val inline pushes1 : opcode: Opcode -> Instruction

        /// Creates an instruction that takes no arguments and pops one value from the stack.
        val inline pops1 : opcode: Opcode -> Instruction

        /// Creates an instruction that takes no arguments and pops two values from the stack.
        val inline pops2 : opcode: Opcode -> Instruction

    /// (0x00) An instruction that does nothing (III.3.51).
    val nop : Instruction

    /// (0x01) An instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been tripped"
    /// (III.3.16).
    val ``break`` : Instruction

    /// (0x02) Loads argument #0 for static methods or the "this" pointer for instance methods onto the stack (III.3.38).
    val ldarg_0 : Instruction

    /// (0x03) Loads argument #1 onto the stack (III.3.38).
    val ldarg_1 : Instruction

    /// (0x04) Loads argument #2 onto the stack (III.3.38).
    val ldarg_2 : Instruction

    /// (0x05) Loads argument #3 onto the stack (III.3.38).
    val ldarg_3 : Instruction

    /// (0x06) Loads local variable #0 onto the stack (III.3.43).
    val ldloc_0 : Instruction

    /// (0x07) Loads local variable #1 onto the stack (III.3.43).
    val ldloc_1 : Instruction

    /// (0x08) Loads local variable #2 onto the stack (III.3.43).
    val ldloc_2 : Instruction

    /// (0x09) Loads local variable #3 onto the stack (III.3.43).
    val ldloc_3 : Instruction

    /// (0x0A) Pops a value from the stack and stores it into local variable #0 (III.3.63).
    val stloc_0 : Instruction

    /// (0x0B) Pops a value from the stack and stores it into local variable #1 (III.3.63).
    val stloc_1 : Instruction

    /// (0x0C) Pops a value from the stack and stores it into local variable #2 (III.3.63).
    val stloc_2 : Instruction

    /// (0x0D) Pops a value from the stack and stores it into local variable #3 (III.3.63).
    val stloc_3 : Instruction

    /// <summary>
    /// (0x0E) Loads the specified argument onto the stack (III.3.38).
    /// </summary>
    /// <param name="num">The index of the argument to load onto the stack.</param>
    val ldarg_s : num: uint8 -> Instruction



    /// <summary>
    /// (0x11) Short form of an instruction that loads the local variable at the specified <paramref name="index"/> onto the
    /// stack (III.3.43).
    /// </summary>
    /// <param name="index">The index of the local variable that is loaded onto the stack.</param>
    val ldloc_s : index: uint8 -> Instruction



    /// <summary>
    /// (0x13) Short form of an instruction that pops a value from the stack and stores it into local variable at the specified
    /// <paramref name="index"/> (III.3.63).
    /// </summary>
    /// <param name="index">The index of the local variable that the value popped off of the stack is stored into.</param>
    val stloc_s : index: uint8 -> Instruction

    /// <summary>(0x14) Pushes a <see langword="null"/> reference onto the stack.</summary>
    val ldnull : Instruction

    /// <summary>
    /// (0x15) Short form of an instruction that pushes the integer <c>-1</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_m1 : Instruction

    /// <summary>
    /// (0x16) Short form of an instruction that pushes the integer <c>0</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_0 : Instruction

    /// <summary>
    /// (0x17) Short form of an instruction that pushes the integer <c>1</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_1 : Instruction

    /// <summary>
    /// (0x18) Short form of an instruction that pushes the integer <c>2</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_2 : Instruction

    /// <summary>
    /// (0x19) Short form of an instruction that pushes the integer <c>3</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_3 : Instruction

    /// <summary>
    /// (0x1A) Short form of an instruction that pushes the integer <c>4</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_4 : Instruction

    /// <summary>
    /// (0x1B) Short form of an instruction that pushes the integer <c>5</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_5 : Instruction

    /// <summary>
    /// (0x1C) Short form of an instruction that pushes the integer <c>6</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_6 : Instruction

    /// <summary>
    /// (0x1D) Short form of an instruction that pushes the integer <c>7</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_7 : Instruction

    /// <summary>
    /// (0x1E) Short form of an instruction that pushes the integer <c>8</c> onto the stack as an <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_8 : Instruction

    /// <summary>
    /// (0x1F) Short form of an instruction that pushes the signed byte constant <paramref name="number"/> onto the stack as an
    /// <c>int32</c> (III.3.40).
    /// </summary>
    val ldc_i4_s : number: int8 -> Instruction



    /// (0x26) Pops the value at the top of the stack (III.3.54).
    val pop : Instruction



    /// (0x28) Calls the specified method (III.3.19).
    val call : method: MethodTok -> Instruction // TODO: Make a static class for Call and Callvirt instructions, and provide call and callvirt in Unsafe module that accepts a MethodTok.



    /// (0x2A) Returns from the current method (III.3.56).
    val ret : Instruction



    /// <summary>
    /// (0x69) Converts the value on top of the stack into an <c>int32</c> without an overflow check (III.3.27).
    /// </summary>
    val conv_i4 : Instruction



    /// <summary>(0x6F) Calls a method associated with an object (III.4.2).</summary>
    val callvirt : method: MethodTok -> Instruction // TODO: Take a non-static method here.



    /// <summary>
    /// Contains functions for generating the <c>ldstr</c> (0x72) instruction, which loads a literal string from the <c>#US</c> heap
    /// (III.4.16).
    /// </summary>
    /// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
    [<RequireQualifiedAccess>]
    module Ldstr =
        /// (0x72) Writes an instruction that loads a literal string at the specified offset from the <c>#US</c> heap (III.4.16).
        val ofOffset : offset: UserStringOffset -> Instruction

        val ofString : string -> Instruction

        val ofMemory : inref<System.ReadOnlyMemory<char>> -> Instruction

    /// <summary>
    /// (0x72) Pushes a string literal onto the stack (III.4.16).
    /// </summary>
    val inline ldstr : string -> Instruction

    /// <summary>
    /// Contains functions for generating the <c>newobj</c> (0x73) instruction, which "creates a new object or a new instance of a
    /// value type" (III.4.21).
    /// </summary>
    /// <remarks>For value types, the <c>initobj</c> instruction is usually used instead.</remarks>
    [<RequireQualifiedAccess>]
    module Newobj =
        val ofMethod : ctor: MethodTok -> Instruction // TODO: Move Newobj.ofMethod to Unsafe module?
    
        val inline ofDefinedMethod :
            ctor: MethodTok<TypeDefinition<'Kind>, FSharpIL.Cli.MethodDefinition<FSharpIL.Cli.MethodKinds.ObjectConstructor>> ->
            Instruction when 'Kind :> TypeKinds.IHasConstructors



    /// (0x7B) Pops a member reference off of the stack and pushes the value of an instance field onto the stack (III.4.10).
    // TODO: Instead take a FieldDefinition<FieldKinds.Instance> or FieldReference<FieldKinds.Instance>
    val ldfld : field: FieldTok -> Instruction

    /// (0x7C) Pops a member reference off of the stack and pushes an unmanaged pointer to an instance field onto the stack
    /// (III.4.11).
    val ldflda : field: FieldTok -> Instruction

    /// (0x7D) Pops a member reference and a value off of the stack, storing the value into an instance field (III.4.28).
    val stfld : field: FieldTok -> Instruction // TODO: How to accept both DefinedField and ReferencedField that are static only?

    /// (0x7E) Loads the value of a static field onto the stack (III.4.14).
    val ldsfld : field: FieldTok -> Instruction

    /// (0x7F) Pushes an unmanaged pointer to a static field onto the stack (III.4.15).
    val ldsflda : field: FieldTok -> Instruction // TODO: How to accept both DefinedField and ReferencedField that are static only?

    /// (0x80) Pops a value off of the stack and stores it into a static field (III.4.30).
    val stsfld : field: FieldTok -> Instruction // TODO: How to accept both DefinedField and ReferencedField that are static only?



    /// (0x8D) Pops a signed native or 32-bit integer <c>length</c> off of the stack and creates "a zero-based, one-dimensional
    /// array" of the specified type (III.4.20).
    val newarr : etype: TypeTok -> Instruction

    /// (0x8E) Pops an object reference to an array off of the stack and pushes the length of the array onto the stack as an
    /// unsigned native integer (III.4.12).
    val ldlen : Instruction



    /// (0xFE 0x09) Long form of an instruction that loads the specified argument onto the stack (III.3.38).
    val ldarg : num: uint16 -> Instruction



    /// <summary>
    /// (0xFE 0x0C) Long form of an instruction that loads the local variable at the specified <paramref name="index"/> onto the
    /// stack (III.3.43).
    /// </summary>
    val ldloc : index: LocalVarIndex -> Instruction



    /// <summary>
    /// (0xFE 0x0E) Long form of an instruction that pops a value from the stack and stores it into the local variable at the
    /// specified <paramref name="index"/> (III.3.63).
    /// </summary>
    val stloc : index: LocalVarIndex -> Instruction

    /// Contains functions used to write the most shortened from of CIL opcodes whenever possible.
    module Shortened =
        /// (0x02 to 0x05, 0x0E, 0xFE 0x09) Shortest form of an instruction that loads the specified argument onto the stack
        /// (III.3.38).
        val ldarg : num: uint16 -> Instruction

        /// <summary>
        /// (0x06 to 0x09, 0x11, 0xFE 0x0C) Shortest form of an instruction that loads the local variable at the specified
        /// <paramref name="index"/> onto the stack (III.3.43).
        /// </summary>
        val ldloc : index: LocalVarIndex -> Instruction

        /// <summary>
        /// (0x0A to 0x0D, 0x13, 0xFE 0x0E) Shortest from of an instruction that pops a value from the stack and stores it into
        /// the local variable at the specified <paramref name="index"/> (III.3.63).
        /// </summary>
        val stloc : index: LocalVarIndex -> Instruction

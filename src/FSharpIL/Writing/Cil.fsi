/// Contains functions and types used for writing Common Intermediate Language method bodies (II.25.4 and III).
module FSharpIL.Writing.Cil

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

open FSharpIL.Cli.MemberTok

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodHeader =
    { InitLocals: InitLocals
      MaxStack: MaxStack voption
      /// <summary>
      /// An index into the <c>StandaloneSig</c> (0x11) table describing the types of the local variables of the method.
      /// </summary>
      LocalVariables: FSharpIL.Metadata.Tables.TableIndex<FSharpIL.Metadata.Tables.StandaloneSigRow> }

[<Sealed>]
type Label = interface IEquatable<Label>

[<IsReadOnly; Struct; NoComparison; NoEquality; RequireQualifiedAccessAttribute>]
type BranchKind =
    /// The target of the branch instruction is represented as a 1-byte integer where possible.
    | Shortened
    /// The target of the bracnh instruction is always represented as a 4-byte integer.
    | Long

[<NoComparison; NoEquality; RequireQualifiedAccess>]
type Operand =
    | Nothing
    | Byte of uint8
    | Short of uint16
    | Integer of uint32
    | RawToken of MetadataToken
    | FieldToken of FieldTok
    | MethodToken of MethodTok
    | TypeToken of FSharpIL.Cli.TypeSystem.TypeTok
    | BranchTarget of BranchKind * Label

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
    [<Literal>] val MaxTinyBodySize: uint32 = 63u
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    [<Literal>] val internal FatFormatSize: uint32 = 3u
    [<Literal>] val internal FatMethodAlignment: uint32 = 4u
    [<Literal>] val internal MaxTinyStackCount: uint16 = 8us

/// Represents the method bodies of the CLI metadata (II.25.4).
[<Sealed>]
type MethodBodyStream =
    internal new: unit -> MethodBodyStream

    member Add: body: MethodBody -> struct(MaxStack * uint32)

    member internal WriteTo: section: byref<FSharpIL.ChunkedMemoryBuilder> -> unit

[<Interface>]
type IMetadataTokenSource =
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: FSharpIL.Cli.TypeSystem.TypeTok -> TypeMetadataToken

[<AutoOpen>]
module Instructions =
    [<RequireQualifiedAccess>]
    module Instruction =
        /// Creates an instruction that takes no arguments and pushes values to or pops values from the stack.
        val inline simple : opcode: Opcode -> StackBehavior -> Instruction

        /// Creates an instruction that takes no arguments and does not modify the stack.
        val inline op : opcode: Opcode -> Instruction

        /// Creates an instruction that takes no arguments and pushes one value onto the stack.
        val inline pushes1 : opcode: Opcode -> Instruction

        /// Creates an instruction that takes no arguments and pops one value from the stack.
        val inline pops1 : opcode: Opcode -> Instruction

        // TODO: Only provide values to write long form of branching instructions ONLY, or the most shortened ones in the Shortened module.
        val inline branching : opcode: Opcode -> StackBehavior -> kind: BranchKind -> target: Label -> Instruction

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
    val inline ldarg_s : num: uint8 -> Instruction

    /// Contains functions used to write the most shortened from of CIL opcodes whenever possible.
    module Shortened = begin end

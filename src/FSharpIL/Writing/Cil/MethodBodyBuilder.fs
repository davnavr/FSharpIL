namespace FSharpIL.Writing.Cil

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

type [<IsReadOnly; Struct>] MethodBodyOffset = private { MethodBodyOffset : uint32 }

/// <summary>Represents the target of a branch instruction (III.1.7.2).</summary>
/// <remarks>
/// For negative branch targets, refer to III.1.7.5 for the rules regarding when a branch target to a location results in
/// invalid CIL code.
/// </remarks>
type BranchTarget = struct
    [<DefaultValue>] val mutable Target: int32
    val Position: MethodBodyOffset
    val ShortOpcode: Opcode
    val LongOpcode: Opcode
    internal new (pos, short, long) = { Position = pos; ShortOpcode = short; LongOpcode = long }
    // TODO: Check that these are the lower and upper bounds for int8, since ECMA-335 doesn't appear to say it.
    /// Gets a value indicating whether this branch target must be written as a 4-byte integer rather than a 1-byte integer.
    member this.IsLarge = this.Target > int32 SByte.MaxValue || this.Target < int32 SByte.MinValue
    static member op_Implicit (target: BranchTarget) = int64 target.Target
    static member op_Implicit (target: BranchTarget) = target.Target
    static member op_Explicit (target: BranchTarget) = Checked.int8 target.Target
end

[<IsByRefLike; Struct>]
[<NoComparison; NoEquality>]
type MethodBodyBuilder =
    internal
        { mutable estimatedMaxStack: uint16
          branchTargetList: ImmutableArray<BranchTarget>.Builder
          mutable instructions: ChunkedMemoryBuilder }

    /// <summary>Estimates the maximum number of items that are pushed onto the evaluation stack by this method body.</summary>
    /// <remarks>This estimate might be higher than the number of items that is actually needed by the method.</remarks>
    member this.EstimatedMaxStack = this.estimatedMaxStack

[<Struct>]
type internal MethodCallPatch =
    { MethodCall: FSharpIL.Cli.MethodCallTarget
      CallOpcode: Opcode
      InstructionWriter: ChunkedMemoryBuilder }

[<IsReadOnly; Struct>]
type MethodTokenSource =
    internal { MethodCalls: ImmutableArray<MethodCallPatch>.Builder }

/// Represents the destination that a branch instruction would jump to.
[<IsReadOnly; IsByRefLike>]
type Label = struct
    val Destination: MethodBodyOffset
    new (writer: inref<MethodBodyBuilder>) = { Destination = { MethodBodyOffset = writer.instructions.Length } }
end

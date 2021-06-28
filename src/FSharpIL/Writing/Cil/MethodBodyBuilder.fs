namespace FSharpIL.Writing.Cil

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Cil

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
          // TODO: Rename to instructions
          mutable methodBody: ChunkedMemoryBuilder }

    member this.EstimatedMaxStack = this.estimatedMaxStack

/// Represents the destination that a branch instruction would jump to.
[<IsReadOnly; IsByRefLike>]
type Label = struct
    val Destination: MethodBodyOffset
    new (writer: inref<MethodBodyBuilder>) = { Destination = { MethodBodyOffset = writer.methodBody.Length } }
end

/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module MethodBodyBuilder = // TODO: Update estimated max stack value.
    module Unsafe =
        let internal writeOpcodeHelper (wr: byref<ChunkedMemoryBuilder>) opcode =
            if opcode < Opcode.Arglist
            then wr.Write(uint8 opcode)
            else
                let opcode' = uint16 opcode
                wr.Write(uint8(opcode' >>> 8))
                wr.Write(uint8(opcode' &&& 0xFFus))

        let getOpcodeWriter (builder: byref<_>) = &builder.methodBody

        let writeRawOpcode (builder: byref<_>) opcode = writeOpcodeHelper &builder.methodBody opcode

        /// <summary>Writes a metadata token (III.1.9).</summary>
        /// <exception cref="T:System.ArgumentOutOfRangeException">
        /// Thrown when the <paramref name="index"/> cannot be represented as a 3-byte integer.
        /// </exception>
        let writeMetadataToken (builder: byref<_>) (table: uint8) index =
            if index > 0xFFFFFFu then
                argOutOfRange "index" index "The row or offset pointed to by the metadata token must be able to fit in 3 bytes"
            let wr = &builder.methodBody
            uint8(index &&& 0xFFu) |> wr.Write
            uint8((index >>> 8) &&& 0xFFu) |> wr.Write
            uint8((index >>> 16) &&& 0xFFu) |> wr.Write
            wr.Write table

        let writeBranchInstruction short long ({ branchTargetList = targets } as builder: byref<_>) =
            let i = targets.Count
            targets.Add(BranchTarget({ MethodBodyOffset = builder.methodBody.Length }, short, long))
            &Unsafe.AsRef(&targets.ItemRef i)
            failwith "TODO: Fix, changes to the branch target won't propogate if the internal array of targets is reallocated."

    open Unsafe

    [<RequireQualifiedAccess>]
    module Branch =
        /// Creates a destination for a branch instruction. The branch instruction will jump to the last byte that was written.
        let inline createLabel (wr: inref<_>) = Label &wr
        /// Sets the target of a branch instruction.
        let setTarget (branch: byref<BranchTarget>) (destination: Label) =
            branch.Target <- int32(int64 destination.Destination.MethodBodyOffset - int64 branch.Position.MethodBodyOffset)


    /// (0x00) Writes an instruction that does nothing (III.3.51).
    let inline nop (wr: byref<_>) = writeRawOpcode &wr Opcode.Nop

    /// <summary>
    /// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
    /// tripped" (III.3.16).
    /// </summary>
    let inline ``break`` (wr: byref<_>) = writeRawOpcode &wr Opcode.Break

    /// (0x02) Writes an instruction that loads argument #0 onto the stack (III.3.38).
    let inline ldarg_0 (wr: byref<_>) = writeRawOpcode &wr Opcode.Ldarg_0
    /// (0x03) Writes an instruction that loads argument #1 onto the stack (III.3.38).
    let inline ldarg_1 (wr: byref<_>) = writeRawOpcode &wr Opcode.Ldarg_1
    /// (0x04) Writes an instruction that loads argument #2 onto the stack (III.3.38).
    let inline ldarg_2 (wr: byref<_>) = writeRawOpcode &wr Opcode.Ldarg_2
    /// (0x05) Writes an instruction that loads argument #3 onto the stack (III.3.38).
    let inline ldarg_3 (wr: byref<_>) = writeRawOpcode &wr Opcode.Ldarg_3



    /// (0x0E) Writes the short form of an instruction that loads the specified argument onto the stack (III.3.38).
    let inline ldarg_s (wr: byref<MethodBodyBuilder>) (num: uint8) =
        writeRawOpcode &wr Opcode.Ldarg_s
        (getOpcodeWriter &wr).Write num



    /// (0x2A) Writes an instruction used to return from the current method (III.3.56).
    let inline ret (wr: byref<_>) = writeRawOpcode &wr Opcode.Ret



    /// <summary>
    /// (0x72) Writes an instruction that loads a string from the <c>#US</c> heap (III.4.16).
    /// </summary>
    /// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
    let ldstr (wr: byref<_>) (str: UserStringOffset) =
        writeRawOpcode &wr Opcode.Ldstr
        writeMetadataToken &wr 0x70uy (uint32 str)



    /// (0xFE 0x09) Writes the long form of an instruction that loads the specified argument onto the stack (III.3.38).
    let inline ldarg (wr: byref<_>) (num: uint16) =
        writeRawOpcode &wr Opcode.Ldarg
        (getOpcodeWriter &wr).WriteLE num

    /// Contains functions used to write the most shorted from of CIL opcodes whenever possible.
    module Shortened =
        /// <summary>
        /// (0x02 to 0x05, 0x0E, 0xFE 0x09) Writes the shortest form of an instruction that loads an argument onto the stack
        /// (III.3.38).
        /// </summary>
        let ldarg (wr: byref<_>) num =
            match num with
            | 0us -> ldarg_0 &wr
            | 1us -> ldarg_1 &wr
            | 2us -> ldarg_2 &wr
            | 3us -> ldarg_3 &wr
            | _ when num <= 0xFFus -> ldarg_s &wr (uint8 num)
            | _ -> ldarg &wr num

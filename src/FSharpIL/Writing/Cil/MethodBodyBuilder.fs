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
    { MethodCall: FSharpIL.Cli.MethodCall
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

[<IsReadOnly; Struct>]
type MethodMetadataToken private (token: MetadataToken) = struct // TODO: Move this to separate file.
    internal new (tag, index) = MethodMetadataToken(MetadataToken(tag, index))
    member _.Type = token.Type
    member _.Token = token
    override _.ToString() = token.ToString()
end

[<RequireQualifiedAccess>]
module MethodMetadataToken =
    let Def ({ TableIndex = i }: TableIndex<MethodDefRow>) = MethodMetadataToken(MetadataTokenType.MethodDef, i)
    let Ref ({ TableIndex = i }: TableIndex<MemberRefRow>) = MethodMetadataToken(MetadataTokenType.MemberRef, i)
    let Spec ({ TableIndex = i }: TableIndex<MethodSpecRow>) = MethodMetadataToken(MetadataTokenType.MethodSpec, i)

/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module MethodBodyBuilder = // TODO: Rename module to MethodBodyInstructions and move it to another file
    module Unsafe =
        let internal writeOpcodeHelper (wr: byref<ChunkedMemoryBuilder>) opcode =
            if opcode < Opcode.Arglist
            then wr.Write(uint8 opcode)
            else
                let opcode' = uint16 opcode
                wr.Write(uint8(opcode' >>> 8))
                wr.Write(uint8(opcode' &&& 0xFFus))

        let getInstructionStream (builder: byref<_>) = &builder.instructions

        /// <summary>Increments the estimated <c>MaxStack</c> value for the method body.</summary>
        let incrMaxStack (builder: byref<_>) = builder.estimatedMaxStack <- Checked.(+) builder.estimatedMaxStack 1us

        let writeRawOpcode (builder: byref<_>) opcode = writeOpcodeHelper &builder.instructions opcode

        /// Writes an opcode that pushes a single item onto the stack.
        let inline writePushingOpcode (builder: byref<_>) opcode =
            writeRawOpcode &builder opcode
            incrMaxStack &builder

        /// <summary>Writes a metadata token (III.1.9).</summary>
        let writeMetadataToken (builder: byref<_>) (token: MetadataToken) =
            let mutable wr = &builder.instructions
            let index = token.Index
            // For some reason, usage of |> here prevents writer from updating correctly.
            wr.Write(uint8(index &&& 0xFFu))
            wr.Write(uint8((index >>> 8) &&& 0xFFu))
            wr.Write(uint8((index >>> 16) &&& 0xFFu))
            wr.Write(uint8 token.Type)

        let writeBranchInstruction short long ({ branchTargetList = targets } as builder: byref<_>) =
            let i = targets.Count
            targets.Add(BranchTarget({ MethodBodyOffset = builder.instructions.Length }, short, long))
            failwith "TODO: Fix, changes to the branch target won't propogate if the internal array of targets is reallocated."
            &Unsafe.AsRef(&targets.ItemRef i)

        let internal writeCallInstruction (wr: byref<_>) opcode (method: MethodMetadataToken) hasRetValue =
            writeRawOpcode &wr opcode
            writeMetadataToken &wr method.Token
            if hasRetValue then incrMaxStack &wr

    open Unsafe

    [<RequireQualifiedAccess>]
    module Branch =
        /// Creates a destination for a branch instruction. The branch instruction will jump to the last byte that was written.
        let inline createLabel (wr: inref<_>) = Label &wr
        /// Sets the target of a branch instruction.
        let setTarget (branch: byref<BranchTarget>) (destination: Label) =
            branch.Target <- int32(int64 destination.Destination.MethodBodyOffset - int64 branch.Position.MethodBodyOffset)

    /// Contains functions for generating instructions that call methods specified by a metadata token.
    module Call =
        /// <summary>(0x28) Writes an instruction that calls the specified <paramref name="method"/> (III.3.19).</summary>
        /// <param name="method">The method to call, either a <c>MethodDef</c>, <c>MemberRef</c>, or <c>MethodSpec</c>.</param>
        /// <param name="hasRetValue">
        /// Indicates whether or not calling the method would push a return value onto the stack, and updates the estimated
        /// <c>MaxStack</c> of the method accordingly.
        /// </param>
        /// <param name="stream">The method body stream that the <c>call</c> instruction is written to.</param>
        let call (stream: byref<_>) method hasRetValue = writeCallInstruction &stream Opcode.Call method hasRetValue
        
        /// <summary>(0x6F) Writes an instruction that calls the <paramref name="method"/> associated with an object (III.4.2).</summary>
        let callvirt (stream: byref<_>) method hasRetValue = writeCallInstruction &stream Opcode.Callvirt method hasRetValue

    /// (0x00) Writes an instruction that does nothing (III.3.51).
    let inline nop (wr: byref<_>) = writeRawOpcode &wr Opcode.Nop

    /// <summary>
    /// (0x01) Writes an instruction used for debugging that "signals the CLI to inform the debugger that a breakpoint has been
    /// tripped" (III.3.16).
    /// </summary>
    let inline ``break`` (wr: byref<_>) = writeRawOpcode &wr Opcode.Break

    /// (0x02) Writes an instruction that loads argument #0 onto the stack (III.3.38).
    let inline ldarg_0 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_0
    /// (0x03) Writes an instruction that loads argument #1 onto the stack (III.3.38).
    let inline ldarg_1 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_1
    /// (0x04) Writes an instruction that loads argument #2 onto the stack (III.3.38).
    let inline ldarg_2 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_2
    /// (0x05) Writes an instruction that loads argument #3 onto the stack (III.3.38).
    let inline ldarg_3 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_3



    /// (0x0E) Writes the short form of an instruction that loads the specified argument onto the stack (III.3.38).
    let inline ldarg_s (wr: byref<_>) (num: uint8) =
        writeRawOpcode &wr Opcode.Ldarg_s
        (getInstructionStream &wr).Write num
        incrMaxStack &wr



    /// (0x26) Writes an instruction that pops the value at the top of the stack (III.3.54).
    let inline pop (wr: byref<_>) = writeRawOpcode &wr Opcode.Pop



    let private patchedMethodCall (wr: byref<_>) opcode method { MethodCalls = calls } =
        // 1 byte for opcode, 4 bytes for method token.
        calls.Add { MethodCall = method; CallOpcode = opcode; InstructionWriter = wr.instructions.ReserveBytes 5 }

    /// (0x28) Writes an instruction that calls the specified method (III.3.19).
    let call (wr: byref<_>) method methodTokenSource =
        patchedMethodCall &wr Opcode.Call method methodTokenSource



    //let calli ({ TableIndex = index }: TableIndex<StandaloneSigRow>)



    /// (0x2A) Writes an instruction used to return from the current method (III.3.56).
    let inline ret (wr: byref<_>) = writeRawOpcode &wr Opcode.Ret



    /// <summary>(0x6F) Writes an instruction that calls a method associated with an object (III.4.2).</summary>
    let callvirt (wr: byref<_>) method methodTokenSource =
        patchedMethodCall &wr Opcode.Callvirt method methodTokenSource



    /// <summary>
    /// (0x72) Writes an instruction that loads a string from the <c>#US</c> heap (III.4.16).
    /// </summary>
    /// <remarks>To load a <see langword="null"/> string, generate the <c>ldnull</c> opcode instead.</remarks>
    let ldstr (wr: byref<_>) { UserStringOffset = offset } =
        writeRawOpcode &wr Opcode.Ldstr
        writeMetadataToken &wr (MetadataToken(MetadataTokenType.UserStringHeap, offset))
        incrMaxStack &wr



    /// (0xFE 0x09) Writes the long form of an instruction that loads the specified argument onto the stack (III.3.38).
    let inline ldarg (wr: byref<_>) (num: uint16) =
        writeRawOpcode &wr Opcode.Ldarg
        (getInstructionStream &wr).WriteLE num
        incrMaxStack &wr

    /// Contains functions used to write the most shortened from of CIL opcodes whenever possible.
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

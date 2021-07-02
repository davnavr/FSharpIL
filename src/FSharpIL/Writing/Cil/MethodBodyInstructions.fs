/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module FSharpIL.Writing.Cil.MethodBodyInstructions

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

module Unsafe =
    let writeOpcodeHelper (wr: byref<ChunkedMemoryBuilder>) opcode =
        if opcode < Opcode.Arglist
        then wr.Write(uint8 opcode)
        else
            let opcode' = uint16 opcode
            wr.Write(uint8(opcode' >>> 8))
            wr.Write(uint8(opcode' &&& 0xFFus))

    let getInstructionStream (builder: byref<_>) = &builder.instructions

    let incrMaxStack (builder: byref<_>) = builder.estimatedMaxStack <- Checked.(+) builder.estimatedMaxStack 1us

    let writeRawOpcode (builder: byref<_>) opcode = writeOpcodeHelper &builder.instructions opcode

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
        &System.Runtime.CompilerServices.Unsafe.AsRef(&targets.ItemRef i)

    let writeCallInstruction (wr: byref<_>) opcode (method: MethodMetadataToken) hasRetValue =
        writeRawOpcode &wr opcode
        writeMetadataToken &wr method.Token
        if hasRetValue then incrMaxStack &wr

    let writeFieldInstruction (wr: byref<_>) opcode (field: FieldMetadataToken) pushesFieldValue =
        writeRawOpcode &wr opcode
        writeMetadataToken &wr field.Token
        if pushesFieldValue then incrMaxStack &wr

open Unsafe

[<RequireQualifiedAccess>]
module Branch =
    let inline createLabel (wr: inref<_>) = Label &wr
    let setTarget (branch: byref<BranchTarget>) (destination: Label) =
        branch.Target <- int32(int64 destination.Destination.MethodBodyOffset - int64 branch.Position.MethodBodyOffset)

/// Contains functions for generating instructions that call methods specified by a metadata token.
module Call =
    let call (stream: byref<_>) method hasRetValue = writeCallInstruction &stream Opcode.Call method hasRetValue
    let callvirt (stream: byref<_>) method hasRetValue = writeCallInstruction &stream Opcode.Callvirt method hasRetValue

let inline nop (wr: byref<_>) = writeRawOpcode &wr Opcode.Nop
let inline ``break`` (wr: byref<_>) = writeRawOpcode &wr Opcode.Break
let inline ldarg_0 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_0
let inline ldarg_1 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_1
let inline ldarg_2 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_2
let inline ldarg_3 (wr: byref<_>) = writePushingOpcode &wr Opcode.Ldarg_3

let inline ldarg_s (stream: byref<_>) (num: uint8) =
    writeRawOpcode &stream Opcode.Ldarg_s
    (getInstructionStream &stream).Write num
    incrMaxStack &stream

let inline pop (wr: byref<_>) = writeRawOpcode &wr Opcode.Pop

let private patchedMethodCall (wr: byref<_>) opcode method { MethodCalls = calls } =
    // 1 byte for opcode, 4 bytes for method token.
    calls.Add { Target = method; Opcode = opcode; InstructionWriter = wr.instructions.ReserveBytes 5 }

let call (wr: byref<_>) method methodTokenSource =
    patchedMethodCall &wr Opcode.Call method methodTokenSource

//let calli ({ TableIndex = index }: TableIndex<StandaloneSigRow>)

let inline ret (wr: byref<_>) = writeRawOpcode &wr Opcode.Ret

let callvirt (wr: byref<_>) method methodTokenSource =
    patchedMethodCall &wr Opcode.Callvirt method methodTokenSource

let ldstr (wr: byref<_>) { UserStringOffset = offset } =
    writeRawOpcode &wr Opcode.Ldstr
    writeMetadataToken &wr (MetadataToken(MetadataTokenType.UserStringHeap, offset))
    incrMaxStack &wr

let private patchedFieldInstruction (wr: byref<_>) opcode field pushesFieldValue { FieldInstructions = instrs } =
    // 1 byte for opcode, 4 bytes for field token.
    instrs.Add
        { Target =
            { PushesFieldValue = pushesFieldValue
              Argument = field }
          Opcode = opcode
          InstructionWriter = wr.instructions.ReserveBytes 5 }

let ldfld (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Ldfld field true fieldTokenSource

let ldflda (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Ldflda field true fieldTokenSource

let stfld (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Stfld field false fieldTokenSource

let ldsfld (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Ldsfld field true fieldTokenSource

let ldsflda (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Ldsflda field true fieldTokenSource

let stsfld (wr: byref<_>) field fieldTokenSource =
    patchedFieldInstruction &wr Opcode.Stfld field false fieldTokenSource

let inline ldarg (wr: byref<_>) (num: uint16) =
    writeRawOpcode &wr Opcode.Ldarg
    (getInstructionStream &wr).WriteLE num
    incrMaxStack &wr

module Shortened =
    let ldarg (wr: byref<_>) num =
        match num with
        | 0us -> ldarg_0 &wr
        | 1us -> ldarg_1 &wr
        | 2us -> ldarg_2 &wr
        | 3us -> ldarg_3 &wr
        | _ when num <= 0xFFus -> ldarg_s &wr (uint8 num)
        | _ -> ldarg &wr num

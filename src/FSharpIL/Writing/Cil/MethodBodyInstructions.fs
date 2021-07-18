/// Contains functions used for writing CIL method bodies.
[<AutoOpen>]
module FSharpIL.Writing.Cil.MethodBodyInstructions

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

open FSharpIL.Cli

module Unsafe =
    let writeOpcodeHelper (stream: byref<ChunkedMemoryBuilder>) opcode =
        if opcode < Opcode.Arglist
        then stream.Write(uint8 opcode)
        else
            let opcode' = uint16 opcode
            stream.Write(uint8(opcode' >>> 8))
            stream.Write(uint8(opcode' &&& 0xFFus))

    let getInstructionStream (wr: byref<_>) = &wr.instructions

    let incrMaxStack (stream: byref<_>) = stream.estimatedMaxStack <- Checked.(+) stream.estimatedMaxStack 1us

    let writeRawOpcode (stream: byref<_>) opcode = writeOpcodeHelper &stream.instructions opcode

    let inline writePushingOpcode (stream: byref<_>) opcode =
        writeRawOpcode &stream opcode
        incrMaxStack &stream

    /// <summary>Writes a metadata token (III.1.9).</summary>
    let writeMetadataToken (stream: byref<_>) (token: MetadataToken) =
        let mutable wr = &stream.instructions
        let index = token.Index
        // For some reason, usage of |> here prevents writer from updating correctly.
        wr.Write(uint8(index &&& 0xFFu))
        wr.Write(uint8((index >>> 8) &&& 0xFFu))
        wr.Write(uint8((index >>> 16) &&& 0xFFu))
        wr.Write(uint8 token.Type)

    let writeStringOffset (stream: byref<_>) { UserStringOffset = offset } =
        writeMetadataToken &stream (MetadataToken(MetadataTokenType.UserStringHeap, offset))

    let writeBranchInstruction short long ({ branchTargetList = targets } as stream: byref<_>) =
        let i = targets.Count
        targets.Add(BranchTarget({ MethodBodyOffset = stream.instructions.Length }, short, long))
        failwith "TODO: Fix, changes to the branch target won't propogate if the internal array of targets is reallocated."
        &System.Runtime.CompilerServices.Unsafe.AsRef(&targets.ItemRef i)

    let inline writeStringToken (stream: byref<_>) (str: string) (tokens: MetadataTokenSource) =
        writeStringOffset &stream (tokens.GetUserString str)

    let inline writeMethodToken (stream: byref<_>) (method: MethodCallTarget<_, _>) (tokens: MetadataTokenSource) =
        writeMetadataToken &stream ((tokens.GetMethodToken method).Token)

    let inline writeFieldToken (stream: byref<_>) (field: FieldArg<_, _>) (tokens: MetadataTokenSource) =
        writeMetadataToken &stream ((tokens.GetFieldToken field).Token)

    let inline writeTypeToken (stream: byref<_>) t (tokens: MetadataTokenSource) =
        writeMetadataToken &stream ((tokens.GetTypeToken t).Token)

    let inline writeFieldInstruction (stream: byref<_>) opcode pushesFieldValue field tokens =
        writeRawOpcode &stream opcode
        writeFieldToken &stream field tokens
        if pushesFieldValue then incrMaxStack &stream

    let inline writeTypeInstruction (stream: byref<_>) opcode t tokens =
        writeRawOpcode &stream opcode
        writeTypeToken &stream t tokens
        incrMaxStack &stream // Assumes that all instruction that take a typeTok returns something onto the stack.

open Unsafe

[<RequireQualifiedAccess>]
module Ldstr =
    let inline ofOffset (stream: byref<_>) offset =
        writeRawOpcode &stream Opcode.Ldstr
        writeStringOffset &stream offset
        incrMaxStack &stream

    let inline ofString (stream: byref<_>) str tokens =
        writeRawOpcode &stream Opcode.Ldstr
        writeStringToken &stream str tokens
        incrMaxStack &stream

    let inline ofMemory (stream: byref<_>) (str: inref<System.ReadOnlyMemory<_>>) (tokens: MetadataTokenSource) =
        writeRawOpcode &stream Opcode.Ldstr
        writeStringOffset &stream (tokens.GetUserString &str)
        incrMaxStack &stream

[<RequireQualifiedAccess>]
module Branch =
    let inline createLabel (stream: inref<_>) = Label &stream
    let setTarget (branch: byref<BranchTarget>) (destination: Label) =
        branch.Target <- int32(int64 destination.Destination.MethodBodyOffset - int64 branch.Position.MethodBodyOffset)

module Call =
    let inline instr (stream: byref<_>) opcode (method: MethodMetadataToken) hasRetValue =
        writeRawOpcode &stream opcode
        writeMetadataToken &stream method.Token
        if hasRetValue then incrMaxStack &stream

    let instr' (stream: byref<_>) opcode method (tokens: MetadataTokenSource) =
        instr &stream opcode (tokens.GetMethodToken method) (not method.Method.ReturnType.IsVoid)

    let call (stream: byref<_>) method hasRetValue = instr &stream Opcode.Call method hasRetValue
    let callvirt (stream: byref<_>) method hasRetValue = instr &stream Opcode.Callvirt method hasRetValue

let inline nop (stream: byref<_>) = writeRawOpcode &stream Opcode.Nop
let inline ``break`` (stream: byref<_>) = writeRawOpcode &stream Opcode.Break
let inline ldarg_0 (stream: byref<_>) = writePushingOpcode &stream Opcode.Ldarg_0
let inline ldarg_1 (stream: byref<_>) = writePushingOpcode &stream Opcode.Ldarg_1
let inline ldarg_2 (stream: byref<_>) = writePushingOpcode &stream Opcode.Ldarg_2
let inline ldarg_3 (stream: byref<_>) = writePushingOpcode &stream Opcode.Ldarg_3

let inline ldarg_s (stream: byref<_>) (num: uint8) =
    writeRawOpcode &stream Opcode.Ldarg_s
    (getInstructionStream &stream).Write num
    incrMaxStack &stream

let inline pop (stream: byref<_>) = writeRawOpcode &stream Opcode.Pop

let call (stream: byref<_>) method tokens = Call.instr' &stream Opcode.Call method tokens

//let calli ({ TableIndex = index }: TableIndex<StandaloneSigRow>)

let inline ret (wr: byref<_>) = writeRawOpcode &wr Opcode.Ret

let callvirt (stream: byref<_>) method tokens = Call.instr' &stream Opcode.Callvirt method tokens

let inline ldstr (stream: byref<_>) str tokens = Ldstr.ofString &stream str tokens

let inline ldfld (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Ldfld true field tokens
let inline ldflda (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Ldflda true field tokens
let inline stfld (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Stfld false field tokens
let inline ldsfld (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Ldsfld true field tokens
let inline ldsflda (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Ldsflda true field tokens
let inline stsfld (stream: byref<_>) (field: FieldArg) tokens = writeFieldInstruction &stream Opcode.Stfld false field tokens

let inline newarr (stream: byref<_>) etype typeTokenSource =
    writeTypeInstruction &stream Opcode.Newarr etype typeTokenSource
    incrMaxStack &stream

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

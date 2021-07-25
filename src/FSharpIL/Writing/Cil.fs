module FSharpIL.Writing.Cil

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL
open FSharpIL.Utilities

[<Struct>]
type MethodHeader =
    { InitLocals: InitLocals
      MaxStack: MaxStack voption
      LocalVariables: TableIndex<StandaloneSigRow> }

[<Sealed>]
type Label () =
    interface IEquatable<Label> with member this.Equals other = Object.ReferenceEquals(this, other)

[<Struct>]
type BranchKind = | Shortened | Long

type Operand =
    | Nothing
    | Byte of uint8
    | Short of uint16
    | Integer of uint32
    | RawToken of MetadataToken
    | FieldToken of FieldTok
    | MethodToken of MethodTok
    | TypeToken of TypeTok
    | BranchTarget of BranchKind * Label

    override this.ToString() =
        match this with
        | Nothing -> String.Empty
        | Byte(ToString str)
        | Short(ToString str)
        | Integer(ToString str)
        | FieldToken(ToString str)
        | MethodToken(ToString str)
        | TypeToken(ToString str) -> str
        | RawToken token -> sprintf "0x%08X // %A" token.Value token.Type
        | BranchTarget _ -> "IL_XXXX"

[<Struct>]
type StackBehavior =
    | PopOrPush of int8

[<Struct>]
type Instruction =
    { Opcode: Opcode
      Operand: Operand
      StackBehavior: StackBehavior }

    override this.ToString() =
        let opcode = ParsedOpcode.name this.Opcode
        match this.Operand.ToString() with
        | "" -> opcode
        | operand -> String.Concat(opcode, " ", operand)

type InstructionBlock =
    private
    | InstructionSequence of seq<Instruction>
    | InstructionList of Instruction list
    | InstructionBlock of ImmutableArray<Instruction>
    | Labelled of Label * InstructionBlock

[<IsReadOnly; Struct; RequireQualifiedAccess>]
type InstructionOrLabel =
    | Instruction of instruction: Instruction
    | Label of label: Label

module InstructionBlock =
    let ofList instructions = InstructionList instructions
    let ofBlock instructions = InstructionBlock instructions
    let ofSeq instructions = InstructionSequence instructions
    let label next = let l = Label() in struct(l, Labelled(l, next))

    // TODO: Return an enumerable with a struct enumerator where possible.
    let rec toSeq block =
        let inline (|Instructions|) (instructions: #seq<_>) = instructions :> seq<_>
        match block with
        | InstructionBlock(Instructions instructions)
        | InstructionList(Instructions instructions)
        | InstructionSequence instructions -> Seq.map InstructionOrLabel.Instruction instructions
        | Labelled(label, block') ->
            seq {
                InstructionOrLabel.Label label
                yield! toSeq block'
            }

type MethodBody = { Header: MethodHeader; Instructions: seq<InstructionBlock> }

module MethodBody =
    let [<Literal>] MaxTinyBodySize = 63u
    let [<Literal>] MaxTinyStackCount = 8us
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    let [<Literal>] FatFormatSize = 3u
    let [<Literal>] FatMethodAlignment = 4u

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodBodyKind = | Tiny | Fat

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodBodyEntry =
    { Kind: MethodBodyKind
      MaxStack: MaxStack
      CodeSize: uint32
      Instructions: ImmutableArray<Instruction>.Builder
      LabelLookup: Dictionary<Label, uint32> }

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type BranchTargetOffset =
    { Target: Label
      Kind: BranchKind
      Offset: ChunkedMemoryBuilder }

type IMetadataTokenSource =
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: TypeTok -> TypeMetadataToken

[<Sealed>]
type MethodBodyStream (metadataTokenSource: IMetadataTokenSource) =
    let bodies = Dictionary<MethodBody, MethodBodyEntry>()

    // TODO: Store dictionary of method bodies in CliModuleBuilder, so DefineMethod can report errors with bodies.
    member _.Add body = // TODO: Return a Result
        match bodies.TryGetValue body with
        | true, { MaxStack = maxStack; CodeSize = size } -> struct(maxStack, size)
        | false, _ ->
            let instructions = ImmutableArray.CreateBuilder 16
            let mutable size, stack, maxStack = 0u, 0us, MaxStack.Zero
            let mutable resolvedLabels = Unchecked.defaultof<Dictionary<Label, uint32>>
            let mutable unresolvedLabels = Unchecked.defaultof<HashSet<Label>>
            // TODO: Keep track of SEH information.

            // TODO: Avoid allocations by using the struct IEnumerators for some instruction blocks.
            for block in body.Instructions do
                for item in InstructionBlock.toSeq block do
                    match item with
                    | InstructionOrLabel.Instruction instruction ->
                        instructions.Add instruction

                        match instruction.Operand with
                        | BranchTarget(_, label) when not(resolvedLabels.ContainsKey label) ->
                            unresolvedLabels.Add label |> ignore
                        | _ -> ()

                        stack <-
                            match instruction.StackBehavior with
                            | PopOrPush amt -> Checked.uint16(int32 stack - int32 amt) // TODO: How to deal with stack underflow?

                        if stack > uint16 maxStack then maxStack <- MaxStack stack

                        size <- size + Opcode.size instruction.Opcode

                        size <- size +
                            match instruction.Operand with
                            | Nothing -> 0u
                            | Byte _ -> 1u
                            | Short _ -> 2u
                            | Integer _
                            | FieldToken _
                            | MethodToken _
                            | TypeToken _
                            | RawToken _
                            // Initially, all branch targets are assumed to be 4 bytes large, as it simplifies processing.
                            | BranchTarget _ -> 4u
                    | InstructionOrLabel.Label label ->
                        resolvedLabels.Add(label, size) // TODO: How to handle erroneous duplication of a label?
                        unresolvedLabels.Remove label |> ignore

            // TODO: Adjust size here after labels are done to shorten some branches into 1-byte offsets.

            if unresolvedLabels.Count > 0 then noImpl "TODO: How to handle unresolved labels?"
            
            let inline isTinyBody() =
                body.Header.LocalVariables.IsNull &&
                size <= MethodBody.MaxTinyBodySize &&
                uint16 maxStack <= MethodBody.MaxTinyStackCount

            let entry =
                { MethodBodyEntry.Kind = if isTinyBody() then Tiny else Fat
                  MaxStack = maxStack
                  CodeSize = size
                  Instructions = instructions
                  LabelLookup = resolvedLabels }

            bodies.Add(body, entry)
            struct(maxStack, size)

    member _.WriteTo(section: byref<ChunkedMemoryBuilder>) =
        let branchTargetOffsets = ImmutableArray.CreateBuilder<BranchTargetOffset>()

        for KeyValue(body, entry) in bodies do
            noImpl "TODO: Write headers"

            let start = section.Length
            // Difference between the actual offsets of the instructions from the start of the method and the recorded offsets.
            let mutable diff = 0u

            branchTargetOffsets.Clear()

            if branchTargetOffsets.Capacity < entry.LabelLookup.Count then
                branchTargetOffsets.Capacity <- entry.LabelLookup.Count

            for i = 0 to entry.Instructions.Count - 1 do
                let instruction = &entry.Instructions.ItemRef i

                match Opcode.size instruction.Opcode with
                | 1u -> section.Write(uint8 instruction.Opcode)
                | 2u
                | _ ->
                    section.Write(uint8(instruction.Opcode >>> 8))
                    section.Write(uint8(uint16 instruction.Opcode &&& 0xFFus))

                let pos = section.Length - start

                match instruction.Operand with
                | Nothing -> ()
                | Byte value -> section.Write value
                | Short value -> section.WriteLE value
                | Integer value -> section.WriteLE value
                | RawToken token -> section.WriteLE(uint32 token)
                | FieldToken token -> section.WriteLE(uint32(metadataTokenSource.GetFieldToken token))
                | MethodToken token -> section.WriteLE(uint32(metadataTokenSource.GetMethodToken token))
                | TypeToken token -> section.WriteLE(uint32(metadataTokenSource.GetTypeToken token))
                | BranchTarget _ when isNull entry.LabelLookup ->
                    invalidOp(sprintf "Cannot lookup branch target label at IL_%04X, no labels were defined" pos)
                | BranchTarget(kind, label) ->
                    let target = int64 pos - int64(entry.LabelLookup.[label] - diff)
                    let offset = section

                    match kind with
                    | BranchKind.Shortened when target >= int64 SByte.MinValue || target <= int64 SByte.MaxValue ->
                        section.Write(uint8 target)
                        diff <- diff + 3u // 3 extra bytes removed since 4-byte offset shortened to 1-byte offset.
                    | BranchKind.Shortened
                    | BranchKind.Long ->
                        section.WriteLE(uint64 target)

                    if target > 0L then
                        // Offset will need to be continuously updated until the writer writes its target instruction.
                        branchTargetOffsets.Add { BranchTargetOffset.Offset = offset; Kind = kind; Target = label }

                let mutable targeti = 0
                while i < branchTargetOffsets.Count do
                    let target = &branchTargetOffsets.ItemRef i
                    let label = entry.LabelLookup.[target.Target]
                    let target' = label - diff // TODO: Avoid code duplication with initial calculation of target.
                    let pos = section.Length - start

                    if pos >= label then
                        let target'' = int64 pos - int64 target' // TODO: Avoid code duplication with initial calculation of target.
                        let mutable offset = target.Offset

                        // Some targets might suddenly be eligible to turn into 1-byte offsets, but it might be too much work to
                        // shorten them.
                        match target.Kind with
                        | BranchKind.Shortened -> offset.Write(uint8(Checked.int8 target''))
                        | BranchKind.Long -> offset.WriteLE(uint64 target'')

                        targeti <- targeti + 1
                    else
                        branchTargetOffsets.RemoveAt 0 // TODO: Use some Queue<T> class to make removing branch targets that no longer need to be updated more efficient.

module Instructions =
    module Instruction =
        let inline simple opcode behavior = { Opcode = opcode; StackBehavior = behavior; Operand = Operand.Nothing }
        let inline op opcode = simple opcode (StackBehavior.PopOrPush 0y)
        let inline pushes1 opcode = simple opcode (StackBehavior.PopOrPush 1y)
        let inline pops1 opcode = simple opcode (StackBehavior.PopOrPush -1y)

        let inline branching opcode behavior kind target =
            { Opcode = opcode
              StackBehavior = behavior
              Operand = Operand.BranchTarget(kind, target) }

    open Instruction

    let nop = op Opcode.Nop
    let ``break`` = op Opcode.Break
    let ldarg_0 = pushes1 Opcode.Ldarg_0
    let ldarg_1 = pushes1 Opcode.Ldarg_1
    let ldarg_2 = pushes1 Opcode.Ldarg_2
    let ldarg_3 = pushes1 Opcode.Ldarg_3
    let ldloc_0 = pushes1 Opcode.Ldloc_0
    let ldloc_1 = pushes1 Opcode.Ldloc_1
    let ldloc_2 = pushes1 Opcode.Ldloc_2
    let ldloc_3 = pushes1 Opcode.Ldloc_3
    let stloc_0 = pops1 Opcode.Stloc_0
    let stloc_1 = pops1 Opcode.Stloc_1
    let stloc_2 = pops1 Opcode.Stloc_2
    let stloc_3 = pops1 Opcode.Stloc_3

    let inline ldarg_s num =
        { Opcode = Opcode.Ldarg_s
          StackBehavior = StackBehavior.PopOrPush 1y
          Operand = Operand.Byte num }

    module Shortened = ()

module FSharpIL.Writing.Cil

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli
open FSharpIL.Cli.TypeSystem

open FSharpIL
open FSharpIL.Utilities

[<Struct; RequireQualifiedAccess>]
type LocalVariables =
    | Token of index: TableIndex<StandaloneSigRow>
    | Locals of ImmutableArray<LocalType>

    member this.IsNull =
        match this with
        | Token token -> token.IsNull
        | Locals locals -> locals.IsDefaultOrEmpty

module LocalVariables =
    let Null = LocalVariables.Token { TableIndex = 0u }

[<Struct>]
type MethodHeader =
    { InitLocals: InitLocals
      MaxStack: MaxStack voption
      LocalVariables: LocalVariables }

[<Sealed>]
type Label () =
    interface IEquatable<Label> with member this.Equals other = Object.ReferenceEquals(this, other)

[<Struct; RequireQualifiedAccess>]
type BranchKind = | Short | Long

type Operand =
    | Nothing
    | Byte of uint8
    | Short of uint16
    | Integer of uint32
    | RawToken of MetadataToken
    | FieldToken of FieldTok
    | MethodToken of MethodTok
    | TypeToken of TypeTok
    | StringToken of ReadOnlyMemory<char>
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
        | RawToken token -> sprintf "/* %08X */" (uint32 token)
        | BranchTarget _ -> sprintf "LABEL"
        | StringToken str -> String.Concat("\"", String str.Span, "\"")

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
    let [<Literal>] FatFormatSize = 3u
    let [<Literal>] FatMethodAlignment = 4u

    let inline create initLocals maxStack localVariables instructions =
        { Header = { InitLocals = initLocals; MaxStack = maxStack; LocalVariables = localVariables }
          Instructions = instructions }

    let inline ofSeq instructions = create SkipInitLocals ValueNone LocalVariables.Null [ InstructionBlock.ofSeq instructions ]

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
    abstract GetLocalVariables: ImmutableArray<LocalType> -> TableIndex<StandaloneSigRow>
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: TypeTok -> TypeMetadataToken

[<Sealed>]
type MethodBodyStream () =
    let bodies = Dictionary<MethodBody, MethodBodyEntry>()

    // TODO: Store dictionary of method bodies in CliModuleBuilder, so when DefineMethod is called, it can call this Add method.
    member _.Add body = // TODO: Define exception type for invalid IL.
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

                        // TODO: Check that operands reference valid Local variable indices.

                        stack <-
                            match instruction.StackBehavior with
                            | PopOrPush amt -> Checked.uint16(int32 stack - int32 amt) // TODO: How to deal with stack underflow?

                        if stack > uint16 maxStack then maxStack <- MaxStack stack

                        size <- size +
                            Opcode.size instruction.Opcode +
                            match instruction.Operand with
                            | Nothing -> 0u
                            | Byte _
                            | BranchTarget(BranchKind.Short, _) -> 1u
                            | Short _ -> 2u
                            | Integer _
                            | FieldToken _
                            | MethodToken _
                            | TypeToken _
                            | RawToken _
                            | BranchTarget(BranchKind.Long, _) -> 4u
                    | InstructionOrLabel.Label label ->
                        resolvedLabels.Add(label, size) // TODO: How to handle erroneous duplication of a label?
                        unresolvedLabels.Remove label |> ignore

            if unresolvedLabels.Count > 0 then noImpl "TODO: Throw exception for unresolved labels?"
            
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

    member _.WriteTo(metadataTokenSource: IMetadataTokenSource, section: byref<ChunkedMemoryBuilder>) =
        for KeyValue(body, entry) in bodies do
            match entry.Kind with
            | Tiny ->
                let flags = uint8 ILMethodFlags.TinyFormat ||| (Checked.uint8 entry.CodeSize <<< 2)
                if flags &&& uint8 ILMethodFlags.FatFormat <> 0uy then invalidOp(sprintf "Invalid tiny method flags %A" flags)
                section.Write flags
            | Fat ->
                let mutable flags' = ILMethodFlags.FatFormat

                match body.Header.InitLocals with
                | InitLocals -> flags' <- flags' ||| ILMethodFlags.InitLocals
                | SkipInitLocals -> ()

                //if has extra data sections then flags' <- flags' ||| ILMethodFlags.MoreSects

                section.AlignTo(int32 MethodBody.FatMethodAlignment) // Padding
                section.WriteLE(uint16 flags' ||| (uint16 MethodBody.FatFormatSize <<< 12)) // Flags & size
                section.WriteLE(uint16 entry.MaxStack)
                section.WriteLE entry.CodeSize

                let locals =
                    match body.Header.LocalVariables with
                    | LocalVariables.Token token -> token
                    | LocalVariables.Locals locals -> metadataTokenSource.GetLocalVariables locals

                section.WriteLE(uint32(MetadataToken(MetadataTokenType.StandaloneSig, locals.TableIndex)))

                // TODO: Write extra data sections for fat method.

            let start = section.Length

            for i = 0 to entry.Instructions.Count - 1 do
                let instruction = &entry.Instructions.ItemRef i

                match Opcode.size instruction.Opcode with
                | 1u -> section.Write(uint8 instruction.Opcode)
                | 2u
                | _ ->
                    section.Write(uint8(instruction.Opcode >>> 8))
                    section.Write(uint8(uint16 instruction.Opcode &&& 0xFFus))

                match instruction.Operand with
                | Nothing -> ()
                | Byte value -> section.Write value
                | Short value -> section.WriteLE value
                | Integer value -> section.WriteLE value
                | RawToken token -> section.WriteLE(uint32 token)
                | FieldToken token -> section.WriteLE(uint32(metadataTokenSource.GetFieldToken token))
                | MethodToken token -> section.WriteLE(uint32(metadataTokenSource.GetMethodToken token))
                | TypeToken token -> section.WriteLE(uint32(metadataTokenSource.GetTypeToken token))
                | BranchTarget(kind, target) ->
                    // TODO: Have error checking in Add method to prevent invalid short branch targets.
                    let target =
                        let size =
                            match kind with
                            | BranchKind.Short -> 1L
                            | BranchKind.Long -> 4L

                        int64(section.Length - start) + size - int64 entry.LabelLookup.[target]

                    match kind with
                    | BranchKind.Short -> section.Write(uint8(Checked.int8 target))
                    | BranchKind.Long -> section.WriteLE(uint32 target)

module Instructions =
    module Instruction =
        let simple opcode behavior = { Opcode = opcode; StackBehavior = behavior; Operand = Operand.Nothing }
        let inline op opcode = simple opcode (StackBehavior.PopOrPush 0y)
        let inline pushes1 opcode = simple opcode (StackBehavior.PopOrPush 1y)
        let inline pops1 opcode = simple opcode (StackBehavior.PopOrPush -1y)

        let methodTokenCall opcode method =
            { Opcode = opcode
              Operand = Operand.MethodToken method
              StackBehavior =
                let pops = Checked.int8 method.Member.ParameterTypes.Length
                let pushes = if method.Member.HasReturnValue then 1y else 0y
                PopOrPush(pushes - pops) }

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

    let ldarg_s num =
        { Opcode = Opcode.Ldarg_s
          StackBehavior = StackBehavior.PopOrPush 1y
          Operand = Operand.Byte num }

    let call method = methodTokenCall Opcode.Call method

    let ret = op Opcode.Ret

    let callvirt method = methodTokenCall Opcode.Callvirt method

    module Ldstr =
        let ofOffset { UserStringOffset = offset } =
            { pushes1 Opcode.Ldstr with Operand = Operand.RawToken(MetadataToken(MetadataTokenType.UserStringHeap, offset)) }

        let ofMemory (str: inref<_>) = { pushes1 Opcode.Ldstr with Operand = Operand.StringToken str }

        let ofString (str: string) = { pushes1 Opcode.Ldstr with Operand = Operand.StringToken(str.AsMemory()) }

    let inline ldstr str = Ldstr.ofString str

    module Shortened = ()

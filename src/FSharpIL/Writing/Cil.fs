module FSharpIL.Writing.Cil

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL.Utilities

[<Struct>]
type MethodHeader =
    { InitLocals: InitLocals
      MaxStack: MaxStack voption
      LocalVariables: TableIndex<StandaloneSigRow> }

[<Sealed>]
type Label () =
    interface IEquatable<Label> with member this.Equals other = Object.ReferenceEquals(this, other)

type Operand =
    | Nothing
    | Byte of uint8
    | Short of uint16
    | Integer of uint32
    | FieldToken of FieldTok
    | MethodToken of MethodTok
    | TypeToken of TypeTok
    | BranchTarget of Label

type StackBehavior =
    | PopOrPush of int8

[<Struct>]
type Instruction private (opcode: Opcode, operand: Operand, stackBehavior: StackBehavior, label: Label) =
    member _.Opcode = opcode
    member _.Operand = operand
    member _.StackBehavior = stackBehavior
    // TODO: Instead of destination Instruction instance having label, have branch Instruction instance have Destination
    member _.Label = if Object.ReferenceEquals(null, label) then ValueNone else ValueSome label

    //member WithLabel() = // TODO: This won't work to prevent duplicates, since struct copying will allow copying of whole struct anyway, and using one Instruction from another method body stream will break things anyway.

type MethodBody = { Header: MethodHeader; Instructions: seq<Instruction> }

module MethodBody =
    let [<Literal>] MaxTinyBodySize = 63u
    let [<Literal>] MaxTinyStackCount = 8us
    /// The size of the fat format header, as a count of 4-byte integers (II.25.4.3).
    let [<Literal>] FatFormatSize = 3u
    let [<Literal>] FatMethodAlignment = 4u

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type MethodBodyKind = | Tiny of size: uint8 | Fat

[<Sealed>]
type MethodBodyStream () = // TODO: Have parameter for function/object that validates MethodBody.
    let bodies = Dictionary<MethodBody, MethodBodyKind>()

    member this.Add body = // TODO: Return a Result
        match bodies.TryGetValue body with
        | true, _ -> ()
        | false, _ ->
            let inline tinyMaxStack() =
                match body.Header.MaxStack with
                | ValueSome mstack -> uint16 mstack <= MethodBody.MaxTinyStackCount
                | ValueNone -> true

            // Smallest instruction is 1 byte long, so this is a shortcut to check if the method should be in the fat format.
            let inline tinyInstrCount() =
                let count =
                    match body.Instructions with
                    | :? (Instruction[]) as arr -> ValueSome arr.Length
                    | :? ICollection<Instruction> as col -> ValueSome col.Count
                    | :? IReadOnlyCollection<Instruction> as col -> ValueSome col.Count
                    | _ -> ValueNone

                count <= ValueSome(int32 MethodBody.MaxTinyStackCount)

            // TODO: Convert seq<Instruction> to ImmutableArray<Instruction>.Builder anyway, since whole instruction sequence might need to be enumerated in order to make sure all branch instructions go somewhere.
            if body.Header.LocalVariables.IsNull && tinyMaxStack() && tinyInstrCount() then
                let mutable size = 0u
                let mutable labels = Unchecked.defaultof<Dictionary<Label, uint32>>

                use enumerator = body.Instructions.GetEnumerator()

                while enumerator.MoveNext() && size <= uint32 MethodBody.MaxTinyStackCount do
                    let instr = enumerator.Current

                    let operand =
                        match instr.Operand with
                        | Nothing -> 0u
                        | BranchTarget label ->
                            if labels = null then labels <- Dictionary()
                            ()

                    size <- size + Opcode.size instr.Opcode + operand

                    noImpl "TODO: Increment size"

                bodies.Add(body, if size <= uint32 MethodBody.MaxTinyStackCount then Tiny(Checked.uint8 size) else Fat)
            else
                // Safe to assume that the method would need to be in the fat format.
                bodies.Add(body, Fat)

    member _.WriteTo(section: byref<FSharpIL.ChunkedMemoryBuilder>) =
        noImpl "cannot serialize method bodies yet": unit

type IMetadataTokenSource =
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: FSharpIL.Cli.TypeSystem.TypeTok -> TypeMetadataToken

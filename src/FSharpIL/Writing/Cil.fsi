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

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Operand =
    | Nothing
    | Byte of uint8
    | Short of uint16
    | Integer of uint32
    | FieldToken of FieldTok
    | MethodToken of MethodTok
    | TypeToken of FSharpIL.Cli.TypeSystem.TypeTok
    | BranchTarget of Label

    interface IEquatable<Operand>

[<IsReadOnly; Struct; NoComparison; NoEquality; RequireQualifiedAccess>]
type StackBehavior =
    | PopOrPush of int8

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Instruction =
    member Opcode: Opcode
    member Operand: Operand
    member Label: Label voption
    member StackBehavior: StackBehavior

    new: opcode: Opcode * operand: Operand * StackBehavior -> Instruction

module Instructions = begin end

[<NoComparison; ReferenceEquality>]
type MethodBody =
    { Header: MethodHeader
      Instructions: seq<Instruction> }

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

    member Add: body: MethodBody -> unit

    member internal WriteTo: section: byref<FSharpIL.ChunkedMemoryBuilder> -> unit

[<Interface>]
type IMetadataTokenSource =
    abstract GetUserString: inref<ReadOnlyMemory<char>> -> UserStringOffset
    abstract GetMethodToken: method: MethodTok -> MethodMetadataToken
    abstract GetFieldToken: field: FieldTok  -> FieldMetadataToken
    abstract GetTypeToken: FSharpIL.Cli.TypeSystem.TypeTok -> TypeMetadataToken

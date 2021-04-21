namespace FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Writing

type internal LocalVariableTag =
    | Type = 0uy
    | TypedByRef = 1uy
    | ByRef = 2uy

[<RequireQualifiedAccess>]
type LocalVariableConstraint =
    | Pinned

/// <summary>Represents a single local variable in a <c>LocalVarSig</c> item (II.23.2.6).</summary>
[<IsReadOnly; Struct>]
type LocalVariable internal
    (
        tag: LocalVariableTag,
        modifiers: ImmutableArray<CustomModifier>,
        constraints: ImmutableArray<LocalVariableConstraint>,
        ltype: IEncodedType
    ) =
    member _.CustomMod = modifiers
    member _.Constraints = constraints
    member internal _.Tag = tag
    member internal _.Type = ltype // LocalType
    // TODO: Create some safe way to refer to a local variable.
    //member _.Position: int
    //member _.Write(writer: FSharpIL.Writing.ChunkWriter)

type MethodLocalVariables = ImmutableArray<LocalVariable>

[<IsReadOnly; Struct>]
type MethodBody (maxStack: uint16, initLocals: bool) =
    new (maxStack) = MethodBody(maxStack, false)

    /// Specifies the maximum "number of items on the operand stack" (II.25.4.3).
    member _.MaxStack = maxStack
    /// <summary>Corresponds to the <c>CorILMethod_InitLocals</c> flag (II.25.4.4).</summary>
    member _.InitLocals = initLocals
    // TODO: Include exception information for method bodies here.

    static member val Default = MethodBody 8us

[<Flags>]
type internal MethodContentFlags =
    | None = 0uy
    | ThrowsExceptions = 1uy

[<Sealed>]
type MethodBodyContent (metadata: obj, usHeap: obj) =
    let mutable writer = Unchecked.defaultof<ChunkWriter>
    let mutable throws = false
    member internal _.Writer = writer
    member _.ThrowsExceptions with get() = throws and internal set value = throws <- value
    member internal _.MetadataTables = metadata
    member internal _.UserStringHeap = usHeap
    member internal this.Reset(content: LinkedListNode<byte[]>) =
        writer <- ChunkWriter content
        this.ThrowsExceptions <- false

type IMethodBody =
    //abstract ImplFlags: MethodImplFlags

    /// <summary>
    /// Gets a value indicating whether or not the method body RVA is <c>0</c>, indicating a value of <see langword="null"/>.
    /// </summary>
    abstract Exists: bool
    /// <summary>
    /// Corresponds to the <c>LocalVarSigTok</c> field, which describes "the layout of the local variables for the method"
    /// (II.25.4.3)
    /// </summary>
    abstract LocalVariables: RawIndex<MethodLocalVariables> voption
    /// <summary>Writes the CLI opcodes that make up the body of the method.</summary>
    /// <exception cref="T:System.NotSupportedException">The method does not have a body.</exception>
    // TODO: Make argument a byref struct to prevent "escaping" of method body writers.
    abstract WriteBody: MethodBodyContent -> MethodBody

/// <summary>Represents a method whose <c>RVA</c> column has a value of zero, indicating that it does not have a body.</summary>
[<Sealed>]
type NullMethodBody private () =
    static member val Instance = NullMethodBody()
    interface IMethodBody with
        member _.Exists = false
        member _.LocalVariables = ValueNone
        member _.WriteBody _ = NotSupportedException "Abstract methods do not have method bodies" |> raise

/// <summary>Represents a non-null method body.</summary>
[<AbstractClass>]
type ConcreteMethodBody (localVarSig) =
    abstract WriteBody: MethodBodyContent -> MethodBody
    member _.LocalVariables = localVarSig
    interface IMethodBody with
        member _.Exists = true
        member _.LocalVariables = localVarSig
        member this.WriteBody content = this.WriteBody content

[<Sealed>]
type internal MutableMethodBody (localVarSig, writer) =
    inherit ConcreteMethodBody(localVarSig)
    let mutable writer' = writer
    new(localVarSig) = MutableMethodBody(localVarSig, fun _ -> invalidOp "The method body was not initialized")
    new() = MutableMethodBody ValueNone
    member _.SetBody writer = writer' <- writer
    override _.WriteBody content = writer' content

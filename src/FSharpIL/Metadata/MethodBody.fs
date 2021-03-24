namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

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

[<IsReadOnly; Struct>]
type MethodBody = // TODO: Make this a normal struct instead.
    { /// Specifies the maximum "number of items on the operand stack" (II.25.4.3).
      MaxStack: uint16
      // TODO: Include exception information and local variable information for method bodies here.
      /// <summary>Corresponds to the <c>CorILMethod_InitLocals</c> flag (II.25.4.4).</summary>
      InitLocals: bool
      /// <summary>
      /// Corresponds to the <c>LocalVarSigTok</c> field, which describes "the layout of the local variables for the method"
      /// (II.25.4.3)
      /// </summary>
      LocalVariableSignature: ImmutableArray<LocalVariable> voption }

[<AbstractClass>]
type MethodBodyContent internal (writer: FSharpIL.Writing.ChunkWriter) =
    member internal _.Writer = writer

type IMethodBody =
    abstract Exists: bool
    /// <summary>Writes the CLI opcodes that make up the body of the method.</summary>
    /// <exception cref="T:System.NotSupportedException">The method does not have a body.</exception>
    // TODO: Make argument a byref struct to prevent "escaping" of method body writers.
    abstract WriteBody: MethodBodyContent -> MethodBody

/// <summary>Represents a method whose <c>RVA</c> column has a value of zero, indicating that it does not have a body.</summary>
[<Sealed>]
type NullMethodBody internal () =
    interface IMethodBody with
        member _.Exists = false
        member _.WriteBody _ = NotSupportedException "Abstract methods do not have method bodies" |> raise

/// <summary>Represents a non-null method body.</summary>
[<AbstractClass>]
type ConcreteMethodBody () =
    abstract WriteBody: MethodBodyContent -> MethodBody
    interface IMethodBody with
        member _.Exists = true
        member this.WriteBody content = this.WriteBody content

[<Sealed>]
type internal MutableMethodBody (writer) =
    inherit ConcreteMethodBody()
    let mutable writer' = writer
    new() = MutableMethodBody(fun _ -> invalidOp "The method body was not initialized")
    member _.SetBody writer = writer' <- writer
    override _.WriteBody content = writer' content

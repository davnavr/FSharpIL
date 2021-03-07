namespace FSharpIL.Metadata

open System

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type MethodBody =
    { /// Specifies the maximum "number of items on the operand stack" (II.25.4.3).
      MaxStack: uint16
      // TODO: Include exception information and local variable information for method bodies here.
      /// <summary>Corresponds to the <c>CorILMethod_InitLocals</c> flag (II.25.4.4).</summary>
      InitLocals: bool }

[<AbstractClass>]
type MethodBodyContent internal (writer: FSharpIL.Writing.ChunkWriter) =
    member internal _.Writer = writer

type IMethodBody =
    abstract Exists: bool
    /// <summary>Writes the CLI opcodes that make up the body of the method.</summary>
    /// <exception cref="T:System.NotSupportedException">The method does not have a body.</exception>
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

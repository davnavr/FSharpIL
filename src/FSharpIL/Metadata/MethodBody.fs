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
type ConcreteMethodBody internal () =
    abstract WriteBody: MethodBodyContent -> MethodBody
    interface IMethodBody with
        member _.Exists = true
        member this.WriteBody content = this.WriteBody content

// TODO: Move this module to another file.
[<RequireQualifiedAccess>]
module MethodBody =
    let create (writer: MethodBodyContent -> MethodBody) =
        { new ConcreteMethodBody() with member _.WriteBody content = writer content }
    /// Represents a method body that does not exist, used for abstract methods.
    let none = NullMethodBody()
    ///// A method body containing a single <c>ret</c> instruction.
    //let empty = { new IMethodBody with

namespace FSharpIL.Metadata

open System

[<AbstractClass>]
type MethodBodyContent internal (writer: FSharpIL.Writing.ChunkWriter) =
    member internal _.Writer = writer

type IMethodBody =
    abstract Exists: bool
    /// <summary>Writes the CLI opcodes that make up the body of the method.</summary>
    /// <exception cref="T:System.NotSupportedException">The method does not have a body.</exception>
    abstract WriteBody: MethodBodyContent -> unit

/// <summary>Represents a method whose <c>RVA</c> column has a value of zero, indicating that it does not have a body.</summary>
[<Sealed>]
type NullMethodBody internal () =
    interface IMethodBody with
        member _.Exists = false
        member _.WriteBody _ = NotSupportedException "Abstract methods do not have method bodies" |> raise

[<RequireQualifiedAccess>]
module MethodBody =
    let create (writer: MethodBodyContent -> unit) =
        { new IMethodBody with
            member _.Exists = true
            member _.WriteBody content = writer content }
    let none = NullMethodBody()

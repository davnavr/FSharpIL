[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.MethodBody

let create (writer: MethodBodyContent -> MethodBody) =
    { new ConcreteMethodBody() with member _.WriteBody content = writer content }

/// Represents a method body that does not exist, used for abstract methods.
let none = NullMethodBody()

/// <summary>A method body containing a single <c>ret</c> instruction.</summary>
let empty =
    { new ConcreteMethodBody() with
        member _.WriteBody content =
            let writer = MethodBodyWriter content
            writer.Ret()
            { MaxStack = 0us; InitLocals = false } }

/// <remarks>
/// This method can be used to create recursive methods.
/// </remarks>
let mutableBody() = let body = MutableMethodBody() in body :> ConcreteMethodBody, body.SetBody

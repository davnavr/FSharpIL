[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.MethodBody

let create localVarSig (writer: MethodBodyContent -> MethodBody) =
    { new ConcreteMethodBody(localVarSig) with member _.WriteBody content = writer content }

/// Represents a method body that does not exist, used for abstract methods.
let none = NullMethodBody.Instance

/// <summary>A method body containing a single <c>ret</c> instruction.</summary>
let empty localVarSig =
    { new ConcreteMethodBody(localVarSig) with
        member _.WriteBody content =
            let writer = MethodBodyWriter content
            writer.Ret()
            MethodBody() }

/// <remarks>
/// This method can be used to create recursive methods.
/// </remarks>
let mutableBody() = let body = MutableMethodBody() in body :> ConcreteMethodBody, body.SetBody

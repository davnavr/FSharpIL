namespace FSharpIL.Metadata

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticMethod =
    // TODO: Prevent enums from having static methods.
    let tryAddRow builder owner (method: inref<StaticMethod>) =
        let method' = method.Definition()
        Unsafe.tryAddMethodDefRow<StaticMethod> builder owner &method'

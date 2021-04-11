namespace FSharpIL.Metadata

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticMethod =
    let tryAddRow builder (StaticMemberParent owner) (method: inref<StaticMethod>) =
        let method' = method.Definition()
        Unsafe.tryAddMethodDefRow<StaticMethod> builder owner &method'
    let addRow builder owner (method: inref<_>) = tryAddRow builder owner &method |> ValidationError.check

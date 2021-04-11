namespace FSharpIL.Metadata

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module StaticMethod =
    let methodIndex (method: RawIndex<StaticMethod>) = method.ChangeTag<MethodDefRow>()
    // TODO: Should inref be used for adding methods? It may not be necessary since these methods are inlined.
    let inline tryAddRow builder (StaticMemberParent owner) (method: StaticMethod) =
        let method' = method.Definition()
        Unsafe.tryAddMethodDefRow<StaticMethod> builder owner &method'
    let inline addRow builder owner method = tryAddRow builder owner method |> ValidationError.check

[<AutoOpen>]
module FSharpIL.Metadata.Tables.CodedIndexPatterns

let inline invalidCodedIndex tag = failwithf "Invalid coded index tag %A" tag

[<RequireQualifiedAccess>]
module ResolutionScope =
    let inline (|Null|TypeRef|ModuleRef|Module|AssemblyRef|) (rscope: ResolutionScope) =
        match rscope.Tag with
        | _ when rscope.IsNull -> Null
        | ResolutionScopeTag.TypeRef -> TypeRef(TableIndex.ofIntUnsafe<TypeRefRow> rscope.Index)
        | ResolutionScopeTag.ModuleRef -> ModuleRef(TableIndex.ofIntUnsafe<ModuleRefRow> rscope.Index)
        | ResolutionScopeTag.Module -> Module(TableIndex.ofIntUnsafe<ModuleRow> rscope.Index)
        | ResolutionScopeTag.AssemblyRef -> AssemblyRef(TableIndex.ofIntUnsafe<AssemblyRefRow> rscope.Index)
        | bad -> invalidCodedIndex bad

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.ConstantParent

let (|Field|Param|Property|) (parent: ConstantParent) =
    match parent.Tag with
    | ConstantParentTag.Field -> Field(parent.ToRawIndex<FieldRow>())
    | ConstantParentTag.Param -> Param(parent.ToRawIndex<ParamRow>())
    | ConstantParentTag.Property -> Property(parent.ToRawIndex<PropertyRow>())
    | _ -> invalidArg "parent" "Invalid constant parent tag"

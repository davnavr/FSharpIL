namespace FSharpIL.Metadata

type internal IMethodSpec = interface end

type MethodDefOrRefTag =
    | Def = 0uy
    | RefDefault = 1uy
    | RefGeneric = 2uy
    | RefVarArg = 3uy

type MethodDefOrRef = TaggedIndex<MethodDefOrRefTag>

[<RequireQualifiedAccess>]
module MethodDefOrRef =
    let (|Def|RefDefault|RefGeneric|RefVarArg|) (index: MethodDefOrRef) =
        match index.Tag with
        | MethodDefOrRefTag.Def -> Def(index.ToRawIndex<MethodDefRow>())
        | MethodDefOrRefTag.RefGeneric -> RefGeneric(index.ToRawIndex<MethodRefGeneric>())
        | MethodDefOrRefTag.RefVarArg -> RefVarArg(index.ToRawIndex<MethodRefVarArg>())
        | MethodDefOrRefTag.RefDefault
        | _ -> RefDefault(index.ToRawIndex<MethodRefDefault>())

    let Def (index: RawIndex<MethodDefRow>) = index.ToTaggedIndex MethodDefOrRefTag.Def
    let RefDefault (index: RawIndex<MethodRefDefault>) = index.ToTaggedIndex MethodDefOrRefTag.RefDefault
    let RefGeneric (index: RawIndex<MethodRefGeneric>) = index.ToTaggedIndex MethodDefOrRefTag.RefGeneric
    let RefVarArg (index: RawIndex<MethodRefVarArg>) = index.ToTaggedIndex MethodDefOrRefTag.RefVarArg

/// <summary>Represents a row in the <c>MethodSpec</c> table (II.22.29).</summary>
[<System.Runtime.CompilerServices.IsReadOnly>]
[<NoComparison; StructuralEquality>]
type MethodSpecRow = struct
    val Method: MethodDefOrRef
    val internal Item: IMethodSpec // Instantiation
    internal new (method, instantiation) = { Method = method; Item = instantiation }
end

/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodSpecError (methodSpec: MethodSpecRow) =
    inherit ValidationError()
    member _.MethodSpec = methodSpec

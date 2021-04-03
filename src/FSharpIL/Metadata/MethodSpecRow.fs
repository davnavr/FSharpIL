namespace FSharpIL.Metadata

open System.Runtime.CompilerServices

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

[<IsReadOnly; Struct>]
type MethodSpecBlob internal (index: int32) =
    member internal _.Index = index

/// <summary>Represents a row in the <c>MethodSpec</c> table (II.22.29).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type MethodSpecRow (method: MethodDefOrRef, instantiation: MethodSpecBlob) =
    member _.Method = method
    member _.Instantiation = instantiation

/// <category>Errors</category>
[<Sealed>]
type DuplicateMethodSpecError (methodSpec: MethodSpecRow) =
    inherit ValidationError()
    member _.MethodSpec = methodSpec

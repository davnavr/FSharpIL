namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Printf

open FSharpIL

type MemberRefParentTag =
    | TypeDef = 0uy
    | TypeRef = 1uy
    | ModuleRef = 2uy
    | MethodDef = 3uy
    | TypeSpec = 4uy

type MemberRefParent = TaggedIndex<MemberRefParentTag>

[<RequireQualifiedAccess>]
module MemberRefParent =
    let (|TypeDef|TypeRef|ModuleRef|MethodDef|TypeSpec|) (parent: MemberRefParent) =
        match parent.Tag with
        | MemberRefParentTag.TypeRef -> TypeRef(parent.ToRawIndex<TypeRef>())
        | MemberRefParentTag.ModuleRef -> ModuleRef(parent.ToRawIndex<ModuleRef>())
        | MemberRefParentTag.MethodDef -> MethodDef(parent.ToRawIndex<MethodDefRow>())
        | MemberRefParentTag.TypeSpec -> TypeSpec(parent.ToRawIndex<TypeSpecRow>())
        | MemberRefParentTag.TypeDef
        | _ -> TypeDef(parent.ToRawIndex<TypeDefRow>())

    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex MemberRefParentTag.TypeRef
    let TypeDef (index: RawIndex<TypeDefRow>) = index.ToTaggedIndex MemberRefParentTag.TypeDef
    let TypeSpec (index: RawIndex<TypeSpecRow>) = index.ToTaggedIndex MemberRefParentTag.TypeSpec
    let ModuleRef (index: RawIndex<ModuleRef>) = index.ToTaggedIndex MemberRefParentTag.ModuleRef

[<NoComparison; StructuralEquality>]
type MemberRef<'Signature> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: Blob<'Signature> }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal CallingConvention =
    let flags hasThis explicitThis genParamCount varArgsEmpty =
        let mutable value = CallingConvention.Default
        if hasThis then value <- value ||| CallingConvention.HasThis
        if explicitThis then value <- value ||| CallingConvention.ExplicitThis
        if genParamCount > 0u then value <- value ||| CallingConvention.Generic
        if not varArgsEmpty then value <- value ||| CallingConvention.VarArg
        value

/// <summary>
/// Represents a default <c>MethodRefSig</c> item (II.23.2.1), which "provides the call site Signature for a method" (II.23.2.2).
/// </summary>
[<IsReadOnly>]
type MethodRefDefaultSignature = struct
    val HasThis: bool
    val ExplicitThis: bool
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>

    new (hasThis, explicitThis, retType, parameters: ImmutableArray<_>) =
        { HasThis = hasThis
          ExplicitThis = explicitThis
          ReturnType = retType
          Parameters = parameters }

    new (hasThis, explicitThis, retType, [<System.ParamArray>] parameters: ParamItem[]) =
        MethodRefDefaultSignature(hasThis, explicitThis, retType, parameters.ToImmutableArray())

    new (retType, parameters: ImmutableArray<_>) =
        MethodRefDefaultSignature(false, false, retType, parameters)

    member internal this.CallingConventions = CallingConvention.flags this.HasThis this.ExplicitThis 0u true
end

/// <summary>Represents a generic <c>MethodRefSig</c> item (II.23.2.1).</summary>
[<IsReadOnly>]
type MethodRefGenericSignature = struct
    val HasThis: bool
    val ExplicitThis: bool
    /// Gets the number of generic parameters.
    val GenParamCount: uint32
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>

    new (hasThis, explicitThis, genParamCount, retType, parameters) =
        { HasThis = hasThis
          ExplicitThis = explicitThis
          GenParamCount = genParamCount
          ReturnType = retType
          Parameters = parameters }

    new (genParamCount, retType, parameters) = MethodRefGenericSignature(false, false, genParamCount, retType, parameters)

    member internal this.CallingConventions = CallingConvention.flags this.HasThis this.ExplicitThis this.GenParamCount true
end

/// <summary>Represents a <c>MethodRefSig</c> with a <c>VARARG</c> calling convention (II.23.2.2).</summary>
type MethodRefVarArgSignature = struct
    val HasThis: bool
    val ExplicitThis: bool
    val ReturnType: ReturnTypeItem
    val Parameters: ImmutableArray<ParamItem>
    val VarArgParameters: ImmutableArray<ParamItem>

    new (hasThis, explicitThis, retType, parameters, varArgParameters) =
        { HasThis = hasThis
          ExplicitThis = explicitThis
          ReturnType = retType
          Parameters = parameters
          VarArgParameters = varArgParameters }

    new (hasThis, explicitThis, retType, parameters) = MethodRefVarArgSignature(hasThis, explicitThis, retType, parameters, ImmutableArray.Empty)
    new (retType, parameters, varArgParameters) = MethodRefVarArgSignature(false, false, retType, parameters, varArgParameters)
    new (retType, parameters) = MethodRefVarArgSignature(retType, parameters, ImmutableArray.Empty)

    /// <summary>Gets the total number of parameters and <c>VARARG</c> arguments.</summary>
    /// <remarks>This holds the total number of <c>Param</c> items before and after the sentinel byte.</remarks>
    member this.ParamCount = uint32 (this.Parameters.Length + this.VarArgParameters.Length)

    member internal this.CallingConventions = CallingConvention.flags this.HasThis this.ExplicitThis 0u this.VarArgParameters.IsEmpty
end

type MethodRefDefault = MemberRef<MethodRefDefaultSignature>
type MethodRefGeneric = MemberRef<MethodRefGenericSignature>
type MethodRefVarArg = MemberRef<MethodRefVarArgSignature>
type FieldRef = MemberRef<FieldSignature>

type internal MemberRefSignatureTag =
    | MethodDefault = 1uy
    | MethodGeneric = 2uy
    | MethodVarArg = 3uy
    | Field = 4uy

// TODO: Rename to MethodRefSignatureBlob?
[<IsReadOnly; Struct>]
type MethodRefSignature internal (tag: MemberRefSignatureTag, index: int32) =
    member internal _.Tag = tag
    member internal _.Index = index
    new (blob: Blob<MethodRefDefaultSignature>) = MethodRefSignature(MemberRefSignatureTag.MethodDefault, blob.Index)
    new (blob: Blob<MethodRefGenericSignature>) = MethodRefSignature(MemberRefSignatureTag.MethodGeneric, blob.Index)
    new (blob: Blob<MethodRefVarArgSignature>) = MethodRefSignature(MemberRefSignatureTag.MethodVarArg, blob.Index)
    member internal this.AsBlob<'Item>() = Blob<'Item> this.Index

// TODO: Rename to MemberRefSignatureBlob?
/// <summary>Represents the signature of a <c>MethodRef</c> or <c>FieldRef</c> (II.23.2.2).</summary>
[<IsReadOnly>]
type MemberRefSignature = struct
    val internal Tag: MemberRefSignatureTag
    val internal Index: int32
    internal new (tag, index) = { Tag = tag; Index = index }
    new (blob: MethodRefSignature) = MemberRefSignature(blob.Tag, blob.Index)
    new (blob: Blob<FieldSignature>) = MemberRefSignature(MemberRefSignatureTag.Field, blob.Index)
    member internal this.AsBlob<'Item>() = Blob<'Item> this.Index
end

[<RequireQualifiedAccess>]
module MethodRefSignature =
    let (|Default|Generic|VarArg|) (signature: MethodRefSignature) =
        match signature.Tag with
        | MemberRefSignatureTag.MethodDefault -> Default(signature.AsBlob<MethodRefDefaultSignature>())
        | MemberRefSignatureTag.MethodGeneric -> Generic(signature.AsBlob<MethodRefGenericSignature>())
        | MemberRefSignatureTag.MethodVarArg -> VarArg(signature.AsBlob<MethodRefVarArgSignature>())
        | _ -> invalidArg "signature" "Invalid method reference signature kind"

[<RequireQualifiedAccess>]
module MemberRefSignature =
    let (|MethodRef|FieldRef|) (signature: MemberRefSignature) =
        match signature.Tag with
        | MemberRefSignatureTag.MethodDefault
        | MemberRefSignatureTag.MethodGeneric
        | MemberRefSignatureTag.MethodVarArg -> MethodRef(MethodRefSignature(signature.Tag, signature.Index))
        | MemberRefSignatureTag.Field -> FieldRef(signature.AsBlob<FieldSignature>())
        | _ -> invalidArg "signature" "Invalid member reference signature kind"

    let (|MethodDefault|MethodGeneric|MethodVarArg|Field|) (signature: MemberRefSignature) =
        match signature with
        | MethodRef method ->
            match method with
            | MethodRefSignature.Default signature -> MethodDefault signature
            | MethodRefSignature.Generic signature -> MethodGeneric signature
            | MethodRefSignature.VarArg signature -> MethodVarArg signature
        | FieldRef field -> Field field

// TODO: Figure out if MemberRefRow constructor should be public.
/// <summary>
/// Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
[<IsReadOnly; Struct>]
type MemberRefRow internal (parent: MemberRefParent, name: Identifier, signature: MemberRefSignature) =
    member _.Class = parent
    member _.Name = name
    member _.Signature = signature

/// <summary>
/// Error used when there is a duplicate row in the <c>MemberRef</c> table (6).
/// </summary>
/// <category>Warnings</category>
[<Sealed>]
type DuplicateMemberRefWarning (parent: MemberRefParent, name: Identifier) =
    inherit ValidationWarning()
    member _.Class = parent
    member _.Name = name
    override _.ToString() =
        sprintf
            "A duplicate member reference \"%A\" was added when an existing row with the same class, name, and signature already exists"
            name

[<Sealed>]
type MemberRefTableBuilder internal () =
    let members = RowArrayList<MemberRefRow>.Create()

    member _.Count = members.Count

    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member internal _.Add<'Tag>(row: MemberRefRow) =
        let i, duplicate = members.Add row
        struct(RawIndex<'Tag> i.Value, duplicate)

    member internal this.Add<'Sig, 'Tag>
        (
            tag: MemberRefSignatureTag,
            { Class = parent; MemberName = name; Signature = signature: Blob<'Sig> }: inref<_>
        ) =
        this.Add<'Tag>(MemberRefRow(parent, name, MemberRefSignature(tag, signature.Index)))

    member this.Add(method: MethodRefDefault) = this.Add<_, MethodRefDefault>(MemberRefSignatureTag.MethodDefault, &method)
    member this.Add(method: MethodRefGeneric) = this.Add<_, MethodRefGeneric>(MemberRefSignatureTag.MethodGeneric, &method)
    member this.Add(method: MethodRefVarArg) = this.Add<_, MethodRefVarArg>(MemberRefSignatureTag.MethodVarArg, &method)
    member this.Add(field: FieldRef) = this.Add<_, FieldRef>(MemberRefSignatureTag.Field, &field)

    member internal _.ToImmutable() = members.ToImmutable()

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator

[<Sealed>]
type MethodRefSigBlobLookup internal
    (
        rdefault: MethodRefDefaultSignature[],
        rgeneric: MethodRefGenericSignature[],
        rvararg: MethodRefVarArgSignature[]
    ) =
    member _.Count = rdefault.Length + rgeneric.Length + rvararg.Length
    member _.ItemRef(i: Blob<MethodRefDefaultSignature>): inref<_> = &rdefault.[i.Index]
    member _.ItemRef(i: Blob<MethodRefGenericSignature>): inref<_> = &rgeneric.[i.Index]
    member _.ItemRef(i: Blob<MethodRefVarArgSignature>): inref<_> = &rvararg.[i.Index]
    member this.Item with get (i: Blob<MethodRefDefaultSignature>) = this.ItemRef i
    member this.Item with get (i: Blob<MethodRefGenericSignature>) = this.ItemRef i
    member this.Item with get (i: Blob<MethodRefVarArgSignature>) = this.ItemRef i

[<Sealed>]
type MethodRefSigBlobLookupBuilder internal () =
    let rdefault = Dictionary<MethodRefDefaultSignature, int32>()
    let rgeneric = Dictionary<MethodRefGenericSignature, int32>()
    let rvararg = Dictionary<MethodRefVarArgSignature, int32>()

    member _.Count = rdefault.Count + rgeneric.Count + rvararg.Count
    member _.TryAdd signature = Blob.tryAddTo rdefault signature
    member _.TryAdd signature = Blob.tryAddTo rgeneric signature
    member _.TryAdd signature = Blob.tryAddTo rvararg signature
    member this.GetOrAdd(signature: MethodRefDefaultSignature) = this.TryAdd signature |> Result.any
    member this.GetOrAdd(signature: MethodRefGenericSignature) = this.TryAdd signature |> Result.any
    member this.GetOrAdd(signature: MethodRefVarArgSignature) = this.TryAdd signature |> Result.any
    member internal _.ToImmutable() =
        let rdefault' = Array.zeroCreate rdefault.Count
        for KeyValue(signature, i) in rdefault do
            rdefault'.[i] <- signature

        let rgeneric' = Array.zeroCreate rgeneric.Count
        for KeyValue(signature, i) in rgeneric do
            rgeneric'.[i] <- signature

        let rvararg' = Array.zeroCreate rvararg.Count
        for KeyValue(signature, i) in rvararg do
            rvararg'.[i] <- signature

        MethodRefSigBlobLookup(rdefault', rgeneric', rvararg')

//TODO: Figure out how to allow creation of Blob<MethodRefDefaultSignature>, etc.

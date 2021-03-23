namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Printf

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
type MemberRef<'Signature when 'Signature : equality and 'Signature : struct> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

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

/// <summary>
/// Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
[<NoComparison; StructuralEquality>]
type MemberRefRow =
    | MethodRefDefault of MethodRefDefault
    | MethodRefGeneric of MethodRefGeneric
    | MethodRefVarArg of MethodRefVarArg
    | FieldRef of FieldRef

/// <summary>
/// Error used when there is a duplicate row in the <c>MemberRef</c> table (6).
/// </summary>
/// <category>Warnings</category>
[<Sealed>]
type DuplicateMemberRefWarning (row: MemberRefRow) =
    inherit ValidationWarning()
    member _.Member = row
    override this.ToString() =
        sprintf
            "A duplicate member reference \"%O\" was added when an existing row with the same class, name, and signature already exists"
            this.Member

[<Sealed>]
type MemberRefTableBuilder internal () =
    let members = RowArrayList<MemberRefRow>.Create()

    member _.Count = members.Count

    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member internal _.Add<'MemberRef>(row: MemberRefRow) =
        let i, duplicate = members.Add row
        struct(RawIndex<'MemberRef> i.Value, duplicate)

    member this.Add(method: MethodRefDefault) = this.Add<MethodRefDefault>(MethodRefDefault method)
    member this.Add(method: MethodRefGeneric) = this.Add<MethodRefGeneric>(MethodRefGeneric method)
    member this.Add(method: MethodRefVarArg) = this.Add<MethodRefVarArg>(MethodRefVarArg method)
    member this.Add(field: FieldRef) = this.Add<FieldRef>(FieldRef field)

    member internal _.ToImmutable() = members.ToImmutable()

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator

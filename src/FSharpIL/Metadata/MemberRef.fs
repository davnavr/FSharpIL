namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Printf

[<RequireQualifiedAccess>]
type MemberRefParent =
    // | MethodDef // of ?
    // | ModuleRef // of ?
    // | TypeDef // of ?
    | TypeRef of SimpleIndex<TypeRef>
    | TypeSpec of SimpleIndex<TypeSpecRow>

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | TypeRef tref -> IndexOwner.checkIndex owner tref
            | TypeSpec tref -> IndexOwner.checkIndex owner tref

[<NoComparison; StructuralEquality>]
type MemberRef<'Signature when 'Signature : equality and 'Signature : struct and 'Signature :> IIndexValue> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

    member internal this.CheckOwner owner =
        IndexOwner.checkOwner owner this.Class
        this.Signature.CheckOwner owner

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

    new (hasThis, explicitThis, retType, parameters) =
        { HasThis = hasThis
          ExplicitThis = explicitThis
          ReturnType = retType
          Parameters = parameters }

    new (retType, parameters) = MethodRefDefaultSignature(false, false, retType, parameters)

    member internal this.CallingConventions = CallingConvention.flags this.HasThis this.ExplicitThis 0u true

    interface IIndexValue with
        member this.CheckOwner owner =
            this.ReturnType.CheckOwner owner
            for param in this.Parameters do param.CheckOwner owner
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

    interface IIndexValue with
        member this.CheckOwner owner =
            this.ReturnType.CheckOwner owner
            for param in this.Parameters do param.CheckOwner owner
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

    interface IIndexValue with
        member this.CheckOwner owner =
            this.ReturnType.CheckOwner owner
            for parameter in this.Parameters do parameter.CheckOwner owner
            for parameter in this.VarArgParameters do parameter.CheckOwner owner
end

type MethodRefDefault = MemberRef<MethodRefDefaultSignature>
type MethodRefGeneric = MemberRef<MethodRefGenericSignature>
type MethodRefVarArg = MemberRef<MethodRefVarArgSignature>
// type FieldRef = MemberRef<>

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
    // | FieldRef // of ?

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | MethodRefDefault method -> method.CheckOwner owner
            | MethodRefGeneric method -> method.CheckOwner owner
            | MethodRefVarArg method -> method.CheckOwner owner

type MemberRefIndex<'Member> = TaggedIndex<'Member, MemberRefRow>

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
type MemberRefTable internal (owner: IndexOwner, warnings: ImmutableArray<ValidationWarning>.Builder) =
    let members = List<MemberRefRow>()
    let lookup = HashSet<MemberRefRow>()

    member _.Count = members.Count

    // TODO: Instead of a union type, have two overloaded GetIndex methods for adding MethodRefs and FieldRefs.

    // TODO: Enforce CLS checks.
    // NOTE: Duplicates (based on owning class, name, and signature) are allowed, but produce a warning.
    member private _.GetIndex<'MemberRef>(row: MemberRefRow) =
        IndexOwner.checkOwner owner row

        if lookup.Add row |> not then
            DuplicateMemberRefWarning row |> warnings.Add

        members.Add row
        MemberRefIndex<'MemberRef>(owner, row)

    member this.GetIndex(method: MethodRefDefault) = this.GetIndex<MethodRefDefault>(MethodRefDefault method)
    member this.GetIndex(method: MethodRefGeneric) = this.GetIndex<MethodRefGeneric>(MethodRefGeneric method)
    member this.GetIndex(method: MethodRefVarArg) = this.GetIndex<MethodRefVarArg>(MethodRefVarArg method)
    // member this.GetIndex(field: FieldRef) = this.GetIndex<FieldRef>(FieldRef field)

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator

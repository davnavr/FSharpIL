namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
type MemberRefParent =
    // | MethodDef // of ?
    // | ModuleRef // of ?
    // | TypeDef // of ?
    | TypeRef of SimpleIndex<TypeRef>
    // | TypeSpec // of ?

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | TypeRef tref -> IndexOwner.checkIndex owner tref

type MemberRef<'Signature when 'Signature : equality and 'Signature :> IIndexValue> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

    interface IIndexValue with
        member this.CheckOwner owner =
            IndexOwner.checkOwner owner this.Class
            this.Signature.CheckOwner owner

/// <summary>Represents a <c>MethodRefSig</c>, which "provides the call site Signature for a method" (II.23.2.2).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type MethodRefSignature =
    { HasThis: bool
      ExplicitThis: bool
      /// <summary>Corresponds to the <c>RetType</c> item, which specifies the return type of the method.</summary>
      ReturnType: ReturnTypeItem
      Parameters: ImmutableArray<ParamItem>
      VarArgParameters: ImmutableArray<ParamItem> }

    member this.ParamCount = uint32 (this.Parameters.Length + this.VarArgParameters.Length)

    member internal this.CallingConventions =
        let mutable flags = CallingConvention.Default
        if this.HasThis then flags <- flags ||| CallingConvention.HasThis
        if this.ExplicitThis then flags <- flags ||| CallingConvention.ExplicitThis
        if not this.VarArgParameters.IsEmpty then flags <- flags ||| CallingConvention.VarArg
        flags

    interface IIndexValue with
        member this.CheckOwner owner = failwith ""

type MethodRef = MemberRef<MethodRefSignature>

// type FieldRef = 

/// <summary>
/// Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
[<NoComparison; StructuralEquality>]
type MemberRefRow =
    | MethodRef of MethodRef
    // | FieldRef // of ?

    member this.Class =
        match this with
        | MethodRef { Class = parent } -> parent

    member this.MemberName =
        match this with
        | MethodRef { MemberName = name } -> name

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | MethodRef method -> IndexOwner.checkOwner owner method

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

    member this.GetIndex(method: MethodRef) = this.GetIndex<MethodRef>(MethodRef method)
    // member this.GetIndex(field: FieldRef) = this.GetIndex<FieldRef>(FieldRef field)

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator

namespace FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

// TODO: Create error checks for MethodImpl.

[<IsReadOnly; Struct>]
[<CustomEquality; NoComparison>]
type MethodImpl internal (owner: RawIndex<TypeDefRow>, methodBody: MethodDefOrRef, methodDecl: MethodDefOrRef) =
    member _.Class = owner
    member _.MethodBody = methodBody
    member _.MethodDeclaration = methodDecl
    interface IEquatable<MethodImpl> with
        member _.Equals other = methodDecl = other.MethodDeclaration

type internal MethodImplBody = MethodDefOrRef
type internal MethodImplDecl = MethodDefOrRef

[<Sealed>]
type MethodImplTableBuilder internal () =
    let lookup = Dictionary<RawIndex<TypeDefRow>, Dictionary<MethodImplDecl, MethodImplBody>>()
    let mutable count = 0

    member _.Count = count

    member _.TryAdd(owner, methodBody: MethodDefOrRef, methodDecl: MethodDefOrRef) =
        let impls =
            match lookup.TryGetValue owner with
            | true, existing -> existing
            | false, _ ->
                let empty = Dictionary()
                lookup.[owner] <- empty
                empty
        // TODO: Check that methodDecl belongs to owner.
        if impls.TryAdd(methodDecl, methodBody) then
            count <- count + 1
            true
        else false

    member internal _.ToImmutable() =
        let rows = ImmutableArray.CreateBuilder<MethodImpl> count
        for KeyValue(owner, impls) in lookup do
            for KeyValue(decl, body) in impls do
                rows.Add(MethodImpl(owner, body, decl))
        rows.ToImmutable()

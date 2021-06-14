namespace FSharpIL.Writing.Abstractions

open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables

[<IsReadOnly>]
type TypeVisibility = struct
    val Flags: TypeDefFlags
    val Parent: TableIndex<TypeDefRow>
    internal new (flags, parent) = { Flags = flags; Parent = parent }
    internal new (flags) = { Flags = flags; Parent = Unchecked.defaultof<_> }
    member this.IsNested = not this.Parent.IsNull
end

[<IsReadOnly>]
[<System.ObsoleteAttribute>]
type NestedTypeVisibility = struct
    val Visibility: TypeVisibility
    internal new (flags, parent) = { Visibility = TypeVisibility(flags, parent) }
    member this.Flags = this.Visibility.Flags
    member this.Parent = this.Visibility.Parent
end

[<System.ObsoleteAttribute>]
[<AutoOpen>]
module NestedTypeVisiblity =
    let inline (|NestedPublic|NestedPrivate|NestedFamily|NestedAssembly|NestedFamilyAndAssembly|NestedFamilyOrAssembly|)
        (visibility: NestedTypeVisibility)
        =
        match visibility.Flags with
        | TypeDefFlags.NestedPublic -> NestedPublic visibility.Parent
        | TypeDefFlags.NestedFamily -> NestedFamily visibility.Parent
        | TypeDefFlags.NestedAssembly -> NestedAssembly visibility.Parent
        | TypeDefFlags.NestedFamAndAssem -> NestedFamilyAndAssembly visibility.Parent
        | TypeDefFlags.NestedFamOrAssem -> NestedFamilyOrAssembly visibility.Parent
        | TypeDefFlags.NestedPrivate
        | _ -> NestedPrivate visibility.Parent

[<RequireQualifiedAccess>]
module TypeVisibility =
    let NotPublic = TypeVisibility TypeDefFlags.NotPublic
    let Public = TypeVisibility TypeDefFlags.Public
    let NestedPublic parent = TypeVisibility(TypeDefFlags.NestedPublic, parent)
    let NestedPrivate parent = TypeVisibility(TypeDefFlags.NestedPrivate, parent)
    let NestedFamily parent = TypeVisibility(TypeDefFlags.NestedFamily, parent)
    let NestedAssembly parent = TypeVisibility(TypeDefFlags.NestedAssembly, parent)
    let NestedFamilyAndAssembly parent = TypeVisibility(TypeDefFlags.NestedFamAndAssem, parent)
    let NestedFamilyOrAssembly parent = TypeVisibility(TypeDefFlags.NestedFamOrAssem, parent)

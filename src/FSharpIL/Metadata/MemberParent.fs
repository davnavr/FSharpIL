namespace FSharpIL.Metadata

type StaticMemberParentTag =
    | Unsafe = 0uy
    | ConcreteClass = 1uy
    | AbstractClass = 2uy
    | SealedClass = 3uy
    | StaticClass = 4uy
    | Delegate = 5uy
    | Interface = 6uy
    | Struct = 7uy

/// <summary>Represents a row in the <c>TypeDef</c> table that can own a static field or static method.</summary>
type StaticMemberParent = TaggedIndex<StaticMemberParentTag>

[<RequireQualifiedAccess>]
module StaticMemberParent =
    let ConcreteClass (def: RawIndex<ConcreteClassDef>) = def.ToTaggedIndex StaticMemberParentTag.ConcreteClass
    let AbstractClass (def: RawIndex<AbstractClassDef>) = def.ToTaggedIndex StaticMemberParentTag.AbstractClass
    let SealedClass (def: RawIndex<SealedClassDef>) = def.ToTaggedIndex StaticMemberParentTag.SealedClass
    let StaticClass (def: RawIndex<StaticClassDef>) = def.ToTaggedIndex StaticMemberParentTag.StaticClass
    let Delegate (def: RawIndex<DelegateDef>) = def.ToTaggedIndex StaticMemberParentTag.Delegate
    let Interface (def: RawIndex<InterfaceDef>) = def.ToTaggedIndex StaticMemberParentTag.Interface
    let Struct (def: RawIndex<StructDef>) = def.ToTaggedIndex StaticMemberParentTag.Struct

[<AutoOpen>]
module MemberParentPatterns =
    let (|StaticMemberParent|) (parent: StaticMemberParent) = parent.ToRawIndex<TypeDefRow>()

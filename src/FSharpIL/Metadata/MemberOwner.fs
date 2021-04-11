namespace FSharpIL.Metadata

// TODO: Rename Parent to Owner.
// TODO: Consider removing Unsafe option and just use a function in Unsafe module instead.

type StaticMemberOwnerTag =
    | Unsafe = 0uy
    | ConcreteClass = 1uy
    | AbstractClass = 2uy
    | SealedClass = 3uy
    | StaticClass = 4uy
    | Delegate = 5uy
    | Interface = 6uy
    | Struct = 7uy

/// <summary>Represents a row in the <c>TypeDef</c> table that can own a static field or static method.</summary>
type StaticMemberOwner = TaggedIndex<StaticMemberOwnerTag>

[<RequireQualifiedAccess>]
module StaticMemberOwner =
    let Unsafe (def: RawIndex<TypeDefRow>) = def.ToTaggedIndex StaticMemberOwnerTag.Unsafe
    let ConcreteClass (def: RawIndex<ConcreteClassDef>) = def.ToTaggedIndex StaticMemberOwnerTag.ConcreteClass
    let AbstractClass (def: RawIndex<AbstractClassDef>) = def.ToTaggedIndex StaticMemberOwnerTag.AbstractClass
    let SealedClass (def: RawIndex<SealedClassDef>) = def.ToTaggedIndex StaticMemberOwnerTag.SealedClass
    let StaticClass (def: RawIndex<StaticClassDef>) = def.ToTaggedIndex StaticMemberOwnerTag.StaticClass
    let Delegate (def: RawIndex<DelegateDef>) = def.ToTaggedIndex StaticMemberOwnerTag.Delegate
    let Interface (def: RawIndex<InterfaceDef>) = def.ToTaggedIndex StaticMemberOwnerTag.Interface
    let Struct (def: RawIndex<StructDef>) = def.ToTaggedIndex StaticMemberOwnerTag.Struct

type InstanceMemberOwnerTag =
    | Unsafe = 0uy
    | ConcreteClass = 1uy
    | AbstractClass = 2uy
    | SealedClass = 3uy
    | Delegate = 4uy
    | Struct = 5uy

type InstanceMemberOwner = TaggedIndex<InstanceMemberOwnerTag>

[<RequireQualifiedAccess>]
module InstanceMemberOwner =
    let Struct (def: RawIndex<StructDef>) = def.ToTaggedIndex InstanceMemberOwnerTag.Struct

type AbstractMethodOwnerTag =
    | Unsafe = 0uy
    | AbstractClass = 1uy
    | Interface = 2uy

type AbstractMethodOwner = TaggedIndex<AbstractMethodOwnerTag>

[<RequireQualifiedAccess>]
module AbstractMethodOwner =
    let Unsafe (def: RawIndex<TypeDefRow>) = def.ToTaggedIndex AbstractMethodOwnerTag.Unsafe
    let AbstractClass (def: RawIndex<AbstractClassDef>) = def.ToTaggedIndex AbstractMethodOwnerTag.AbstractClass
    let Interface (def: RawIndex<InterfaceDef>) = def.ToTaggedIndex AbstractMethodOwnerTag.Interface
    //toInstance (parent: AbstractMethodParent): InstanceMemberOwner

[<AutoOpen>]
module MemberOwnerPatterns =
    let inline private helper (parent: TaggedIndex<_>) = parent.ToRawIndex<TypeDefRow>()
    let (|StaticMemberOwner|) (parent: StaticMemberOwner) = helper parent
    let (|AbstractMethodOwner|) (parent: AbstractMethodOwner) = helper parent
    let (|InstanceMemberOwner|) (parent: InstanceMemberOwner) = helper parent

namespace FSharpIL.Metadata

type InterfaceImplementorTag =
    | ConcreteClass = 1uy
    | AbstractClass = 2uy
    | SealedClass = 3uy
    | Interface = 4uy
    | Struct = 5uy

type InterfaceImplementor = TaggedIndex<InterfaceImplementorTag>

[<RequireQualifiedAccess>]
module InterfaceImplementor =
    let ConcreteClass (def: RawIndex<ConcreteClassDef>) = def.ToTaggedIndex InterfaceImplementorTag.ConcreteClass
    let AbstractClass (def: RawIndex<AbstractClassDef>) = def.ToTaggedIndex InterfaceImplementorTag.AbstractClass
    let SealedClass (def: RawIndex<SealedClassDef>) = def.ToTaggedIndex InterfaceImplementorTag.SealedClass
    let Interface (def: RawIndex<InterfaceDef>) = def.ToTaggedIndex InterfaceImplementorTag.Interface
    let Struct (def: RawIndex<StructDef>) = def.ToTaggedIndex InterfaceImplementorTag.Struct

[<AutoOpen>]
module InterfaceImplementorPatterns =
    let (|InterfaceImplementor|) (typeDef: InterfaceImplementor) = typeDef.ToRawIndex<TypeDefRow>()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix); RequireQualifiedAccess>]
module InterfaceImpl =
    let inline addRow (builder: CliMetadataBuilder) (InterfaceImplementor typeDef) intf =
        builder.InterfaceImpl.Add(typeDef, intf)

﻿namespace FSharpIL.Writing.Abstractions

open FSharpIL.Metadata

[<RequireQualifiedAccess>]
type TypeReferenceParent =
    | TypeRef of TypeReference
    //| Assembly of AssemblyReference
    //| Module of ModuleReference

and TypeReference =
    { ResolutionScope: TypeReferenceParent
      TypeName: Identifier
      TypeNamespace: Identifier voption }

[<System.Runtime.CompilerServices.IsReadOnly>]
type TypeReference<'Kind when 'Kind :> TypeKinds.Kind> = struct
    val Reference: TypeReference
    new (reference) = { Reference = reference }
end

type ConcreteClassRef = TypeReference<TypeKinds.ConcreteClass>
type AbstractClassRef = TypeReference<TypeKinds.AbstractClass>
type SealedRef = TypeReference<TypeKinds.SealedClass>
type StaticRef = TypeReference<TypeKinds.StaticClass>
type DelegateRef = TypeReference<TypeKinds.Delegate>
type EnumRef = TypeReference<TypeKinds.Enum>
type InterfaceRef = TypeReference<TypeKinds.Interface>
type ValueTypeRef = TypeReference<TypeKinds.ValueType>

[<AutoOpen>]
module TypeReferencePatterns =
    let inline (|TypeReference|) (typeRef: TypeReference<'Kind>) = typeRef.Reference

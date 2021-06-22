namespace rec FSharpIL.Cli

open FSharpIL.Metadata

[<RequireQualifiedAccess>]
type TypeReferenceParent =
    | TypeRef of TypeReference
    | Assembly of AssemblyReference
    //| Module of ModuleReference

and TypeReference =
    { ResolutionScope: TypeReferenceParent
      TypeName: Identifier
      TypeNamespace: Identifier voption }

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeReference<'Kind when 'Kind :> TypeKinds.Kind> = internal { TypeReference: TypeReference }

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
    val (|TypeReference|) : TypeReference<'Kind> -> TypeReference

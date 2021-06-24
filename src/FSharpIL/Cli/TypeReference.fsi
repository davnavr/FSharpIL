namespace rec FSharpIL.Cli

open System

open FSharpIL.Metadata

type internal ITypeReference = interface
    inherit IEquatable<ITypeReference>
    inherit IComparable<ITypeReference>
    inherit IComparable

    abstract ResolutionScope: TypeReferenceParent
    abstract TypeName: Identifier
    abstract TypeNamespace: Identifier voption
end

[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type TypeReferenceParent =
    | TypeRef of ReferencedType
    | Assembly of AssemblyReference
    //| Module of ModuleReference

[<CustomComparison; CustomEquality>]
type TypeReference<'Kind when 'Kind :> TypeKinds.Kind> =
    { ResolutionScope: TypeReferenceParent
      TypeName: Identifier
      TypeNamespace: Identifier voption }

    member Equals<'OtherKind when 'OtherKind :> TypeKinds.Kind> : other: TypeReference<'OtherKind> -> bool
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface ITypeReference

type ConcreteClassRef = TypeReference<TypeKinds.ConcreteClass>
type AbstractClassRef = TypeReference<TypeKinds.AbstractClass>
type SealedRef = TypeReference<TypeKinds.SealedClass>
type StaticRef = TypeReference<TypeKinds.StaticClass>
type DelegateRef = TypeReference<TypeKinds.Delegate>
type EnumRef = TypeReference<TypeKinds.Enum>
type InterfaceRef = TypeReference<TypeKinds.Interface>
type ValueTypeRef = TypeReference<TypeKinds.ValueType>

[<RequireQualifiedAccess>]
[<CustomComparison; CustomEquality>]
type ReferencedType =
    | ConcreteClass of ConcreteClassRef

    member internal Reference: ITypeReference
    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IComparable

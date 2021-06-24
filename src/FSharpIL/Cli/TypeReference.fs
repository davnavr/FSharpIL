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

[<AutoOpen>]
module TypeReferenceHelpers =
    let inline (|TypeReference|) (def: #ITypeReference) = def :> ITypeReference

    let inline getTypeReference< ^T when ^T : (member internal Reference : ITypeReference)> (case: ^T) =
        (^T : (member internal Reference : ITypeReference) case)

    let inline castTypeReference< ^T when ^T : (member internal Reference : ITypeReference)> (obj: obj) =
        match obj with
        | :? ^T as other -> getTypeReference<'T> other
        | _ -> obj :?> ITypeReference

    let inline equalsTypeReference (current: ^T) (obj: obj) =
        (castTypeReference< ^T> obj).Equals(getTypeReference current)

    let inline compareTypeReference (current: ^T) (obj: obj) =
        (castTypeReference< ^T> obj).CompareTo(getTypeReference current)

[<CustomComparison; CustomEquality>]
type TypeReference<'Kind when 'Kind :> TypeKinds.Kind> =
    { ResolutionScope: TypeReferenceParent
      TypeName: Identifier
      TypeNamespace: Identifier voption }

    member this.Equals(other: TypeReference<_>) = this.Equals(other :> ITypeReference)

    member this.Equals(other: ITypeReference) =
        this.ResolutionScope = other.ResolutionScope
        && this.TypeNamespace = other.TypeNamespace
        && this.TypeName = other.TypeName

    member this.CompareTo(other: ITypeReference) =
        match compare this.ResolutionScope other.ResolutionScope with
        | result -> result

    override this.Equals obj =
        match obj with
        | :? ITypeReference as other -> this.Equals other
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.ResolutionScope, this.TypeNamespace, this.TypeName)

    interface ITypeReference with
        member this.ResolutionScope = this.ResolutionScope
        member this.TypeName = this.TypeName
        member this.TypeNamespace = this.TypeNamespace
        member this.Equals other = this.Equals other
        member this.CompareTo other = this.CompareTo other
        member this.CompareTo(obj: obj) = this.CompareTo(obj :?> ITypeReference)

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

    member this.Reference =
        match this with
        | ConcreteClass(TypeReference tref) -> tref

    override this.GetHashCode() = this.Reference.GetHashCode()
    override this.Equals obj = equalsTypeReference this obj

    interface IComparable with member this.CompareTo obj = compareTypeReference this obj

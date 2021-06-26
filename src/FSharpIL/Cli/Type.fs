namespace rec FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities

[<AbstractClass>]
type Type (typeNamespace: Identifier voption, parent: Type voption, typeName: Identifier) =
    member _.TypeName = typeName
    member _.TypeNamespace = typeNamespace
    member _.EnclosingType = parent

    member _.IsNested = parent.IsSome

    member private this.CaseEquals(other: Type) =
        match this, other with
        | (:? ReferencedType as this'), (:? ReferencedType as other') -> this'.ResolutionScope = other'.ResolutionScope
        | :? DefinedType, :? DefinedType -> true
        | _ -> false

    member this.Equals(other: Type) =
        typeName = other.TypeName && typeNamespace = other.TypeNamespace && this.CaseEquals other && parent = other.EnclosingType

    member _.CompareTo(other: Type) =
        // TODO: Match on the type
        match compare typeNamespace other.TypeNamespace with
        | 0 ->
            match compare typeName other.TypeName with
            | 0 -> compare parent other.EnclosingType
            | result -> result
        | result -> result

    override this.Equals obj =
        match obj with
        | :? Type as other -> this.Equals other
        | _ -> false

    override _.GetHashCode() = HashCode.Combine(typeName, typeNamespace, parent)

    interface IEquatable<Type> with member this.Equals other = this.Equals other
    interface IComparable with member this.CompareTo obj = this.CompareTo(obj :?> Type)
    interface IComparable<Type> with member this.CompareTo other = this.CompareTo other

[<RequireQualifiedAccess>]
module TypeKinds =
    type ConcreteClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = Unchecked.defaultof<_>
    end

    type AbstractClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract
    end

    type SealedClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

    type StaticClass = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract ||| TypeDefFlags.Sealed
    end

    type Delegate = struct
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

    type Enum = struct
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Abstract
    end

    type Interface = struct
        interface TypeAttributes.IHasStaticMethods
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags
                with [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
                    get() = TypeDefFlags.Abstract ||| TypeDefFlags.Interface
    end

    type ValueType = struct
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
        interface IAttributeTag<TypeDefFlags> with
            member _.RequiredFlags with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() = TypeDefFlags.Sealed
    end

[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type TypeReferenceParent =
    | TypeRef of ReferencedType
    | Assembly of AssemblyReference

[<AbstractClass>]
type ReferencedType =
    inherit Type
    val ResolutionScope: TypeReferenceParent

    new (parent: TypeReferenceParent, typeNamespace, typeName) =
        let parent' =
            match parent with
            | TypeReferenceParent.TypeRef enclosing -> ValueSome(enclosing :> Type)
            | TypeReferenceParent.Assembly _ -> ValueNone
        { inherit Type(typeNamespace, parent', typeName)
          ResolutionScope = parent }

[<Sealed>]
type TypeReference<'Kind> (parent: TypeReferenceParent, typeNamespace: Identifier voption, typeName: Identifier) =
    inherit ReferencedType(parent, typeNamespace, typeName)

type ReferencedType with
    static member ConcreteClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.ConcreteClass>(resolutionScope, typeNamespace, typeName)

    static member StaticClass(resolutionScope, typeNamespace, typeName) =
        TypeReference<TypeKinds.StaticClass>(resolutionScope, typeNamespace, typeName)

[<IsReadOnly>]
type TypeVisibility = struct
    val Flag: TypeDefFlags
    val internal Parent: DefinedType

    internal new (flag, parent) =
        { Flag = flag
          Parent =
            match parent with
            | ValueSome parent' -> parent'
            | ValueNone -> Unchecked.defaultof<_> }

    member this.IsNested = this.Parent <> Unchecked.defaultof<_>
    member this.EnclosingClass = if this.Parent = Unchecked.defaultof<_> then ValueNone else ValueSome this.Parent
end

[<RequireQualifiedAccess>]
module TypeVisibility =
    let NotPublic = TypeVisibility(TypeDefFlags.NotPublic, ValueNone)
    let Public = TypeVisibility(TypeDefFlags.Public, ValueNone)
    let NestedPublic parent = TypeVisibility(TypeDefFlags.NestedPublic, ValueSome parent)
    let NestedPrivate parent = TypeVisibility(TypeDefFlags.NestedPrivate, ValueSome parent)
    let NestedFamily parent = TypeVisibility(TypeDefFlags.NestedFamily, ValueSome parent)
    let NestedAssembly parent = TypeVisibility(TypeDefFlags.NestedAssembly, ValueSome parent)
    let NestedFamilyAndAssembly parent = TypeVisibility(TypeDefFlags.NestedFamAndAssem, ValueSome parent)
    let NestedFamilyOrAssembly parent = TypeVisibility(TypeDefFlags.NestedFamOrAssem, ValueSome parent)

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    | Null

[<AbstractClass>]
type DefinedType =
    inherit Type
    val Flags: TypeDefFlags
    val Extends: ClassExtends

    new (visibility: TypeVisibility, flags, typeNamespace, parent, typeName, extends) =
        { inherit Type(typeNamespace, Convert.unsafeValueOption<DefinedType, _> parent, typeName)
          Flags = visibility.Flag ||| flags
          Extends = extends }

    member this.EnclosingClass = Convert.unsafeValueOption<_, DefinedType> this.EnclosingType
    member this.Visibility = TypeVisibility(this.Flags &&& TypeDefFlags.VisibilityMask, this.EnclosingClass)

[<Sealed>]
type TypeDefinition<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct>
    (
        visibility: TypeVisibility,
        flags: TypeAttributes<'Kind>,
        typeNamespace: Identifier voption,
        enclosingClass: DefinedType voption,
        typeName: Identifier,
        extends: ClassExtends
    )
    =
    inherit DefinedType(visibility, flags.Flags, typeNamespace, enclosingClass, typeName, extends)

type DefinedType with
    static member ConcreteClass(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.ConcreteClass>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

    static member AbstractClass(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.AbstractClass>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

    static member SealedClass(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.SealedClass>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

    static member StaticClass(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.StaticClass>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

    static member Interface(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.Interface>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

    static member ValueType(visibility, flags, typeNamespace, enclosingClass, typeName, extends) =
        TypeDefinition<TypeKinds.ValueType>(visibility, flags, typeNamespace, enclosingClass, typeName, extends)

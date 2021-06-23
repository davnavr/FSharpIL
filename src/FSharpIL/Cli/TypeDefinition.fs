namespace rec FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type ITypeDefinition = interface
    inherit IEquatable<ITypeDefinition>
    inherit IComparable<ITypeDefinition>
    inherit IComparable

    abstract TypeName: Identifier
    abstract TypeNamespace: Identifier voption
    abstract EnclosingClass: EnclosingClass voption
end

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    | Null
    | ConcreteDef of ConcreteClassDef
    | AbstractDef of AbstractClassDef
    | ConcreteRef of ConcreteClassRef
    | AbstractRef of AbstractClassRef

[<AutoOpen>]
module TypeDefinitionHelpers =
    let inline (|TypeDefinition|) (def: #ITypeDefinition) = def :> ITypeDefinition

    let inline getTypeDefinition< ^T when ^T : (member Definition : ITypeDefinition)> (case: ^T) = (^T : (member Definition : ITypeDefinition) case)

    let inline castTypeDefinition< ^T when ^T : (member Definition : ITypeDefinition)> (obj: obj) =
        match obj with
        | :? ^T as other -> getTypeDefinition<'T> other
        | _ -> obj :?> ITypeDefinition

    let inline equalsTypeDefinition (current: ^T) (obj: obj) =
        (castTypeDefinition< ^T> obj).Equals(getTypeDefinition current)

    let inline compareTypeDefinition (current: ^T) (obj: obj) =
        (castTypeDefinition< ^T> obj).CompareTo(getTypeDefinition current)

[<RequireQualifiedAccess>]
[<CustomComparison; CustomEquality>]
type EnclosingClass =
    | Concrete of ConcreteClassDef
    | Abstract of AbstractClassDef
    | Sealed of SealedClassDef
    | Static of StaticClassDef
    | Interface of InterfaceDef
    | ValueType of ValueTypeDef

    member this.Definition =
        match this with
        | Concrete(TypeDefinition def)
        | Abstract(TypeDefinition def)
        | Sealed(TypeDefinition def)
        | Static(TypeDefinition def)
        | Interface(TypeDefinition def)
        | ValueType(TypeDefinition def) -> def

    override this.Equals obj = equalsTypeDefinition this obj
    override this.GetHashCode() = this.Definition.GetHashCode()

    interface IComparable with member this.CompareTo obj = compareTypeDefinition this obj

[<CustomComparison; CustomEquality>]
type TypeDefinition<'Flags, 'Kind when 'Flags :> IAttributeTag<TypeDefFlags> and 'Flags : struct and 'Kind :> TypeKinds.Kind> =
    { Visibility: TypeVisibility
      Flags: Attributes<'Flags, TypeDefFlags, AttributeKinds.U4, uint32>
      TypeName: Identifier
      TypeNamespace: Identifier voption
      Extends: ClassExtends
      /// Gets the type that contains this nested type (II.22.32).
      EnclosingClass: EnclosingClass voption }

    member this.Equals(other: TypeDefinition<_, _>) = this.Equals(other :> ITypeDefinition)

    member this.Equals(other: ITypeDefinition) =
        this.TypeNamespace = other.TypeNamespace &&
        this.TypeName = other.TypeName &&
        this.EnclosingClass = other.EnclosingClass

    member this.CompareTo(other: #ITypeDefinition) =
        match compare this.TypeNamespace other.TypeNamespace with
        | 0 ->
            match compare this.TypeName other.TypeName with
            | 0 ->
                match this.EnclosingClass, other.EnclosingClass with
                | ValueNone, ValueNone -> 0
                | ValueSome _, ValueNone -> 1
                | ValueNone _, ValueSome _ -> -1
                | ValueSome parent1, ValueSome parent2 -> compare parent1 parent2
            | result -> result
        | result -> result

    override this.Equals obj =
        match obj with
        | :? ITypeDefinition as other -> this.Equals other
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.TypeNamespace, this.TypeName, this.EnclosingClass)

    interface ITypeDefinition with
        member this.TypeName = this.TypeName
        member this.TypeNamespace = this.TypeNamespace
        member this.EnclosingClass = this.EnclosingClass
        member this.Equals other = this.Equals other
        member this.CompareTo other = this.CompareTo other
        member this.CompareTo(obj: obj) = this.CompareTo(obj :?> ITypeDefinition)

[<RequireQualifiedAccess>]
[<CustomComparison; CustomEquality>]
type DefinedType =
    | ConcreteClass of ConcreteClassDef
    | AbstractClass of AbstractClassDef
    | SealedClass of SealedClassDef
    | StaticClass of StaticClassDef
    | Delegate of DelegateDef
    | Enum of EnumDef
    | Interface of InterfaceDef
    | ValueType of ValueTypeDef

    member this.Definition =
        match this with
        | ConcreteClass(TypeDefinition def)
        | AbstractClass(TypeDefinition def)
        | SealedClass(TypeDefinition def)
        | StaticClass(TypeDefinition def)
        | Delegate(TypeDefinition def)
        | Enum(TypeDefinition def)
        | Interface(TypeDefinition def)
        | ValueType(TypeDefinition def) -> def

    override this.Equals obj = equalsTypeDefinition this obj
    override this.GetHashCode() = this.Definition.GetHashCode()

    interface IComparable with member this.CompareTo obj = compareTypeDefinition this obj

[<RequireQualifiedAccess>]
module TypeDefinitionFlags =
    type ConcreteClass = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = Unchecked.defaultof<_>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type AbstractClass = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Abstract
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type SealedClass = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Sealed
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type StaticClass = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Abstract ||| TypeDefFlags.Sealed
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type Delegate = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Sealed
        interface TypeAttributes.ISerializableType
    end

    type Enum = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Abstract
        interface TypeAttributes.ISerializableType
    end

    type Interface = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Abstract ||| TypeDefFlags.Interface
        interface TypeAttributes.IHasStaticMethods
    end

    type ValueType = struct
        interface IAttributeTag<TypeDefFlags> with member _.RequiredFlags = TypeDefFlags.Sealed
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

type ConcreteClassDef = TypeDefinition<TypeDefinitionFlags.ConcreteClass, TypeKinds.ConcreteClass>
type AbstractClassDef = TypeDefinition<TypeDefinitionFlags.AbstractClass, TypeKinds.AbstractClass>
type SealedClassDef = TypeDefinition<TypeDefinitionFlags.SealedClass, TypeKinds.SealedClass>
type StaticClassDef = TypeDefinition<TypeDefinitionFlags.StaticClass, TypeKinds.StaticClass>
type DelegateDef = TypeDefinition<TypeDefinitionFlags.Delegate, TypeKinds.Delegate>
type EnumDef = TypeDefinition<TypeDefinitionFlags.Enum, TypeKinds.Enum>
type InterfaceDef = TypeDefinition<TypeDefinitionFlags.Interface, TypeKinds.Interface>
type ValueTypeDef = TypeDefinition<TypeDefinitionFlags.ValueType, TypeKinds.ValueType>

[<IsReadOnly>]
type TypeVisibility = struct
    val Flag: TypeDefFlags
    val internal EnclosingType: EnclosingClass

    internal new (flag, parent) = { Flag = flag; EnclosingType = parent }

    member this.IsNested = this.EnclosingType <> Unchecked.defaultof<_>
    member this.Parent = if this.IsNested then ValueSome this.EnclosingType else ValueNone
end

[<RequireQualifiedAccess>]
module TypeVisibility =
    let NotPublic = TypeVisibility(TypeDefFlags.NotPublic, Unchecked.defaultof<_>)
    let Public = TypeVisibility(TypeDefFlags.Public, Unchecked.defaultof<_>)
    let NestedPublic parent = TypeVisibility(TypeDefFlags.NestedPublic, parent)
    let NestedPrivate parent = TypeVisibility(TypeDefFlags.NestedPrivate, parent)
    let NestedFamily parent = TypeVisibility(TypeDefFlags.NestedFamily, parent)
    let NestedAssembly parent = TypeVisibility(TypeDefFlags.NestedAssembly, parent)
    let NestedFamilyAndAssembly parent = TypeVisibility(TypeDefFlags.NestedFamAndAssem, parent)
    let NestedFamilyOrAssembly parent = TypeVisibility(TypeDefFlags.NestedFamOrAssem, parent)

[<RequireQualifiedAccess>]
module TypeDefinition =
    let inline flags { Visibility = vis; Flags = flags } = vis.Flag ||| flags.Flags

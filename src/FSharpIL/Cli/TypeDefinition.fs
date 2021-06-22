namespace rec FSharpIL.Cli

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    | Null
    | ConcreteDef of ConcreteClassDef
    | AbstractDef of AbstractClassDef
    | ConcreteRef of ConcreteClassRef
    | AbstractRef of AbstractClassRef

[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type EnclosingClass =
    | Concrete of ConcreteClassDef
    | Abstract of AbstractClassDef
    | Sealed of SealedClassDef
    | Static of StaticClassDef
    | Interface of InterfaceDef

[<NoComparison; StructuralEquality>]
type TypeDefinition<'Flags, 'Kind when 'Flags :> IAttributeTag<TypeDefFlags> and 'Flags : struct and 'Kind :> TypeKinds.Kind> =
    { Visibility: TypeVisibility
      Flags: Attributes<'Flags, TypeDefFlags, AttributeKinds.U4, uint32>
      TypeName: Identifier
      TypeNamespace: Identifier voption
      Extends: ClassExtends
      /// Gets the type that contains this nested type (II.22.32).
      EnclosingClass: EnclosingClass voption }

[<RequireQualifiedAccess>]
[<CustomComparison; StructuralEquality>]
type DefinedType =
    | ConcreteClass of ConcreteClassDef
    | AbstractClass of AbstractClassDef
    | SealedClassDef of SealedClassDef
    | StaticClassDef of StaticClassDef
    | DelegateDef of DelegateDef
    | EnumDef of EnumDef
    | InterfaceDef of InterfaceDef
    | ValueTypeDef of ValueTypeDef

    member this.TypeName =
        match this with
        | ConcreteClass { TypeName = name }
        | AbstractClass { TypeName = name }
        | SealedClassDef { TypeName = name }
        | StaticClassDef { TypeName = name }
        | DelegateDef { TypeName = name }
        | EnumDef { TypeName = name }
        | InterfaceDef { TypeName = name }
        | ValueTypeDef { TypeName = name } -> name

    member this.TypeNamespace =
        match this with
        | ConcreteClass { TypeNamespace = name }
        | AbstractClass { TypeNamespace = name }
        | SealedClassDef { TypeNamespace = name }
        | StaticClassDef { TypeNamespace = name }
        | DelegateDef { TypeNamespace = name }
        | EnumDef { TypeNamespace = name }
        | InterfaceDef { TypeNamespace = name }
        | ValueTypeDef { TypeNamespace = name } -> name

    member this.EnclosingClass =
        match this with
        | ConcreteClass { EnclosingClass = parent }
        | AbstractClass { EnclosingClass = parent }
        | SealedClassDef { EnclosingClass = parent }
        | StaticClassDef { EnclosingClass = parent }
        | DelegateDef { EnclosingClass = parent }
        | EnumDef { EnclosingClass = parent }
        | InterfaceDef { EnclosingClass = parent }
        | ValueTypeDef { EnclosingClass = parent } -> parent

    interface System.IComparable<DefinedType> with
        member this.CompareTo other =
            match compare this.TypeNamespace other.TypeNamespace with
            | 0 ->
                match compare this.TypeName other.TypeName with
                | 0 -> compare this.EnclosingClass other.EnclosingClass
                | result -> result
            | result -> result

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

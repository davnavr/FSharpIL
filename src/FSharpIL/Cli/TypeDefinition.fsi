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

/// Specifies the base class that a defined class inherits from (II.22.37).
[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type ClassExtends =
    | Null
    | ConcreteDef of ConcreteClassDef
    | AbstractDef of AbstractClassDef
    | ConcreteRef of ConcreteClassRef
    | AbstractRef of AbstractClassRef
    //| TypeSpec

[<RequireQualifiedAccess>]
[<CustomComparison; CustomEquality>]
type EnclosingClass =
    | Concrete of ConcreteClassDef
    | Abstract of AbstractClassDef
    | Sealed of SealedClassDef
    | Static of StaticClassDef
    | Interface of InterfaceDef
    | ValueType of ValueTypeDef

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IComparable

[<CustomComparison; CustomEquality>]
type TypeDefinition<'Flags, 'Kind when 'Flags :> IAttributeTag<TypeDefFlags> and 'Flags : struct and 'Kind :> TypeKinds.Kind> =
    { Visibility: TypeVisibility
      Flags: Attributes<'Flags, TypeDefFlags, AttributeKinds.U4, uint32>
      TypeName: Identifier
      TypeNamespace: Identifier voption
      Extends: ClassExtends
      /// Gets the type that contains this nested type (II.22.32).
      EnclosingClass: EnclosingClass voption }

    member Equals<'OtherFlags, 'OtherKind
        when 'OtherFlags :> IAttributeTag<TypeDefFlags>
        and 'OtherFlags : struct
        and 'OtherKind :> TypeKinds.Kind> :
        other: TypeDefinition<'OtherFlags, 'OtherKind> -> bool

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface ITypeDefinition
    interface IComparable

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

    override Equals: obj -> bool
    override GetHashCode: unit -> int32

    interface IComparable

[<RequireQualifiedAccess>]
module TypeDefinitionFlags =
    type ConcreteClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type AbstractClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type SealedClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type StaticClass = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
        interface TypeAttributes.IHasLayout
        interface TypeAttributes.IHasStringFormat
        interface TypeAttributes.ISerializableType
    end

    type Delegate = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType
    end

    type Enum = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.ISerializableType
    end

    type Interface = struct
        interface IAttributeTag<TypeDefFlags>
        interface TypeAttributes.IHasStaticMethods
    end

    type ValueType = struct
        interface IAttributeTag<TypeDefFlags>
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
[<StructuralComparison; StructuralEquality>]
type TypeVisibility = struct
    val Flag: TypeDefFlags
    val internal EnclosingType: EnclosingClass

    internal new : flag: TypeDefFlags * parent: EnclosingClass -> TypeVisibility

    member IsNested: bool
    member Parent: EnclosingClass voption
end

[<RequireQualifiedAccess>]
module TypeVisibility =
    val NotPublic: TypeVisibility
    val Public: TypeVisibility
    val NestedPublic: parent: EnclosingClass -> TypeVisibility
    val NestedPrivate: parent: EnclosingClass -> TypeVisibility
    val NestedFamily: parent: EnclosingClass -> TypeVisibility
    val NestedAssembly: parent: EnclosingClass -> TypeVisibility
    val NestedFamilyAndAssembly: parent: EnclosingClass -> TypeVisibility
    val NestedFamilyOrAssembly: parent: EnclosingClass -> TypeVisibility

[<RequireQualifiedAccess>]
module TypeDefinition =
    val inline flags: TypeDefinition<'Flags, 'Tag> -> TypeDefFlags

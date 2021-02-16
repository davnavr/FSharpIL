﻿namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

type IFlags<'Flags when 'Flags :> System.Enum> = abstract Value: 'Flags

[<IsReadOnly>]
type ValidFlags<'Tag, 'Flags when 'Flags :> System.Enum> =
    struct
        val Value: 'Flags
        internal new(flags: 'Flags) = { Value = flags }
        override this.ToString() = this.Value.ToString()
        interface IFlags<'Flags> with member this.Value = this.Value
    end

// TODO: Determine if flag types should be reference or value types.
// TODO: Instead of marker types, use the actual types themselves such as using ConcreteClassDef instead of ConcreteClassFlags.

type LayoutFlag =
    | AutoLayout
    /// Used as the default layout for structs by the C# and F# compilers.
    | SequentialLayout
    | ExplicitLayout

    member this.Flags =
        match this with
        | AutoLayout -> TypeAttributes.AutoLayout
        | SequentialLayout -> TypeAttributes.SequentialLayout
        | ExplicitLayout -> TypeAttributes.ExplicitLayout

type StringFormattingFlag =
    | AnsiClass
    | UnicodeClass
    | AutoClass
    // | CustomFormatClass

    member this.Flags =
        match this with
        | AnsiClass -> TypeAttributes.AnsiClass
        | UnicodeClass -> TypeAttributes.UnicodeClass
        | AutoClass -> TypeAttributes.AutoClass

[<IsReadOnly; Struct>]
type ClassFlags =
    { Layout: LayoutFlag
      SpecialName: bool
      Import: bool
      Serializable: bool
      StringFormat: StringFormattingFlag
      BeforeFieldInit: bool
      RTSpecialName: bool }

    member this.Value =
        let mutable flags =
            let layout =
                match this.Layout with
                | AutoLayout -> TypeAttributes.AutoLayout
                | SequentialLayout -> TypeAttributes.SequentialLayout
                | ExplicitLayout -> TypeAttributes.ExplicitLayout
            let stringf =
                match this.StringFormat with
                | AnsiClass -> TypeAttributes.AnsiClass
                | UnicodeClass -> TypeAttributes.UnicodeClass
                | AutoClass -> TypeAttributes.AutoClass
            layout ||| stringf
        if this.SpecialName then flags <- flags ||| TypeAttributes.SpecialName
        if this.Import then flags <- flags ||| TypeAttributes.Import
        if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
        if this.BeforeFieldInit then flags <- flags ||| TypeAttributes.BeforeFieldInit
        if this.RTSpecialName then flags <- flags ||| TypeAttributes.RTSpecialName
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

    static member None =
        { Layout = AutoLayout
          SpecialName = false
          Import = false
          Serializable = false
          StringFormat = AnsiClass
          BeforeFieldInit = false
          RTSpecialName = false }

[<AbstractClass; Sealed>] type ConcreteClassFlags = class end
[<AbstractClass; Sealed>] type AbstractClassFlags = class end
[<AbstractClass; Sealed>] type SealedClassFlags = class end
[<AbstractClass; Sealed>] type StaticClassFlags = class end

[<Struct; IsReadOnly>]
type DelegateFlags =
    { Serializable: bool }

    member this.Value =
        let mutable flags = TypeAttributes.Sealed
        if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

[<Struct; IsReadOnly>]
[<RequireQualifiedAccess>]
type InterfaceFlags =
    { Import: bool }

    member this.Value =
        let mutable flags = TypeAttributes.Abstract ||| TypeAttributes.Interface
        if this.Import then flags <- flags ||| TypeAttributes.Import
        flags

    interface IFlags<TypeAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type StructFlags = class end

type Visibility =
    | CompilerControlled
    | Private
    | FamilyAndAssembly
    | Assembly
    | Family
    | FamilyOrAssembly
    | Public

    interface IFlags<FieldAttributes> with
        member this.Value =
            match this with
            | CompilerControlled -> FieldAttributes.PrivateScope
            | Private -> FieldAttributes.Private
            | FamilyAndAssembly -> FieldAttributes.FamANDAssem
            | Assembly -> FieldAttributes.Assembly
            | Family -> FieldAttributes.Family
            | FamilyOrAssembly -> FieldAttributes.FamORAssem
            | Public -> FieldAttributes.Public

    interface IFlags<MethodAttributes> with
        member this.Value =
            match this with
            | CompilerControlled -> MethodAttributes.PrivateScope
            | Private -> MethodAttributes.Private
            | FamilyAndAssembly -> MethodAttributes.FamANDAssem
            | Assembly -> MethodAttributes.Assembly
            | Family -> MethodAttributes.Family
            | FamilyOrAssembly -> MethodAttributes.FamORAssem
            | Public -> MethodAttributes.Public

/// <summary>
/// Visibility for fields and methods defined in the <c>&lt;Module&gt;</c> pseudo-class.
/// </summary>
[<RequireQualifiedAccess>]
type GlobalVisibility =
    | Public
    | CompilerControlled
    | Private

    interface IFlags<FieldAttributes> with
        member this.Value =
            match this with
            | Public -> FieldAttributes.Public
            | CompilerControlled -> FieldAttributes.PrivateScope
            | Private -> FieldAttributes.Private

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type FieldFlags<'Visibility when 'Visibility :> IFlags<FieldAttributes>> =
    { Visibility: 'Visibility
      NotSerialized: bool
      /// Sets the `SpecialName` and `RTSpecialName` flags. // TODO: SpecialName is required to be set if RTSpecialName is set, so allow fields that set special name but don't set RTSpecialName.
      SpecialName: bool }

    member this.Value =
        let mutable flags = this.Visibility.Value
        if this.NotSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        if this.SpecialName then flags <- flags ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        flags

    interface IFlags<FieldAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type InstanceFieldFlags = class end
[<AbstractClass; Sealed>] type StaticFieldFlags = class end
[<AbstractClass; Sealed>] type GlobalFieldFlags = class end

// NOTE: For methods, SpecialName has to be set if RTSpecialName is set.
// NOTE: For methods, RTSpecialName and SpecialName is set when it is a ctor or cctor

type VTableLayout =
    | ReuseSlot
    | NewSlot

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type StaticMethodDefFlags<'Visibility when 'Visibility :> IFlags<MethodAttributes>> =
    { Visibility: 'Visibility
      HideBySig: bool }

    member this.Value =
        let flags = this.Visibility.Value
        if this.HideBySig
        then flags ||| MethodAttributes.HideBySig
        else flags

    interface IFlags<MethodAttributes> with member this.Value = this.Value

[<IsReadOnly; Struct>]
type InstanceMethodDefFlags =
    { Visibility: Visibility
      HideBySig: bool
      VTableLayout: VTableLayout }

    member this.Value =
        let vtable =
            match this.VTableLayout with
            | ReuseSlot -> MethodAttributes.ReuseSlot
            | NewSlot -> MethodAttributes.NewSlot
        let mutable flags = (this.Visibility :> IFlags<MethodAttributes>).Value
        if this.HideBySig then flags <- flags ||| MethodAttributes.HideBySig
        flags ||| vtable

    interface IFlags<MethodAttributes> with member this.Value = this.Value

[<IsReadOnly; Struct>]
type MethodImplFlags =
    { ForwardRef: bool
      PreserveSig: bool
      NoInlining: bool
      NoOptimization: bool }

    member this.Value =
        let mutable flags = enum<MethodImplAttributes> 0
        if this.ForwardRef then flags <- flags ||| MethodImplAttributes.ForwardRef
        if this.PreserveSig then flags <- flags ||| MethodImplAttributes.PreserveSig
        if this.NoInlining then flags <- flags ||| MethodImplAttributes.NoInlining
        if this.NoOptimization then flags <- flags ||| MethodImplAttributes.NoOptimization
        flags

    interface IFlags<MethodImplAttributes> with member this.Value = this.Value

    static member None =
        { ForwardRef = false
          PreserveSig = false
          NoInlining = false
          NoOptimization = false }

[<AbstractClass; Sealed>] type InstanceMethodFlags = class end
[<AbstractClass; Sealed>] type AbstractMethodFlags = class end
[<AbstractClass; Sealed>] type FinalMethodFlags = class end
[<AbstractClass; Sealed>] type StaticMethodFlags = class end
[<AbstractClass; Sealed>] type GlobalMethodFlags = class end

// NOTE: Constructors and Class Constructors cannot be marked CompilerControlled.
[<AbstractClass; Sealed>] type ConstructorFlags = class end
[<AbstractClass; Sealed>] type ClassConstructorFlags = class end

[<IsReadOnly; Struct>]
type ParamFlags =
    { In: bool
      Out: bool
      Optional: bool }

    member this.Value =
        let mutable flags = ParameterAttributes.None
        if this.In then flags <- flags ||| ParameterAttributes.In
        if this.Out then flags <- flags ||| ParameterAttributes.Out
        if this.Optional then flags <- flags ||| ParameterAttributes.Optional
        flags

    interface IFlags<ParameterAttributes> with member this.Value = this.Value

    static member None = { In = false; Out = false; Optional = false }

[<IsReadOnly; Struct>]
type GenericParamFlags =
    { ReferenceType: bool
      /// <summary><c>NotNullableValueType</c></summary>
      ValueType: bool
      DefaultConstructor: bool }

    member this.Value =
        let mutable flags = GenericParameterAttributes.None
        if this.ReferenceType then flags <- GenericParameterAttributes.ReferenceTypeConstraint
        if this.ValueType then flags <- GenericParameterAttributes.NotNullableValueTypeConstraint
        if this.DefaultConstructor then flags <- GenericParameterAttributes.DefaultConstructorConstraint
        flags

    static member None = { ReferenceType = false; ValueType = false; DefaultConstructor = false }

    interface IFlags<GenericParameterAttributes> with member this.Value = this.Value

[<AbstractClass; Sealed>] type NonVariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type CovariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type ContravariantGenericParamFlags = class end

type TypeFlags<'Tag> = ValidFlags<'Tag, TypeAttributes>

[<AutoOpen>]
module Flags =
    let (|Flags|) (flags: #IFlags<_>) = flags.Value

    // TODO: Move Flags functions to separate module marked RequireQualifiedAccess

    let concreteClassFlags (Flags flags: ClassFlags) = TypeFlags<ConcreteClassFlags> flags
    let abstractClassFlags (Flags flags: ClassFlags) = TypeFlags<AbstractClassFlags>(TypeAttributes.Abstract ||| flags)
    let sealedClassFlags (Flags flags: ClassFlags) = TypeFlags<SealedClassFlags>(TypeAttributes.Sealed ||| flags)
    let staticClassFlags (Flags flags: ClassFlags) = TypeFlags<StaticClassFlags>(TypeAttributes.Abstract ||| TypeAttributes.Sealed ||| flags)
    let structFlags (Flags flags: ClassFlags) = TypeFlags<StructFlags>(TypeAttributes.Sealed ||| flags)

    let instanceFieldFlags (Flags flags: FieldFlags<Visibility>) = ValidFlags<InstanceFieldFlags, _> flags
    let staticFieldFlags (Flags flags: FieldFlags<Visibility>) = ValidFlags<StaticFieldFlags, _>(flags ||| FieldAttributes.Static)
    let globalFieldFlags (Flags flags: FieldFlags<GlobalVisibility>) = ValidFlags<GlobalFieldFlags, _>(flags ||| FieldAttributes.Static) // TODO: Set other special flags

    let instanceMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<InstanceMethodFlags, _> flags
    let abstractMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<AbstractMethodFlags, _>(MethodAttributes.Abstract ||| MethodAttributes.Virtual ||| flags)
    let finalMethodFlags (Flags flags: InstanceMethodDefFlags) = ValidFlags<FinalMethodFlags, _>(MethodAttributes.Final ||| MethodAttributes.Virtual ||| flags)
    let staticMethodFlags (Flags flags: StaticMethodDefFlags<Visibility>) = ValidFlags<StaticMethodFlags, _>(MethodAttributes.Static ||| flags)
    // let globalMethodFlags (Flags flags: StaticMethodDefFlags<GlobalVisibility>) = invalidOp ""

    // let constructorFlags (Flags flags: _) = ValidFlags<ConstructorFlags, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| flags)
    // let classConstructorFlags (Flags flags: _) = ValidFlags<ClassConstructorFlags, _>(MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.Static ||| flags)

    /// Flags for non-variant generic parameters.
    let invariantFlags (Flags flags: GenericParamFlags) = ValidFlags<NonVariantGenericParamFlags, _> flags
    /// Flags for generic parameters that can be covariant.
    let covariantFlags (covariant: bool) (Flags flags: GenericParamFlags) =
        if covariant
        then GenericParameterAttributes.Covariant ||| flags
        else flags
        |> ValidFlags<CovariantGenericParamFlags, _>
    /// Flags fo generic parameters that can be contravariant.
    let contravariantFlags (contravariant: bool) (Flags flags: GenericParamFlags) =
        if contravariant
        then GenericParameterAttributes.Contravariant ||| flags
        else flags
        |> ValidFlags<ContravariantGenericParamFlags, _>

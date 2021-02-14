namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

type IFlags<'Flags when 'Flags :> System.Enum> = abstract Flags: 'Flags // TODO: Rename to Value.

[<IsReadOnly>]
type ValidFlags<'Tag, 'Flags when 'Flags :> System.Enum> =
    struct
        val Value: 'Flags
        internal new(flags: 'Flags) = { Value = flags }
        override this.ToString() = this.Value.ToString()
        interface IFlags<'Flags> with member this.Flags = this.Value
    end

// TODO: Determine if flag types should be reference or value types.
// TODO: Replace most flag types with ValidFlags<_, _>.

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

    static member Zero = AutoLayout

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

    static member Zero = AnsiClass

[<IsReadOnly; Struct>]
type ClassFlags =
    { Layout: LayoutFlag
      SpecialName: bool
      Import: bool
      Serializable: bool
      StringFormat: StringFormattingFlag
      BeforeFieldInit: bool
      RTSpecialName: bool }

    member this.Flags =
        let mutable flags = this.Layout.Flags ||| this.StringFormat.Flags
        if this.SpecialName then flags <- flags ||| TypeAttributes.SpecialName
        if this.Import then flags <- flags ||| TypeAttributes.Import
        if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
        if this.BeforeFieldInit then flags <- flags ||| TypeAttributes.BeforeFieldInit
        if this.RTSpecialName then flags <- flags ||| TypeAttributes.RTSpecialName
        flags

    static member Zero =
        { Layout = LayoutFlag.Zero
          SpecialName = false
          Import = false
          Serializable = false
          StringFormat = StringFormattingFlag.Zero
          BeforeFieldInit = false
          RTSpecialName = false }

[<IsReadOnly; Struct>]
type ConcreteClassFlags private (flags: TypeAttributes) =
    new (flags: ClassFlags) = ConcreteClassFlags(flags.Flags)
    interface IFlags<TypeAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type AbstractClassFlags private (flags: TypeAttributes) =
    new (flags: ClassFlags) = AbstractClassFlags(flags.Flags ||| TypeAttributes.Abstract)
    interface IFlags<TypeAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type SealedClassFlags private (flags: TypeAttributes) =
   new (flags: ClassFlags) = SealedClassFlags(flags.Flags ||| TypeAttributes.Sealed)
   interface IFlags<TypeAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type StaticClassFlags private (flags: TypeAttributes) =
   new (flags: ClassFlags) = StaticClassFlags(flags.Flags ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
   interface IFlags<TypeAttributes> with member _.Flags = flags

[<Struct; IsReadOnly>]
type DelegateFlags =
    { Serializable: bool }

    interface IFlags<TypeAttributes> with
        member this.Flags =
            let mutable flags = TypeAttributes.Sealed
            if this.Serializable then flags <- flags ||| TypeAttributes.Serializable
            flags

[<Struct; IsReadOnly>]
[<RequireQualifiedAccess>]
type InterfaceFlags =
    { Import: bool }

    interface IFlags<TypeAttributes> with
        member this.Flags =
            let mutable flags = TypeAttributes.Abstract ||| TypeAttributes.Interface
            if this.Import then flags <- flags ||| TypeAttributes.Import
            flags

[<IsReadOnly; Struct>]
type StructFlags private (flags: TypeAttributes) =
    new (flags: ClassFlags) = StructFlags(flags.Flags ||| TypeAttributes.Sealed)
    interface IFlags<TypeAttributes> with member _.Flags = flags

    static member Zero = { ClassFlags.Zero with Layout = LayoutFlag.SequentialLayout } |> StructFlags

type Visibility =
    | CompilerControlled
    | Private
    | FamilyAndAssembly
    | Assembly
    | Family
    | FamilyOrAssembly
    | Public

    interface IFlags<FieldAttributes> with
        member this.Flags =
            match this with
            | CompilerControlled -> FieldAttributes.PrivateScope
            | Private -> FieldAttributes.Private
            | FamilyAndAssembly -> FieldAttributes.FamANDAssem
            | Assembly -> FieldAttributes.Assembly
            | Family -> FieldAttributes.Family
            | FamilyOrAssembly -> FieldAttributes.FamORAssem
            | Public -> FieldAttributes.Public

    interface IFlags<MethodAttributes> with
        member this.Flags =
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
        member this.Flags =
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

    member this.Flags =
        let mutable flags = this.Visibility.Flags
        if this.NotSerialized then flags <- flags ||| FieldAttributes.NotSerialized
        if this.SpecialName then flags <- flags ||| FieldAttributes.SpecialName ||| FieldAttributes.RTSpecialName
        flags

[<IsReadOnly; Struct>]
type InstanceFieldFlags private (flags: FieldAttributes) =
    new (flags: FieldFlags<Visibility>) = InstanceFieldFlags(flags.Flags)
    interface IFlags<FieldAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type StaticFieldFlags private (flags: FieldAttributes) =
    new (flags: FieldFlags<Visibility>) = StaticFieldFlags(flags.Flags)
    interface IFlags<FieldAttributes> with member _.Flags = flags // TODO: Set special flags.

[<IsReadOnly; Struct>]
type GlobalFieldFlags private (flags: FieldAttributes) =
    new (flags: FieldFlags<GlobalVisibility>) = GlobalFieldFlags(flags.Flags)
    interface IFlags<FieldAttributes> with member _.Flags = flags // TODO: Set special flags.

// NOTE: For methods, SpecialName has to be set if RTSpecialName is set.
// NOTE: For methods, RTSpecialName and SpecialName is set when it is a ctor or cctor

type VTableLayout =
    | ReuseSlot
    | NewSlot

    interface IFlags<MethodAttributes> with
        member this.Flags =
            match this with
            | ReuseSlot -> MethodAttributes.ReuseSlot
            | NewSlot -> MethodAttributes.NewSlot

    static member Zero = ReuseSlot

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type StaticMethodDefFlags<'Visibility when 'Visibility :> IFlags<MethodAttributes>> =
    { Visibility: 'Visibility
      HideBySig: bool }

    member this.Flags =
        let flags = this.Visibility.Flags
        if this.HideBySig
        then flags ||| MethodAttributes.HideBySig
        else flags

[<IsReadOnly; Struct>]
type InstanceMethodDefFlags =
    { Visibility: Visibility
      HideBySig: bool
      VTableLayout: VTableLayout }

    member this.Flags =
        let mutable flags = (this.Visibility :> IFlags<MethodAttributes>).Flags
        if this.HideBySig then flags <- flags ||| MethodAttributes.HideBySig
        flags ||| (this.VTableLayout :> IFlags<_>).Flags

[<IsReadOnly; Struct>]
type MethodImplFlags =
    { ForwardRef: bool
      PreserveSig: bool
      NoInlining: bool
      NoOptimization: bool }

    member this.Flags =
        let mutable flags = enum<MethodImplAttributes > 0
        if this.ForwardRef then flags <- flags ||| MethodImplAttributes.ForwardRef
        if this.PreserveSig then flags <- flags ||| MethodImplAttributes.PreserveSig
        if this.NoInlining then flags <- flags ||| MethodImplAttributes.NoInlining
        if this.NoOptimization then flags <- flags ||| MethodImplAttributes.NoOptimization
        flags

    static member Zero =
        { ForwardRef = false
          PreserveSig = false
          NoInlining = false
          NoOptimization = false }

[<IsReadOnly; Struct>]
type InstanceMethodFlags private (flags: MethodAttributes) =
    new (flags: InstanceMethodDefFlags) = InstanceMethodFlags(flags.Flags)
    interface IFlags<MethodAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type AbstractMethodFlags private (flags: MethodAttributes) =
    new (flags: InstanceMethodDefFlags) = AbstractMethodFlags(flags.Flags ||| MethodAttributes.Abstract ||| MethodAttributes.Virtual)
    interface IFlags<MethodAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
type FinalMethodFlags private (flags: MethodAttributes) =
    interface IFlags<MethodAttributes> with member _.Flags = flags ||| MethodAttributes.Final ||| MethodAttributes.Virtual

[<IsReadOnly; Struct>]
type StaticMethodFlags private (flags: MethodAttributes) =
    new (flags: StaticMethodDefFlags<Visibility>) = StaticMethodFlags(flags.Flags ||| MethodAttributes.Static)
    interface IFlags<MethodAttributes> with member _.Flags = flags

// type GlobalMethodFlags

// NOTE: Constructors and Class Constructors cannot be marked CompilerControlled.
[<IsReadOnly; Struct>]
type ConstructorFlags private (flags: MethodAttributes) =
    interface IFlags<MethodAttributes> with member _.Flags = flags ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName // TODO: Move setting of flags into a constructor.

[<IsReadOnly; Struct>]
type ClassConstructorFlags private (flags: MethodAttributes) =
    interface IFlags<MethodAttributes> with member _.Flags = flags ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.Static

// TODO: Reduce number of struct types by using the records directly or by using functions instead.

[<IsReadOnly; Struct>]
type ParamFlags =
    { In: bool
      Out: bool
      Optional: bool }

    member this.Flags =
        let mutable flags = ParameterAttributes.None
        if this.In then flags <- flags ||| ParameterAttributes.In
        if this.Out then flags <- flags ||| ParameterAttributes.Out
        if this.Optional then flags <- flags ||| ParameterAttributes.Optional
        flags

    // TODO: Rename default flags from Zero to None
    static member Zero = { In = false; Out = false; Optional = false }

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

    interface IFlags<GenericParameterAttributes> with member this.Flags = this.Value

[<AbstractClass; Sealed>] type NonVariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type CovariantGenericParamFlags = class end
[<AbstractClass; Sealed>] type ContravariantGenericParamFlags = class end

[<AutoOpen>]
module Flags =
    let (|Flags|) (flags: #IFlags<_>) = flags.Flags

    /// Flags for non-variant generic parameters.
    let invariantFlags (Flags flags: GenericParamFlags) = ValidFlags<NonVariantGenericParamFlags, _> flags
    /// Flags for covariant generic parameters.
    let covariantFlags (Flags flags: GenericParamFlags) = ValidFlags<CovariantGenericParamFlags, _>(GenericParameterAttributes.Covariant ||| flags)
    /// Flags for contravariant generic parameters.
    let contravariantFlags (Flags flags: GenericParamFlags) = ValidFlags<ContravariantGenericParamFlags, _>(GenericParameterAttributes.Contravariant ||| flags)

namespace FSharpIL.Metadata

open System.Reflection

type private IsReadOnly = System.Runtime.CompilerServices.IsReadOnlyAttribute

type IFlags<'Flags when 'Flags :> System.Enum> =
    abstract Flags: 'Flags

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
[<StructuralComparison; StructuralEquality>]
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
[<StructuralComparison; StructuralEquality>]
type ConcreteClassFlags private (flags: TypeAttributes) =
    new (flags: ClassFlags) = ConcreteClassFlags(flags.Flags)
    interface IFlags<TypeAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type AbstractClassFlags private (flags: TypeAttributes) =
    new (flags: ClassFlags) = AbstractClassFlags(flags.Flags ||| TypeAttributes.Abstract)
    interface IFlags<TypeAttributes> with member _.Flags = flags

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type SealedClassFlags private (flags: TypeAttributes) =
   new (flags: ClassFlags) = SealedClassFlags(flags.Flags ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
   interface IFlags<TypeAttributes> with member _.Flags = flags

[<Struct; IsReadOnly>]
[<RequireQualifiedAccess>]
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
[<StructuralComparison; StructuralEquality>]
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

type InitOnly = InitOnly

[<IsReadOnly; Struct>]
type InstanceFieldFlags =
    interface IFlags<FieldAttributes> with member this.Flags = invalidOp "bad"

[<IsReadOnly; Struct>]
type StaticFieldFlags =
    interface IFlags<FieldAttributes> with member this.Flags = invalidOp "bad"

// NOTE: For both methods and fields, RTSpecialName is set if SpecialName is set

[<AutoOpen>]
module Flags =
    let inline (|Flags|) (flags: IFlags<_>) = flags.Flags

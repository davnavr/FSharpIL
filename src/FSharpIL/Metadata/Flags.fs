﻿namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

type IFlags<'T when 'T :> System.Enum> =
    abstract Flags : 'T

type SpecialName = SpecialName
type RTSpecialName = RTSpecialName

[<AbstractClass>]
type FlagsBuilder<'Flags, 'T> internal (case: 'Flags -> 'T) =
    member inline _.Combine(one: unit -> _, two: unit -> _) = fun() -> one() ||| two()
    member inline _.Delay(expr: unit -> unit -> 'Flags) = fun() -> expr () ()
    abstract member Run: (unit -> 'Flags) -> 'T
    default _.Run(expr: unit -> 'Flags) = expr() |> case
    member inline _.Zero() = fun() -> Unchecked.defaultof<'Flags>

type LayoutFlag = // TODO: For these two flags, have equality and comparison functions always return the same value.
    /// Used as the default layout for structs by the C# compiler.
    | SequentialLayout
    | ExplicitLayout

type Sealed = Sealed
type Import = Import
type Serializable = Serializable

[<Struct; IsReadOnly>]
type StringFormattingFlag =
    | UnicodeClass
    | AutoClass
    // | CustomFormatClass

type BeforeFieldInit = BeforeFieldInit

[<AbstractClass>]
type BaseTypeFlagsBuilder<'T> internal (case) =
    inherit FlagsBuilder<TypeAttributes, 'T>(case)

    member inline _.Yield(_: SpecialName) = fun() -> TypeAttributes.SpecialName
    member inline _.Yield(_: Import) = fun() -> TypeAttributes.Import
    member inline _.Yield(_: RTSpecialName) = fun() -> TypeAttributes.RTSpecialName

type TypeFlagsBuilder<'T> internal (case) =
    inherit FlagsBuilder<TypeAttributes, 'T>(case)

    member inline _.Yield(layout: LayoutFlag) =
        fun() ->
            match layout with
            | SequentialLayout -> TypeAttributes.SequentialLayout
            | ExplicitLayout -> TypeAttributes.ExplicitLayout
    member inline _.Yield(_: Serializable) = fun() -> TypeAttributes.Serializable
    member inline _.Yield(charSet: StringFormattingFlag) =
        fun() ->
            match charSet with
            | UnicodeClass -> TypeAttributes.UnicodeClass
            | AutoClass -> TypeAttributes.AutoClass
    member inline _.Yield(_: BeforeFieldInit) = fun() -> TypeAttributes.BeforeFieldInit

type SealedTypeFlagsBuilder<'T> internal (case) =
    inherit TypeFlagsBuilder<'T>(case)

    override _.Run expr = base.Run(fun() -> expr() ||| TypeAttributes.Sealed)

[<Struct; IsReadOnly>]
type ClassFlags =
    internal
    | ClassFlags of TypeAttributes

    static member Default = ClassFlags TypeAttributes.BeforeFieldInit

    interface IFlags<TypeAttributes> with
        override this.Flags = let (ClassFlags flags) = this in flags

type ClassFlagsBuilder<'T> internal (case) =
    inherit TypeFlagsBuilder<'T>(case)

    member inline _.Yield(_: Sealed) = fun() -> TypeAttributes.Sealed

[<Struct; IsReadOnly>]
type AbstractClassFlags =
    internal
    | AbstractClassFlags of TypeAttributes

    static member Default = TypeAttributes.Abstract ||| TypeAttributes.BeforeFieldInit |> AbstractClassFlags

    interface IFlags<TypeAttributes> with
        override this.Flags = let (AbstractClassFlags flags) = this in flags

[<Sealed>]
type AbstractClassFlagsBuilder internal () =
    inherit ClassFlagsBuilder<AbstractClassFlags>(AbstractClassFlags)

    override _.Run expr = base.Run(fun() -> expr() ||| TypeAttributes.Abstract)

[<Struct; IsReadOnly>]
type DelegateFlags =
    internal
    | DelegateFlags of TypeAttributes

    static member Default = DelegateFlags TypeAttributes.Sealed

[<Struct; IsReadOnly>]
type InterfaceFlags =
    internal
    | InterfaceFlags of TypeAttributes

    static member Default = TypeAttributes.Abstract ||| TypeAttributes.Interface |> InterfaceFlags

[<Sealed>]
type InterfaceFlagsBuilder internal() =
    inherit BaseTypeFlagsBuilder<InterfaceFlags>(InterfaceFlags)

    override _.Run expr = base.Run(fun() -> expr() ||| TypeAttributes.Abstract ||| TypeAttributes.Interface)

[<Struct; IsReadOnly>]
type StructFlags =
    internal
    | StructFlags of TypeAttributes

    static member Default = TypeAttributes.BeforeFieldInit ||| TypeAttributes.SequentialLayout ||| TypeAttributes.Sealed |> StructFlags

type Visibility =
    | CompilerControlled
    | Private
    | FamilyAndAssembly
    | Assembly
    | Family
    | FamilyOrAssembly
    | Public

type InitOnly = InitOnly

[<Struct; IsReadOnly>]
type InstanceFieldFlags =
    internal
    | InstanceFieldFlags of FieldAttributes

    static member Default = invalidOp "bad"

    interface IFlags<FieldAttributes> with
        override this.Flags = let (InstanceFieldFlags flags) = this in flags

[<AbstractClass>]
type FieldFlagsBuilder<'T> internal(case) =
    inherit FlagsBuilder<FieldAttributes, 'T>(case)

    member inline _.Yield(_: RTSpecialName, _: SpecialName) = fun() -> FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName
    member inline this.Yield(x: SpecialName, y: RTSpecialName) = this.Yield(y, x)

type InitOnlyFieldFlagsBuilder<'T> internal(case) =
    inherit FieldFlagsBuilder<'T>(case)

    member inline _.Yield(_: InitOnly) = fun() -> FieldAttributes.InitOnly

[<Struct; IsReadOnly>]
type StaticFieldFlags =
    internal
    | StaticFieldFlags of FieldAttributes

    static member Default = invalidOp "bad"

    interface IFlags<FieldAttributes> with
        override this.Flags = let (StaticFieldFlags flags) = this in flags

[<Sealed>]
type StaticFieldFlagsBuilder internal() =
    inherit InitOnlyFieldFlagsBuilder<StaticFieldFlags>(StaticFieldFlags)

    override _.Run expr = base.Run(fun() -> expr() ||| FieldAttributes.Static)

[<AutoOpen>]
module FlagBuilders =
    let classFlags = ClassFlagsBuilder<ClassFlags> ClassFlags
    let abstractClassFlags = AbstractClassFlagsBuilder()
    let delegateFlags = SealedTypeFlagsBuilder<DelegateFlags> DelegateFlags
    let interfaceFlags = InterfaceFlagsBuilder()
    let structFlags = SealedTypeFlagsBuilder<StructFlags> StructFlags

    let instanceFieldFlags = InitOnlyFieldFlagsBuilder<InstanceFieldFlags>(InstanceFieldFlags)
    let staticFieldFlags = StaticFieldFlagsBuilder()

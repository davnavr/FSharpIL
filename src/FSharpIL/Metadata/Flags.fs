namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

/// Marks a class as both abstract and sealed, or specifies that a member is static.
type Static = Static
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

[<Sealed>]
type ClassFlagsBuilder internal () =
    inherit TypeFlagsBuilder<ClassFlags>(ClassFlags)

    member inline _.Yield(_: Static) = fun() -> TypeAttributes.Sealed ||| TypeAttributes.Abstract
    member inline _.Yield(_: Sealed) = fun() -> TypeAttributes.Sealed

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

[<AutoOpen>]
module FlagBuilders =
    let classFlags = ClassFlagsBuilder()
    let delegateFlags = SealedTypeFlagsBuilder<DelegateFlags> DelegateFlags
    let interfaceFlags = InterfaceFlagsBuilder()
    let structFlags = SealedTypeFlagsBuilder<StructFlags> StructFlags

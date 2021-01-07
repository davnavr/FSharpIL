namespace FSharpIL.Metadata

open System.Reflection
open System.Runtime.CompilerServices

[<Struct; IsReadOnly>] type SpecialName = SpecialName
[<Struct; IsReadOnly>] type RTSpecialName = RTSpecialName

[<AbstractClass>]
type FlagsBuilder<'Flags, 'T> internal (case: 'Flags -> 'T) =
    member inline _.Combine(one: unit -> _, two: unit -> _) = fun() -> one() ||| two()
    member inline _.Delay(expr: unit -> unit -> 'Flags) = fun() -> expr () ()
    member _.Run(expr: unit -> 'Flags) = expr() |> case
    member inline _.Zero() = fun() -> Unchecked.defaultof<'Flags>

[<Struct; IsReadOnly>]
type LayoutFlag = // TODO: For these two flags, have equality and comparison functions always return the same value.
    | SequentialLayout
    | ExplicitLayout

[<Struct; IsReadOnly>] type Sealed = Sealed
[<Struct; IsReadOnly>] type Import = Import
[<Struct; IsReadOnly>] type Serializable = Serializable

[<Struct; IsReadOnly>]
type StringFormattingFlag =
    | UnicodeClass
    | AutoClass
    // | CustomFormatClass

[<Struct; IsReadOnly>] type BeforeFieldInit = BeforeFieldInit

type TypeFlagsBuilder<'T> internal (case) =
    inherit FlagsBuilder<TypeAttributes, 'T>(case)

    member inline _.Yield(layout: LayoutFlag) =
        fun() ->
            match layout with
            | SequentialLayout -> TypeAttributes.SequentialLayout
            | ExplicitLayout -> TypeAttributes.ExplicitLayout
    member inline _.Yield(_: SpecialName) = fun() -> TypeAttributes.SpecialName
    member inline _.Yield(_: Import) = fun() -> TypeAttributes.Import
    member inline _.Yield(_: Serializable) = fun() -> TypeAttributes.Serializable
    member inline _.Yield(charSet: StringFormattingFlag) =
        fun() ->
            match charSet with
            | UnicodeClass -> TypeAttributes.UnicodeClass
            | AutoClass -> TypeAttributes.AutoClass
    member inline _.Yield(_: BeforeFieldInit) = fun() -> TypeAttributes.BeforeFieldInit
    member inline _.Yield(_: RTSpecialName) = fun() -> TypeAttributes.RTSpecialName

[<Struct; IsReadOnly>]
type ClassFlags =
    internal
    | ClassFlags of TypeAttributes

type ClassFlagsBuilder internal () =
    inherit TypeFlagsBuilder<ClassFlags>(ClassFlags)

    member inline _.Yield(_: Sealed) = fun() -> TypeAttributes.Sealed

[<AutoOpen>]
module FlagBuilders =
    let classFlags =ClassFlagsBuilder()

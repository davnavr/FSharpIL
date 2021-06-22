[<RequireQualifiedAccess>]
module FSharpIL.Writing.Abstractions.TypeKinds

type [<AbstractClass>] Kind internal () = class end
type ConcreteClass internal () = inherit Kind()
type AbstractClass internal () = inherit Kind()
type SealedClass internal () = inherit Kind()
type StaticClass internal () = inherit Kind()
type Delegate internal () = inherit Kind()
type Enum internal () = inherit Kind()
type Interface internal () = inherit Kind()
type ValueType internal () = inherit Kind()

namespace FSharpIL.Cli

open System

open FSharpIL.Metadata

open FSharpIL.Utilities
open FSharpIL.Utilities.Compare

[<Sealed>]
type Property (name, getter, setter, other) =
    member _.Flags = FSharpIL.Metadata.Tables.PropertyFlags.None
    member _.Name: Identifier = name
    member _.Getter: DefinedMethod voption = getter
    member _.Setter: DefinedMethod voption = setter
    member _.Other: DefinedMethod list = other

    override _.GetHashCode() = HashCode.Combine(name, getter, setter)

    interface IEquatable<Property> with
        member _.Equals other =
            let inline propMethodsEqual a b =
                match a, b with
                | ValueSome a', ValueSome b' -> Method.signatureComparer.Equals(a', b')
                | ValueNone, ValueNone -> true
                | _ -> false

            name === other.Name &&
            propMethodsEqual getter other.Getter &&
            propMethodsEqual setter other.Setter

    override this.Equals obj =
        match obj with
        | :? Property as other -> this === other
        | _ -> false

module PropertyPatterns =
    let inline (|PropertyMethods|) (property: Property) = struct(property.Getter, property.Setter, property.Other)

[<Struct>]
type Property<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> (property: Property) =
    member _.Property = property
    member _.Getter = Convert.unsafeValueOption<_, MethodDefinition<'Kind>> property.Getter
    member _.Setter = Convert.unsafeValueOption<_, MethodDefinition<'Kind>> property.Setter

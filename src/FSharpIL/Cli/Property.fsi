namespace FSharpIL.Cli

open FSharpIL.Metadata

/// Represents a property, which is a grouping of methods that access or modify a value (I.8.11.3 and II.17).
[<Sealed>]
type Property =
    member Flags: FSharpIL.Metadata.Tables.PropertyFlags
    member Name: Identifier
    member Getter: DefinedMethod voption
    member Setter: DefinedMethod voption
    member Other: DefinedMethod list

    internal new:
        name: Identifier *
        getter: DefinedMethod voption *
        setter: DefinedMethod voption *
        other: DefinedMethod list -> Property

    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface System.IEquatable<Property>

[<AutoOpen>]
module PropertyPatterns =
    val inline (|PropertyMethods|) :
        property: Property -> struct(DefinedMethod voption * DefinedMethod voption * DefinedMethod list)

[<System.Runtime.CompilerServices.IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Property<'Kind when 'Kind :> MethodKinds.IKind and 'Kind : struct> =
    member Property: Property
    member Getter: MethodDefinition<'Kind> voption
    member Setter: MethodDefinition<'Kind> voption

    internal new: Property -> Property<'Kind>

    interface System.IEquatable<Property<'Kind>>

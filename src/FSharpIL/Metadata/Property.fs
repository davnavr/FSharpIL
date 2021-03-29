namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type PropertySignature internal
    (
        hasThis: bool,
        customMod: ImmutableArray<CustomModifier>,
        propertyType: EncodedType,
        parameters: ImmutableArray<ParamItem>
    ) =
    member _.HasThis = hasThis
    member _.CustomMod = customMod
    member _.Type = propertyType
    member _.Parameters = parameters

/// <summary>(0x17) Represents a row in the <c>Property</c> table (II.22.34).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type PropertyRow internal (flags: PropertyAttributes, name: Identifier, signature: Blob<PropertySignature>) =
    member _.Flags = flags
    member _.Name = name
    member _.Type = signature

    interface IEquatable<PropertyRow> with
        member _.Equals other = name = other.Name && signature = other.Type

[<IsReadOnly; Struct>]
type Property<'Tag, 'Signature> =
    { Flags: ValidFlags<'Tag, PropertyAttributes>
      /// <summary>Corresponds to the <c>Name</c> column of the <c>Property</c> table (II.22.34).</summary>
      PropertyName: Identifier
      Type: Blob<'Signature> }

    member internal this.Definition() = PropertyRow(this.Flags.Value, this.PropertyName, this.Type.ChangeTag())

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type InstancePropertySignature
    (
        modifiers: ImmutableArray<CustomModifier>,
        propertyType: EncodedType,
        parameters: ImmutableArray<ParamItem>
    ) =
    member _.CustomMod = modifiers
    member _.Type = propertyType
    member _.Parameters = parameters

    new (modifiers, propertyType, [<ParamArray>] parameters: ParamItem[]) =
        InstancePropertySignature(modifiers, propertyType, parameters.ToImmutableArray())

    new (propertyType, [<ParamArray>] parameters: ParamItem[]) =
        InstancePropertySignature(ImmutableArray.Empty, propertyType, parameters)

    member internal _.Signature() = PropertySignature(true, modifiers, propertyType, parameters)

type InstanceProperty = Property<InstanceMethodTag, InstancePropertySignature>

//type VirtualProperty = Property<>

[<IsReadOnly; Struct>]
type StaticPropertySignature internal (signature: InstancePropertySignature) =
    new (modifiers, propertyType, parameters: ImmutableArray<_>) =
        StaticPropertySignature(InstancePropertySignature(modifiers, propertyType, parameters))

    new (modifiers, propertyType, [<ParamArray>] parameters: ParamItem[]) =
        StaticPropertySignature(modifiers, propertyType, parameters.ToImmutableArray())

    new (propertyType, [<ParamArray>] parameters: ParamItem[]) =
        StaticPropertySignature(ImmutableArray.Empty, propertyType, parameters)

    member internal _.Signature() = PropertySignature(false, signature.CustomMod, signature.Type, signature.Parameters)

type StaticProperty = Property<StaticMethodTag, StaticPropertySignature>

/// <summary>
/// Error used when there is a duplicate row in the <c>Property</c> table (8).
/// </summary>
/// <category>Errors</category>
type DuplicatePropertyError (property: PropertyRow) =
    inherit ValidationError()
    member _.Property = property
    override _.ToString() =
        sprintf
            "Unable to add property \"%O\", a property with the same name and signature already exists"
            property

[<Sealed>]
type PropertySigBlobLookup internal (lookup: BlobLookup<PropertySignature>) =
    member _.Count = lookup.Count
    member private _.GetSignature(i: Blob<_>) = lookup.[i.ChangeTag()]
    member _.ItemRef i: inref<_> = &lookup.ItemRef i
    member _.Item with get i = lookup.[i]
    member this.Item with get (i: Blob<InstancePropertySignature>) = this.GetSignature i
    member this.Item with get (i: Blob<StaticPropertySignature>) = this.GetSignature i

[<Sealed>]
type PropertySigBlobLookupBuilder internal () =
    let lookup = BlobLookupBuilder<PropertySignature>()
    member _.Count = lookup.Count
    member private _.TryAdd<'Tag>(signature) = lookup.TryAdd signature |> ValueOption.map (fun i -> i.ChangeTag<'Tag>())
    member this.TryAdd(signature: InstancePropertySignature) = this.TryAdd<InstancePropertySignature>(signature.Signature())
    member this.TryAdd(signature: StaticPropertySignature) = this.TryAdd<StaticPropertySignature>(signature.Signature())
    member internal _.ToImmutable() = PropertySigBlobLookup(lookup.ToImmutable())
